// Function adapted from Plotly R Package 3.60,

// Decode base64 string to Uint8Array
function base64ToUint8Array(base64) {
  var binaryString = atob(base64);
  var bytes = new Uint8Array(binaryString.length);
  for (var i = 0; i < binaryString.length; i++) {
    bytes[i] = binaryString.charCodeAt(i);
  }
  return bytes;
}

// Decompress zlib/gzip data using native DecompressionStream API
// Note: R's memCompress(type="gzip") actually produces zlib format, not gzip
async function decompressData(compressedData, format) {
  if (typeof DecompressionStream === 'undefined') {
    throw new Error('DecompressionStream not supported in this browser');
  }

  // R's memCompress(type="gzip") produces zlib format, use 'deflate' to decompress
  // 'deflate' in DecompressionStream handles zlib format (with header)
  var compressionFormat = (format === 'zlib' || format === 'gzip') ? 'deflate' : format;
  var ds = new DecompressionStream(compressionFormat);
  var decompressedStream = new Blob([compressedData]).stream().pipeThrough(ds);

  // Read stream chunks directly instead of using Response (avoids "Failed to fetch" errors)
  var reader = decompressedStream.getReader();
  var chunks = [];
  var totalLength = 0;

  while (true) {
    var result = await reader.read();
    if (result.done) break;
    chunks.push(result.value);
    totalLength += result.value.length;
  }

  // Combine chunks into single Uint8Array
  var combined = new Uint8Array(totalLength);
  var offset = 0;
  for (var i = 0; i < chunks.length; i++) {
    combined.set(chunks[i], offset);
    offset += chunks[i].length;
  }

  return combined;
}

// Convert buffer to typed array based on dtype
// Handles byte alignment issues when buffer is a Uint8Array slice
function bufferToTypedArray(buffer, dtype) {
  var arrayBuffer;

  if (buffer instanceof Uint8Array && buffer.byteOffset === 0 &&
      buffer.byteLength === buffer.buffer.byteLength) {
    arrayBuffer = buffer.buffer;
  } else if (buffer instanceof Uint8Array) {
    // Create a new ArrayBuffer to ensure proper byte alignment
    // This handles cases where the Uint8Array may be a slice with non-zero offset
    arrayBuffer = buffer.buffer.slice(
      buffer.byteOffset,
      buffer.byteOffset + buffer.byteLength
    );
  } else {
    arrayBuffer = buffer;
  }

  if (dtype === 'uint16') {
    return new Uint16Array(arrayBuffer);
  } else if (dtype === 'float32') {
    return new Float32Array(arrayBuffer);
  } else {
    return new Float64Array(arrayBuffer);
  }
}

// Convert flat typed array to 2D matrix (column-major order from R)
function flatArrayToMatrix(floatArray, nrows, ncols) {
  var matrix = [];
  for (var i = 0; i < nrows; i++) {
    var row = [];
    for (var j = 0; j < ncols; j++) {
      row.push(floatArray[j * nrows + i]);
    }
    matrix.push(row);
  }
  return matrix;
}

// Row-major buffers need no copy: each matrix row is a view into the buffer.
// NA cells become NaN, which Plotly treats as missing.
function rowViewsFromFlatArray(floatArray, nrows, ncols, naPositions) {
  if (naPositions) {
    for (var k = 0; k < naPositions.length; k++) {
      floatArray[naPositions[k]] = NaN;
    }
  }
  var rows = new Array(nrows);
  for (var i = 0; i < nrows; i++) {
    rows[i] = floatArray.subarray(i * ncols, (i + 1) * ncols);
  }
  return rows;
}

// 16-bit quantized values back to floats; step is the quantization step
function dequantize(q, binaryData) {
  var lo = parseFloat(binaryData.q_min), hi = parseFloat(binaryData.q_max);
  var step = (hi - lo) / (Math.pow(2, binaryData.q_bits || 16) - 1);
  var out = new Float32Array(q.length);
  for (var i = 0; i < q.length; i++) {
    out[i] = lo + q[i] * step;
  }
  return { values: out, step: step };
}

// Two 12-bit values per 3 bytes (see encode_matrix_binary); count values out
function unpack12(bytes, count) {
  var out = new Uint16Array(count + (count % 2));
  for (var i = 0, j = 0; j < out.length; i += 3, j += 2) {
    out[j] = bytes[i] | ((bytes[i + 1] & 15) << 8);
    out[j + 1] = (bytes[i + 1] >> 4) | (bytes[i + 2] << 4);
  }
  return count % 2 ? out.subarray(0, count) : out;
}

function isRowMajor(binaryData) {
  return binaryData.layout === 'row-major';
}

// Index of (row, col) in the flat buffer behind trace._z_float32
function zFlatIndex(trace, rowIdx, colIdx) {
  var nrows = trace._z_dims[0];
  var ncols = trace._z_dims[1];
  return trace._z_rowmajor ? rowIdx * ncols + colIdx : colIdx * nrows + rowIdx;
}

// Restore NA values in matrix
function restoreNAValues(matrix, naPositions, nrows) {
  if (!naPositions || naPositions.length === 0) return;

  for (var k = 0; k < naPositions.length; k++) {
    var pos = naPositions[k];
    var col = Math.floor(pos / nrows);
    var row = pos % nrows;
    if (row < matrix.length && col < matrix[row].length) {
      matrix[row][col] = null;  // null is Plotly's representation of NA
    }
  }
}

function buildMatrix(floatArray, binaryData) {
  var nrows = binaryData.dims[0];
  var ncols = binaryData.dims[1];
  if (isRowMajor(binaryData)) {
    return rowViewsFromFlatArray(floatArray, nrows, ncols, binaryData.na_positions);
  }
  var matrix = flatArrayToMatrix(floatArray, nrows, ncols);
  restoreNAValues(matrix, binaryData.na_positions, nrows);
  return matrix;
}

// Decode binary-encoded matrix from trace (async for gzip support)
// Returns { matrix, floatArray, dims, naSet } so the caller can store the
// Float32Array on the trace for post-render z-array release.
async function decodeBinaryMatrixAsync(binaryData) {
  if (!binaryData || !binaryData.data_binary || !binaryData.dims) {
    return null;
  }

  var nrows = binaryData.dims[0];
  var ncols = binaryData.dims[1];
  var dtype = binaryData.dtype || 'float64';
  var encoding = binaryData.encoding || 'base64';

  // Decode base64
  var rawBytes = base64ToUint8Array(binaryData.data_binary);

  // Decompress if compressed (R's memCompress produces zlib format)
  var decodedBytes;
  if (encoding === 'base64-gzip') {
    decodedBytes = await decompressData(rawBytes, 'zlib');
  } else {
    decodedBytes = rawBytes;
  }

  // Convert to appropriate typed array
  var floatArray = dtype === 'uint12'
    ? unpack12(decodedBytes, nrows * ncols)
    : bufferToTypedArray(decodedBytes, dtype);
  var quantStep = 0;
  if (dtype === 'uint16' || dtype === 'uint12') {
    var dq = dequantize(floatArray, binaryData);
    floatArray = dq.values;
    quantStep = dq.step;
  }

  // Build NA position Set for O(1) lookup after z-array release
  var naSet = null;
  if (binaryData.na_positions && binaryData.na_positions.length > 0) {
    naSet = new Set(binaryData.na_positions);
  }

  var matrix = buildMatrix(floatArray, binaryData);

  return { matrix: matrix, floatArray: floatArray, dims: [nrows, ncols], naSet: naSet,
           rowMajor: isRowMajor(binaryData), quantStep: quantStep };
}

// Synchronous fallback for non-compressed data
// Returns { matrix, floatArray, dims, naSet } or null (gzip needs async path)
function decodeBinaryMatrixSync(binaryData) {
  if (!binaryData || !binaryData.data_binary || !binaryData.dims) {
    return null;
  }

  var encoding = binaryData.encoding || 'base64';

  // If gzip compressed, return null (will be handled async)
  if (encoding === 'base64-gzip') {
    return null;
  }

  var nrows = binaryData.dims[0];
  var ncols = binaryData.dims[1];
  var dtype = binaryData.dtype || 'float64';

  var rawBytes = base64ToUint8Array(binaryData.data_binary);
  var floatArray = dtype === 'uint12'
    ? unpack12(rawBytes, nrows * ncols)
    : bufferToTypedArray(rawBytes, dtype);
  var quantStep = 0;
  if (dtype === 'uint16' || dtype === 'uint12') {
    var dq = dequantize(floatArray, binaryData);
    floatArray = dq.values;
    quantStep = dq.step;
  }

  var naSet = null;
  if (binaryData.na_positions && binaryData.na_positions.length > 0) {
    naSet = new Set(binaryData.na_positions);
  }

  var matrix = buildMatrix(floatArray, binaryData);

  return { matrix: matrix, floatArray: floatArray, dims: [nrows, ncols], naSet: naSet,
           rowMajor: isRowMajor(binaryData), quantStep: quantStep };
}

// Process traces to decode any binary-encoded matrices (async)
async function decodeBinaryTracesAsync(data) {
  var promises = [];

  for (var i = 0; i < data.length; i++) {
    var trace = data[i];
    if (trace.z_binary) {
      // IIFE to capture trace reference in closure
      (function(t) {
        var promise = decodeBinaryMatrixAsync(t.z_binary).then(function(result) {
          if (result === null) return;
          t.z = result.matrix;
          // Store Float32Array backing for post-render z-array release
          t._z_float32 = result.floatArray;
          t._z_dims = result.dims;
          t._z_na = result.naSet;
          t._z_rowmajor = result.rowMajor;
          t._z_step = result.quantStep;
          delete t.z_binary;
        });
        promises.push(promise);
      })(trace);
    }
  }

  await Promise.all(promises);
  return data;
}

// Synchronous version for backwards compatibility (non-gzip only)
function decodeBinaryTraces(data) {
  for (var i = 0; i < data.length; i++) {
    var trace = data[i];
    if (trace.z_binary) {
      var result = decodeBinaryMatrixSync(trace.z_binary);
      if (result !== null) {
        trace.z = result.matrix;
        // Store Float32Array backing for post-render z-array release
        trace._z_float32 = result.floatArray;
        trace._z_dims = result.dims;
        trace._z_na = result.naSet;
        trace._z_rowmajor = result.rowMajor;
        trace._z_step = result.quantStep;
        delete trace.z_binary;
      }
      // If null (gzip), leave z_binary for async processing
    }
  }
  return data;
}

// Check if any traces need async decoding
function needsAsyncDecode(data) {
  for (var i = 0; i < data.length; i++) {
    if (data[i].z_binary && data[i].z_binary.encoding === 'base64-gzip') {
      return true;
    }
  }
  return false;
}

// Expand implicit coordinates (x_implicit/y_implicit) to full arrays
// When x_implicit=n is present, it means x should be [1, 2, ..., n]
function expandImplicitCoordinates(data) {
  for (var i = 0; i < data.length; i++) {
    var trace = data[i];

    if (trace.x_implicit !== undefined) {
      trace._x_seq = true;
      var n = trace.x_implicit;
      trace.x = [];
      for (var j = 1; j <= n; j++) {
        trace.x.push(j);
      }
      delete trace.x_implicit;
    }

    if (trace.y_implicit !== undefined) {
      trace._y_seq = true;
      var m = trace.y_implicit;
      trace.y = [];
      for (var k = 1; k <= m; k++) {
        trace.y.push(k);
      }
      delete trace.y_implicit;
    }
  }
  return data;
}

// Decode compact dendrogram binary data into one line trace per dendrogram.
// Plotly still draws one path per segment, but a trace avoids the extra shape
// DOM nodes and layout objects that one layout shape per segment costs.
async function decodeCompactDendrograms(compactDendros) {
  var traces = [];
  if (!compactDendros) return traces;

  for (var i = 0; i < compactDendros.length; i++) {
    var dendro = compactDendros[i];
    if (!dendro.coords_binary || !dendro.n_segments) continue;

    var rawBytes = base64ToUint8Array(dendro.coords_binary);
    if (dendro.encoding === 'base64-zlib') {
      rawBytes = await decompressData(rawBytes, 'zlib');
    }

    var floatArray = bufferToTypedArray(rawBytes, dendro.dtype || 'float32');

    // Each segment has 4 coordinates: x0, x1, y0, y1
    var n = dendro.n_segments;
    var xs = new Array(n * 3);
    var ys = new Array(n * 3);
    for (var j = 0; j < n; j++) {
      var offset = j * 4;
      xs[j * 3] = floatArray[offset];
      xs[j * 3 + 1] = floatArray[offset + 1];
      xs[j * 3 + 2] = null;
      ys[j * 3] = floatArray[offset + 2];
      ys[j * 3 + 1] = floatArray[offset + 3];
      ys[j * 3 + 2] = null;
    }
    traces.push({
      type: 'scatter',
      mode: 'lines',
      x: xs,
      y: ys,
      xaxis: dendro.xref,
      yaxis: dendro.yref,
      line: { color: dendro.color || 'gray', width: 1 },
      hoverinfo: 'skip',
      showlegend: false
    });
  }

  return traces;
}

// Sanitize text for safe HTML display (prevent XSS)
function sanitizeText(text) {
  if (text === null || text === undefined) return '';
  var str = String(text);
  var div = document.createElement('div');
  div.textContent = str;
  return div.innerHTML;
}

// Generate tooltip text on-demand for lazy tooltips
// Values are read from trace.z matrix (not stored in lazy_tooltip to save space)
function generateLazyTooltip(trace, rowIdx, colIdx) {
  var lt = trace.lazy_tooltip;
  if (!lt) return null;

  // Bounds checking for indices
  if (rowIdx === undefined || colIdx === undefined ||
      rowIdx < 0 || colIdx < 0) {
    return null;
  }

  var cell = lodCell(trace, rowIdx, colIdx);
  var shownRow = rowIdx, shownCol = colIdx;
  rowIdx = cell.row;
  colIdx = cell.col;

  var parts = [];

  // Helper to add run info after sample label
  // Check both lazy_tooltip.run_mapping and trace.run_mapping for compatibility
  function addRunInfo(sampleName) {
    var runMapping = lt.run_mapping || trace.run_mapping;
    if (sampleName && runMapping && runMapping[sampleName]) {
      parts.push('Run: ' + sanitizeText(runMapping[sampleName]));
    }
  }

  // Check bounds for row labels
  if (lt.show_row && lt.row_labels &&
      rowIdx < lt.row_labels.length &&
      lt.row_labels[rowIdx] !== undefined) {
    var rowLabel = sanitizeText(lt.row_labels[rowIdx]);
    parts.push(sanitizeText(lt.prepend_row) + rowLabel);
    // Add run info right after sample label
    if (lt.prepend_row && lt.prepend_row.indexOf('Sample') !== -1) {
      addRunInfo(lt.row_labels[rowIdx]);
    }
  }

  // Check bounds for col labels
  if (lt.show_col && lt.col_labels &&
      colIdx < lt.col_labels.length &&
      lt.col_labels[colIdx] !== undefined) {
    var colLabel = sanitizeText(lt.col_labels[colIdx]);
    parts.push(sanitizeText(lt.prepend_col) + colLabel);
    // Add run info right after sample label
    if (lt.prepend_col && lt.prepend_col.indexOf('Sample') !== -1) {
      addRunInfo(lt.col_labels[colIdx]);
    }
  }

  // Read values — prefer the lightweight Float32Array backing if the 2D z-array
  // was released after Plotly consumed it (Optimization 2).
  if (lt.show_value) {
    var value;
    if (cell.n > 1 || trace._lazy) {
      // Binned overview cell: show the mean the heatmap is drawn with; lazy
      // traces have no full buffer, only the displayed (exact when zoomed) cells
      value = trace.z && trace.z[shownRow] ? trace.z[shownRow][shownCol] : undefined;
    } else if (trace._z_float32 && trace._z_dims) {
      // Column-major layout from R: flatIdx = col * nrows + row
      var nrows = trace._z_dims[0];
      var ncols = trace._z_dims[1];
      if (rowIdx >= 0 && rowIdx < nrows && colIdx >= 0 && colIdx < ncols) {
        var flatIdx = zFlatIndex(trace, rowIdx, colIdx);
        if (trace._z_na && trace._z_na.has(flatIdx)) {
          value = null;
        } else {
          value = trace._z_float32[flatIdx];
        }
      }
    } else if (trace.z &&
               rowIdx < trace.z.length &&
               trace.z[rowIdx] &&
               colIdx < trace.z[rowIdx].length) {
      // Fallback for non-binary traces
      value = trace.z[rowIdx][colIdx];
    }
    if (value !== null && value !== undefined && value === value) {
      // Format to 3 significant figures for display
      var formatted = value;
      if (typeof value === 'number') {
        // Quantized values are good to about one quantization step: do not
        // print digits finer than that
        formatted = trace._z_step >= 1e-8
          ? value.toFixed(Math.min(8, Math.max(0, Math.floor(-Math.log10(trace._z_step)))))
          : value.toPrecision(4);
      }
      parts.push(sanitizeText(lt.prepend_value) + formatted +
                 (cell.n > 1 ? ' (mean of ' + cell.n + ' cells, zoom in for exact)' : ''));
    }
  }

  // Return null if no valid parts (fallback handled by caller)
  if (parts.length === 0) return null;

  return parts.join('<br>');
}

// Create and show a custom tooltip
function showLazyTooltip(x, y, text) {
  // Remove any existing lazy tooltip
  hideLazyTooltip();

  var tooltip = document.createElement('div');
  tooltip.className = 'lazy-hover-tooltip';
  tooltip.innerHTML = text;
  tooltip.style.position = 'fixed';
  tooltip.style.left = (x + 15) + 'px';
  tooltip.style.top = (y + 15) + 'px';
  tooltip.style.backgroundColor = 'rgba(0, 0, 0, 0.85)';
  tooltip.style.color = 'white';
  tooltip.style.padding = '8px 12px';
  tooltip.style.borderRadius = '4px';
  tooltip.style.fontSize = '12px';
  tooltip.style.fontFamily = 'Arial, sans-serif';
  tooltip.style.pointerEvents = 'none';
  tooltip.style.zIndex = '10001';
  tooltip.style.whiteSpace = 'nowrap';
  tooltip.style.boxShadow = '0 2px 8px rgba(0,0,0,0.3)';

  document.body.appendChild(tooltip);

  // Adjust position if tooltip goes off screen
  var rect = tooltip.getBoundingClientRect();
  if (rect.right > window.innerWidth) {
    tooltip.style.left = (x - rect.width - 10) + 'px';
  }
  if (rect.bottom > window.innerHeight) {
    tooltip.style.top = (y - rect.height - 10) + 'px';
  }
}

function hideLazyTooltip() {
  var existing = document.querySelector('.lazy-hover-tooltip');
  if (existing) {
    existing.remove();
  }
}

// Set up lazy tooltip hover handlers for a graph
function setupLazyTooltipHandlers(graphDiv, data) {
  // Check if any trace uses lazy tooltips
  var hasLazyTooltips = data.some(function(trace) {
    return trace.lazy_tooltip !== undefined;
  });

  if (!hasLazyTooltips) return;

  // Clean up only our lazy tooltip handlers (not all plotly_hover/unhover listeners,
  // which would destroy Shiny event handlers registered by setupEventHandlers)
  if (graphDiv._lazyTooltipHandlers) {
    if (graphDiv._lazyTooltipHandlers.mousemove) {
      graphDiv.removeEventListener('mousemove', graphDiv._lazyTooltipHandlers.mousemove);
    }
    try {
      if (graphDiv._lazyTooltipHandlers.hover) {
        graphDiv.removeListener('plotly_hover', graphDiv._lazyTooltipHandlers.hover);
      }
      if (graphDiv._lazyTooltipHandlers.unhover) {
        graphDiv.removeListener('plotly_unhover', graphDiv._lazyTooltipHandlers.unhover);
      }
    } catch (e) {
      // Ignore if listeners don't exist
    }
  }

  // Track mouse position for tooltip placement
  var mouseX = 0, mouseY = 0;
  var mousemoveHandler = function(e) {
    mouseX = e.clientX;
    mouseY = e.clientY;
  };
  graphDiv.addEventListener('mousemove', mousemoveHandler);

  // Handle hover events
  var hoverHandler = function(eventData) {
    if (!eventData || !eventData.points || eventData.points.length === 0) return;

    var pt = eventData.points[0];
    var trace = data[pt.curveNumber];

    if (trace && trace.lazy_tooltip) {
      var rowIdx, colIdx;

      // pointNumber can be [row, col] for heatmaps or just a number
      if (Array.isArray(pt.pointNumber)) {
        rowIdx = pt.pointNumber[0];
        colIdx = pt.pointNumber[1];
      } else if (pt.pointIndex !== undefined) {
        if (Array.isArray(pt.pointIndex)) {
          rowIdx = pt.pointIndex[0];
          colIdx = pt.pointIndex[1];
        }
      }

      if (rowIdx !== undefined && colIdx !== undefined) {
        var tooltipText = generateLazyTooltip(trace, rowIdx, colIdx);
        if (tooltipText) {
          showLazyTooltip(mouseX, mouseY, tooltipText);
        }
      }
    }
  };

  var unhoverHandler = function() {
    hideLazyTooltip();
  };

  graphDiv.on('plotly_hover', hoverHandler);
  graphDiv.on('plotly_unhover', unhoverHandler);

  // Store handlers for cleanup
  graphDiv._lazyTooltipHandlers = {
    mousemove: mousemoveHandler,
    hover: hoverHandler,
    unhover: unhoverHandler
  };
}

// add_row_labels / add_col_labels emit one layout annotation (one SVG text node
// plus layout work) per label. At tens of thousands of samples that exhausts
// renderer memory, so label groups larger than this are thinned to what fits the
// visible range and re-thinned on zoom.
var MAX_FULL_LABELS = 400;
var LABEL_PX = 14;

function axisKeyFromRef(ref) {
  return ref.charAt(0) + 'axis' + ref.slice(1);
}

// The coordinate that varies within a label group is the one along its axis
function labelAlongKey(group) {
  var a = group[0], b = group[1];
  return a.x !== b.x ? 'x' : 'y';
}

function thinLabelGroup(group, along, lo, hi, maxLabels) {
  var inRange = group.filter(function(a) { return a[along] >= lo && a[along] <= hi; });
  var stride = Math.max(1, Math.ceil(inRange.length / maxLabels));
  if (stride === 1) return inRange;
  return inRange.filter(function(a, k) { return k % stride === 0; });
}

function visibleRange(layoutOrFull, axisKey) {
  var ax = layoutOrFull && layoutOrFull[axisKey];
  var r = ax && ax.range;
  return r ? [Math.min(r[0], r[1]), Math.max(r[0], r[1])] : [-Infinity, Infinity];
}

// Called before the first render: replaces oversized label groups in the layout
function thinLayoutLabels(graphDiv, layout) {
  graphDiv._fullLabels = null;
  var anns = layout.annotations;
  if (!Array.isArray(anns) || anns.length <= MAX_FULL_LABELS) return;

  var groups = {};
  anns.forEach(function(a) {
    if (a.showarrow !== false || !a.xref || !a.yref) return;
    var key = a.xref + '|' + a.yref + '|' + (a.textangle || 0);
    (groups[key] = groups[key] || []).push(a);
  });

  var big = {};
  Object.keys(groups).forEach(function(key) {
    if (groups[key].length > MAX_FULL_LABELS) {
      var g = groups[key];
      var along = labelAlongKey(g);
      var ref = g[0][along === 'x' ? 'xref' : 'yref'];
      var ax = layout[axisKeyFromRef(ref)];
      big[key] = { all: g, along: along, ref: ref, initialRange: ax && ax.range && ax.range.slice() };
    }
  });
  if (!Object.keys(big).length) return;

  var others = anns.filter(function(a) {
    return !big[a.xref + '|' + a.yref + '|' + (a.textangle || 0)];
  });
  graphDiv._fullLabels = { big: big, others: others };
  layout.annotations = others.concat(rethinned(graphDiv, layout, null));
}

function rethinned(graphDiv, layoutOrFull, d) {
  var out = [];
  var big = graphDiv._fullLabels.big;
  Object.keys(big).forEach(function(key) {
    var b = big[key];
    var axisKey = axisKeyFromRef(b.ref);
    var range = visibleRange(layoutOrFull, axisKey);
    if (d) {
      var lo = d[axisKey + '.range[0]'], hi = d[axisKey + '.range[1]'];
      if (d[axisKey + '.range'] !== undefined) { lo = d[axisKey + '.range'][0]; hi = d[axisKey + '.range'][1]; }
      if (lo !== undefined && hi !== undefined) range = [Math.min(lo, hi), Math.max(lo, hi)];
    }
    var fl = graphDiv._fullLayout && graphDiv._fullLayout[axisKey];
    var px = fl && fl._length ? fl._length : 800;
    out = out.concat(thinLabelGroup(b.all, b.along, range[0], range[1],
                                    d ? Math.max(10, Math.floor(px / LABEL_PX)) : MAX_FULL_LABELS / 2));
  });
  return out;
}

// Layout update that re-thins labels after an axis range change, or null.
// Plotly's autorange also looks at annotations, so with thinned labels it would
// fit only the visible ones; reset to the layout's original range instead.
function labelsUpdateFor(graphDiv, d) {
  var full = graphDiv._fullLabels;
  if (!full || !d) return null;
  var update = {};
  var d2 = Object.assign({}, d);
  var touched = false;
  Object.keys(full.big).forEach(function(key) {
    var b = full.big[key];
    var axisKey = axisKeyFromRef(b.ref);
    if (d[axisKey + '.autorange'] && b.initialRange) {
      d2[axisKey + '.range'] = b.initialRange;
      update[axisKey + '.range'] = b.initialRange.slice();
      update[axisKey + '.autorange'] = false;
      touched = true;
    } else if (d[axisKey + '.range[0]'] !== undefined || d[axisKey + '.range'] !== undefined) {
      touched = true;
    }
  });
  if (!touched) return null;
  update.annotations = full.others.concat(rethinned(graphDiv, graphDiv._fullLayout, d2));
  return update;
}

// compact_label_annotations() ships big label groups as one template plus the
// label text; expand them into annotations before thinning
function expandLabelGroups(x) {
  if (!x.label_groups || !x.label_groups.length) return;
  var anns = x.layout.annotations || [];
  x.label_groups.forEach(function(g) {
    for (var k = 0; k < g.text.length; k++) {
      var a = Object.assign({}, g.template);
      a[g.along] = g.pos ? g.pos[k] : k + 1;
      a.text = g.text[k];
      anns.push(a);
    }
  });
  x.layout.annotations = anns;
  delete x.label_groups;
}

// compact_hover_text() replaces an annotation bar's per-cell text with a
// prefix, a reference to the main heatmap's labels, and a dictionary
function expandHoverText(data) {
  var main = null;
  data.forEach(function(t) { if (!main && t.lazy_tooltip) main = t.lazy_tooltip; });
  data.forEach(function(t) {
    var h = t.hover_compact;
    if (!h) return;
    var labels = h.orient === 'col' ? main && main.col_labels : main && main.row_labels;
    if (!labels || labels.length !== h.idx.length) {
      delete t.hover_compact;
      return;
    }
    var cells = new Array(labels.length);
    for (var i = 0; i < labels.length; i++) {
      cells[i] = h.prefix + labels[i] + '<br>' + h.tails[h.idx[i]];
    }
    t.text = h.orient === 'col' ? [cells] : cells.map(function(c) { return [c]; });
    delete t.hover_compact;
  });
}

// ---- Level of detail --------------------------------------------------------
// Plotly redraws every cell of a heatmap trace on each relayout, which costs
// seconds at millions of cells. Large row-major heatmaps are therefore drawn
// as a binned overview when zoomed out and as the visible full-resolution
// window (zero-copy row views) when zoomed in, so a redraw never exceeds
// LOD_BUDGET_CELLS. The full Float32Array stays on the trace for tooltips.
var LOD_MIN_CELLS = 500000;
var LOD_BUDGET_CELLS = 400000;

function lodEligible(trace) {
  if (trace._lazy) return !!(trace._x_seq && trace._y_seq);
  return !!(trace._z_float32 && trace._z_rowmajor && trace._z_dims &&
            trace._x_seq && trace._y_seq &&
            trace._z_dims[0] * trace._z_dims[1] >= LOD_MIN_CELLS);
}

function lodAxisRange(axObj) {
  var r = axObj && axObj.range;
  return r ? [Math.min(r[0], r[1]), Math.max(r[0], r[1])] : null;
}

// Window and bin factors for the visible ranges (1-based axis coordinates)
function lodDesired(trace, xr, yr) {
  var nr = trace._z_dims[0], nc = trace._z_dims[1];
  function span(range, n) {
    var lo = range ? Math.max(0, Math.floor(range[0] - 0.5)) : 0;
    var hi = range ? Math.min(n, Math.ceil(range[1] - 0.5)) : n;
    var margin = Math.ceil((hi - lo) * 0.25);
    return { lo: lo, hi: hi, start: Math.max(0, lo - margin), end: Math.min(n, hi + margin) };
  }
  var cs = span(xr, nc), rs = span(yr, nr);
  var fc = 1, fr = 1;
  while (Math.ceil((rs.end - rs.start) / fr) * Math.ceil((cs.end - cs.start) / fc) > LOD_BUDGET_CELLS) {
    if ((cs.end - cs.start) / fc >= (rs.end - rs.start) / fr) fc *= 2; else fr *= 2;
  }
  // Align the window to bin boundaries so panning does not shift bins
  var c0 = Math.floor(cs.start / fc) * fc, r0 = Math.floor(rs.start / fr) * fr;
  return { r0: r0, r1: rs.end, c0: c0, c1: cs.end, fr: fr, fc: fc,
           visR0: rs.lo, visR1: rs.hi, visC0: cs.lo, visC1: cs.hi };
}

function lodBuild(trace, d) {
  var ncols = trace._z_dims[1], buf = trace._z_float32;
  var nr = Math.ceil((d.r1 - d.r0) / d.fr), nc = Math.ceil((d.c1 - d.c0) / d.fc);
  var rows = new Array(nr);
  if (d.fr === 1 && d.fc === 1) {
    for (var i = 0; i < nr; i++) {
      rows[i] = buf.subarray((d.r0 + i) * ncols + d.c0, (d.r0 + i) * ncols + d.c1);
    }
    return rows;
  }
  var store = new Float32Array(nr * nc);
  for (var i2 = 0; i2 < nr; i2++) {
    var out = store.subarray(i2 * nc, (i2 + 1) * nc);
    var rs = d.r0 + i2 * d.fr, re = Math.min(d.r1, rs + d.fr);
    for (var j = 0; j < nc; j++) {
      var cs = d.c0 + j * d.fc, ce = Math.min(d.c1, cs + d.fc), sum = 0, cnt = 0;
      for (var r = rs; r < re; r++) {
        var base = r * ncols;
        for (var c = cs; c < ce; c++) {
          var v = buf[base + c];
          if (v === v) { sum += v; cnt++; }
        }
      }
      out[j] = cnt ? sum / cnt : NaN;
    }
    rows[i2] = out;
  }
  return rows;
}

function lodTraceProps(d) {
  return { x0: d.c0 + (d.fc + 1) / 2, dx: d.fc, y0: d.r0 + (d.fr + 1) / 2, dy: d.fr };
}

// Before the first render: swap eligible traces to their overview
function lodInit(data, layout) {
  data.forEach(function(trace) {
    trace._lod = null;
    if (!lodEligible(trace)) return;
    if (trace._lazy) {
      // The inline overview is the whole matrix at the factors R chose
      var ov = trace._lazy.ov, nr = trace._z_dims[0], nc = trace._z_dims[1];
      var d0 = { r0: 0, r1: nr, c0: 0, c1: nc, fr: ov.fr, fc: ov.fc,
                 visR0: 0, visR1: nr, visC0: 0, visC1: nc };
      trace.z = ov.rows;
      delete trace.x;
      delete trace.y;
      Object.assign(trace, lodTraceProps(d0));
      trace._lod = d0;
      return;
    }
    var xr = lodAxisRange(layout[axisKeyFromRef(trace.xaxis || 'x')]);
    var yr = lodAxisRange(layout[axisKeyFromRef(trace.yaxis || 'y')]);
    var d = lodDesired(trace, xr, yr);
    trace.z = lodBuild(trace, d);
    delete trace.x;
    delete trace.y;
    Object.assign(trace, lodTraceProps(d));
    trace._lod = d;
  });
}

// Traces sent with an inline overview only: the full matrix stays on the
// server and windows are fetched as raw float32 bytes (see lazy_window_trace)
function lazyInit(data) {
  data.forEach(function(trace) {
    var lw = trace.lazy_window;
    if (!lw) return;
    trace._z_dims = lw.dims;
    trace._lazy = { url: lw.url, ov: { rows: trace.z, fr: lw.fr, fc: lw.fc } };
    // Windows are exact and the overview is labelled as a mean, so the
    // quantization step must not shorten tooltip digits
    trace._z_step = 0;
    delete trace._z_float32;
    delete trace._z_na;
    delete trace.lazy_window;
  });
}

// Rows for window d: the cached overview, a local slice, or a server fetch
// (a Promise). Cells come back as float32, NaN where there is no value.
function lodWindow(trace, d) {
  var lz = trace._lazy;
  if (!lz) return lodBuild(trace, d);
  var nr = trace._z_dims[0], nc = trace._z_dims[1];
  if (d.r0 === 0 && d.c0 === 0 && d.r1 === nr && d.c1 === nc &&
      d.fr === lz.ov.fr && d.fc === lz.ov.fc) {
    return lz.ov.rows;
  }
  var nrows = Math.ceil((d.r1 - d.r0) / d.fr), ncols = Math.ceil((d.c1 - d.c0) / d.fc);
  var url = lz.url + '&r0=' + d.r0 + '&r1=' + d.r1 + '&c0=' + d.c0 + '&c1=' + d.c1 +
            '&fr=' + d.fr + '&fc=' + d.fc;
  return fetch(url, { cache: 'no-store' }).then(function(resp) {
    if (!resp.ok) throw new Error('heatmap window request failed: ' + resp.status);
    return resp.arrayBuffer();
  }).then(function(buf) {
    var f = new Float32Array(buf);
    if (f.length !== nrows * ncols) throw new Error('heatmap window has the wrong size');
    var rows = new Array(nrows);
    for (var i = 0; i < nrows; i++) rows[i] = f.subarray(i * ncols, (i + 1) * ncols);
    return rows;
  });
}

// Trace updates for windows that no longer fit the visible ranges.
// `rangeOverrides` maps an axis key to a range about to be applied.
function lodChangesFor(graphDiv, rangeOverrides) {
  var changes = [];
  var data = graphDiv.data;
  if (!data || !graphDiv._fullLayout) return changes;
  data.forEach(function(trace, idx) {
    if (!trace._lod) return;
    var xKey = axisKeyFromRef(trace.xaxis || 'x'), yKey = axisKeyFromRef(trace.yaxis || 'y');
    var xr = rangeOverrides[xKey] || lodAxisRange(graphDiv._fullLayout[xKey]);
    var yr = rangeOverrides[yKey] || lodAxisRange(graphDiv._fullLayout[yKey]);
    var d = lodDesired(trace, xr && [Math.min(xr[0], xr[1]), Math.max(xr[0], xr[1])],
                       yr && [Math.min(yr[0], yr[1]), Math.max(yr[0], yr[1])]);
    var st = trace._lod;
    var covers = st.r0 <= d.visR0 && st.r1 >= d.visR1 && st.c0 <= d.visC0 && st.c1 >= d.visC1;
    if (covers && st.fr <= d.fr && st.fc <= d.fc) return;
    changes.push({ idx: idx, d: d });
  });
  return changes;
}

// After a relayout settles: re-thin labels and refine or coarsen heatmap
// windows in ONE Plotly.update, so the plot is redrawn once, not once per fix.
function applyAdaptiveUpdates(graphDiv, d) {
  // Any pass supersedes an in-flight window fetch, including one that finds
  // nothing to do (a zoom back out must not let the zoomed-in reply land)
  var seq = graphDiv._adaptSeq = (graphDiv._adaptSeq || 0) + 1;
  var layoutUpdate = labelsUpdateFor(graphDiv, d);
  var overrides = {};
  if (layoutUpdate) {
    Object.keys(layoutUpdate).forEach(function(k) {
      var m = /^([xy]axis\d*)\.range$/.exec(k);
      if (m) overrides[m[1]] = layoutUpdate[k];
    });
  }
  var changes = lodChangesFor(graphDiv, overrides);
  if (!layoutUpdate && !changes.length) return;

  // Windows may need a server round trip; a newer pass supersedes this one
  var windows = changes.map(function(c) { return lodWindow(graphDiv.data[c.idx], c.d); });
  var apply = function(rowsList) {
    if (seq !== graphDiv._adaptSeq) return;
    var dataUpdate = { z: [], x0: [], dx: [], y0: [], dy: [] };
    var indices = [];
    if (rowsList) {
      changes.forEach(function(c, k) {
        var trace = graphDiv.data[c.idx], props = lodTraceProps(c.d);
        trace._lod = c.d;
        dataUpdate.z.push(rowsList[k]);
        dataUpdate.x0.push(props.x0);
        dataUpdate.dx.push(props.dx);
        dataUpdate.y0.push(props.y0);
        dataUpdate.dy.push(props.dy);
        indices.push(c.idx);
      });
    }
    if (!layoutUpdate && !indices.length) return;
    graphDiv._selfRelayout = true;
    var done = function() { graphDiv._selfRelayout = false; };
    Plotly.update(graphDiv, indices.length ? dataUpdate : {}, layoutUpdate || {},
                  indices.length ? indices : undefined).then(done, done);
  };
  Promise.all(windows).then(apply, function(err) {
    console.warn('iheatmapr: keeping the current heatmap detail.', err);
    apply(null);  // labels still follow the view
  });
}

function scheduleAdaptiveUpdate(graphDiv, d) {
  graphDiv._pendingRelayout = Object.assign(graphDiv._pendingRelayout || {}, d);
  if (graphDiv._adaptiveTimeout) clearTimeout(graphDiv._adaptiveTimeout);
  graphDiv._adaptiveTimeout = setTimeout(function() {
    var pending = graphDiv._pendingRelayout;
    graphDiv._adaptiveTimeout = null;
    graphDiv._pendingRelayout = null;
    applyAdaptiveUpdates(graphDiv, pending);
  }, 50);
}

// Map a displayed cell to the full matrix; binned cells report their first
// row/column and the number of cells they average
function lodCell(trace, rowIdx, colIdx) {
  var st = trace._lod;
  if (!st) return { row: rowIdx, col: colIdx, n: 1 };
  return { row: st.r0 + rowIdx * st.fr, col: st.c0 + colIdx * st.fc, n: st.fr * st.fc };
}

HTMLWidgets.widget({
  name: "iheatmapr",
  type: "output",

  initialize: function(el, width, height) {
    return {};
  },

  resize: function(el, width, height, instance) {
    if (instance.autosize) {
      Plotly.relayout(el.id, {width: width, height: height});
    }
  },

  renderValue: function(el, x, instance) {

    var shinyMode;
    if (typeof(window) !== "undefined") {
      // make sure plots don't get created outside the network
      window.PLOTLYENV = window.PLOTLYENV || {};
      window.PLOTLYENV.BASE_URL = x.base_url;
      shinyMode = !!window.Shiny;
    }

    var graphDiv = document.getElementById(el.id);

    // Hide during rendering to prevent flash of stale content.
    // Uses opacity instead of visibility so Plotly's SVG text measurement
    // (getBBox, getComputedTextLength) still works — visibility:hidden is
    // inherited by SVG children and can cause legend labels to be dropped.
    // The !important ensures withSpinner's inline styles cannot override it.
    // Removed in completeRender's .then() after Plotly finishes painting.
    if (!document.getElementById('iheatmapr-rendering-css')) {
      var s = document.createElement('style');
      s.id = 'iheatmapr-rendering-css';
      s.textContent = '.iheatmapr-rendering{opacity:0!important}';
      document.head.appendChild(s);
    }
    graphDiv.classList.add('iheatmapr-rendering');

    // Merge custom iheatmapr data back into traces
    // This data was separated to avoid Plotly validation stripping it
    if (x.iheatmapr_custom) {
      for (var traceIdx in x.iheatmapr_custom) {
        var idx = parseInt(traceIdx) - 1;  // R is 1-indexed
        if (x.data[idx]) {
          var customData = x.iheatmapr_custom[traceIdx];
          for (var key in customData) {
            x.data[idx][key] = customData[key];
          }
        }
      }
      delete x.iheatmapr_custom;
    }

    // Expand implicit coordinates (x_implicit/y_implicit -> full arrays)
    expandImplicitCoordinates(x.data);
    expandHoverText(x.data);
    expandLabelGroups(x);
    thinLayoutLabels(graphDiv, x.layout);

    // Store current data and source on instance so event handlers always
    // reference latest values (not stale closures from first renderValue call)
    instance.currentData = x.data;
    instance.source = x.source;

    // Helper to create event data sender for Shiny
    var sendEventData = function(eventType) {
      return function(eventData) {
        if (eventData === undefined || !eventData.hasOwnProperty("points")) {
          return null;
        }
        var d = eventData.points.map(function(pt) {
          var obj = {
                curveNumber: pt.curveNumber,
                pointNumber: pt.pointNumber,
                x: pt.x,
                y: pt.y
          };
          // Use instance.currentData (not closed-over x.data) so that
          // subsequent Plotly.react() re-renders update the data correctly
          var tr = instance.currentData[pt.curveNumber];
          // add on additional trace info, if it exists
          var attachKey = function(keyName) {
            if (tr.hasOwnProperty(keyName) && tr[keyName] !== null) {
              if (typeof pt.pointNumber === "number") {
                obj[keyName] = tr[keyName][pt.pointNumber];
              } else {
                obj[keyName] = tr[keyName][pt.pointNumber[0]][pt.pointNumber[1]];
              }
            }
          };
          // For z: prefer Float32Array backing if the 2D z-array was released
          if (tr._z_float32 && tr._z_dims && Array.isArray(pt.pointNumber)) {
            var cell = lodCell(tr, pt.pointNumber[0], pt.pointNumber[1]);
            var rowIdx = cell.row;
            var colIdx = cell.col;
            var zNrows = tr._z_dims[0];
            var zNcols = tr._z_dims[1];
            if (rowIdx >= 0 && rowIdx < zNrows && colIdx >= 0 && colIdx < zNcols) {
              var flatIdx = zFlatIndex(tr, rowIdx, colIdx);
              if (cell.n > 1) {
                obj.z = tr.z[pt.pointNumber[0]][pt.pointNumber[1]];
              } else if (tr._z_na && tr._z_na.has(flatIdx)) {
                obj.z = null;
              } else {
                obj.z = tr._z_float32[flatIdx];
              }
            }
          } else {
            attachKey("z");
          }
          attachKey("key");
          return obj;
        });
        Shiny.onInputChange(
          ".clientValue-" + eventType + "-" + instance.source,
          JSON.stringify(d)
        );
      };
    };

    // Debounce timeout to prevent infinite event loops when re-attaching handlers.
    graphDiv._colorbarHandlerTimeout = null;

    // Debounced function to re-attach colorbar hover handlers
    var scheduleColorbarHandlers = function() {
      if (graphDiv._colorbarHandlerTimeout) {
        clearTimeout(graphDiv._colorbarHandlerTimeout);
      }

      graphDiv._colorbarHandlerTimeout = setTimeout(function() {
        try {
          addColorbarHoverHandlers(graphDiv, instance.currentData);
        } catch (error) {
          console.error('Error attaching colorbar hover handlers:', error);
        } finally {
          graphDiv._colorbarHandlerTimeout = null;
        }
      }, 100);
    };

    // Function to set up event handlers after Plotly renders
    function setupEventHandlers() {
      // Re-attach colorbar hover handlers after plotly redraws
      graphDiv.on('plotly_relayout', function(d) {
        scheduleColorbarHandlers();
        // Our own annotation update: not a user action, keep it out of Shiny
        if (graphDiv._selfRelayout) return;
        scheduleAdaptiveUpdate(graphDiv, d);

        if (shinyMode) {
          Shiny.onInputChange(
            ".clientValue-" + "iheatmapr_relayout" + "-" + instance.source,
            JSON.stringify(d)
          );
        }
      });

      graphDiv.on('plotly_restyle', function(d) {
        scheduleColorbarHandlers();
      });

      graphDiv.on('plotly_redraw', function() {
        scheduleColorbarHandlers();
      });

      // send user input event data to shiny
      if (shinyMode) {
        graphDiv.on('plotly_hover', sendEventData('iheatmapr_hover'));
        graphDiv.on('plotly_click', sendEventData('iheatmapr_click'));
        graphDiv.on('plotly_selected', sendEventData('iheatmapr_selected'));
        graphDiv.on('plotly_unhover', function(eventData) {
          Shiny.onInputChange(".clientValue-iheatmapr_hover-" + instance.source, null);
        });
        graphDiv.on('plotly_doubleclick', function(eventData) {
          Shiny.onInputChange(".clientValue-iheatmapr_click-" + instance.source, null);
        });
        // 'plotly_deselect' is code for doubleclick when in select mode
        graphDiv.on('plotly_deselect', function(eventData) {
          Shiny.onInputChange(".clientValue-iheatmapr_selected-" + instance.source, null);
          Shiny.onInputChange(".clientValue-iheatmapr_click-" + instance.source, null);
        });
      }
    }

    // Function to complete rendering after all data is decoded
    function completeRender() {
      var isFirstRender = !instance.plotly;
      // A re-render replaces the traces: replies for the old ones are stale
      graphDiv._adaptSeq = (graphDiv._adaptSeq || 0) + 1;

      // The vendored bundle (2.x) still auto-converts a bare string
      // colorbar.title into {text: ...} internally, so this is a no-op
      // today. Doing it explicitly here is cheap insurance against a
      // future major-version bump: plotly.js 3.x removed that shim, which
      // silently blanks every colorbar title in this repo's fixtures.
      for (var ti = 0; ti < x.data.length; ti++) {
        var cb = x.data[ti].colorbar;
        if (cb && typeof cb.title === 'string') {
          cb.title = { text: cb.title };
        }
      }

      // Use Plotly.react for subsequent renders to avoid full DOM teardown/rebuild,
      // which eliminates the peak memory spike from old + new graph coexisting.
      lazyInit(x.data);
      lodInit(x.data, x.layout);
      var renderFn = isFirstRender ? Plotly.newPlot : Plotly.react;

      renderFn(graphDiv, x.data, x.layout, x.config).then(function() {
        instance.plotly = true;
        instance.autosize = x.layout.autosize;

        // Release the 2D z-array on traces that have Float32Array backing.
        // Plotly has already deep-copied z into _fullData/calcdata, so the
        // iheatmapr copy is no longer needed. This frees ~13-40 MB at 6500x250.
        for (var i = 0; i < x.data.length; i++) {
          // Row-major z rows are views of _z_float32, so releasing them frees
          // nothing. Any later relayout that carries annotations (label
          // re-thinning) rebuilds the trace from gd.data and would blank the
          // heatmap, so z is only released when no label group is thinned.
          if (x.data[i]._z_float32 && !x.data[i]._z_rowmajor && !graphDiv._fullLabels) {
            x.data[i].z = null;
          }
        }

        // After Plotly.react, DOM is partially rebuilt — reset colorbar handler guards
        // so they get re-attached to the new DOM elements
        if (!isFirstRender) {
          var cbGroups = graphDiv.querySelectorAll('[data-hover-handlers-attached]');
          for (var i = 0; i < cbGroups.length; i++) {
            cbGroups[i].removeAttribute('data-hover-handlers-attached');
          }
        }

        // Add colorbar hover handlers after rendering
        addColorbarHoverHandlers(graphDiv, instance.currentData);

        // Set up lazy tooltip handlers for on-demand tooltip generation
        // (has internal cleanup, safe to call every render)
        setupLazyTooltipHandlers(graphDiv, instance.currentData);

        // Set up persistent event handlers on first render, or re-register
        // if trace count changed (Plotly.react may internally purge+replot)
        var prevTraceCount = instance.traceCount || 0;
        instance.traceCount = x.data.length;
        if (isFirstRender || prevTraceCount !== instance.traceCount) {
          setupEventHandlers();
        }

        // The layout's initial range may already be zoomed in
        scheduleAdaptiveUpdate(graphDiv, {});

        // Reveal now that the new plot is painted
        graphDiv.classList.remove('iheatmapr-rendering');
      });
    }

    // Check if we need async decoding (compressed data or compact dendrograms)
    var hasCompactDendros = x.compact_dendrograms && x.compact_dendrograms.length > 0;

    if (needsAsyncDecode(x.data) || hasCompactDendros) {
      // Async path: decode compressed data and dendrograms then render
      (async function() {
        try {
          // Decode binary traces
          await decodeBinaryTracesAsync(x.data);

          // Decode compact dendrograms (now with zlib compression)
          if (hasCompactDendros) {
            // Push in place: instance.currentData aliases x.data
            Array.prototype.push.apply(
              x.data,
              await decodeCompactDendrograms(x.compact_dendrograms)
            );
            delete x.compact_dendrograms;
          }

          completeRender();
        } catch (error) {
          console.error('Error decoding data:', error);
          // Clean up traces that failed to decode
          for (var j = 0; j < x.data.length; j++) {
            if (x.data[j].z_binary && !x.data[j].z) {
              x.data[j].z = [];
              delete x.data[j].z_binary;
            }
          }
          completeRender();
        }
      })();
    } else {
      // Sync path: decode uncompressed data and render immediately
      x.data = decodeBinaryTraces(x.data);
      completeRender();
    }

    // Helper function to create/show tooltip for colorbar labels
    function showColorbarTooltip(element, text) {
      // Remove any existing tooltip
      var existing = document.querySelector('.colorbar-hover-tooltip');
      if (existing) {
        existing.remove();
      }

      // Create tooltip div
      var tooltip = document.createElement('div');
      tooltip.className = 'colorbar-hover-tooltip';
      tooltip.textContent = text;
      tooltip.style.position = 'absolute';
      tooltip.style.backgroundColor = 'rgba(0, 0, 0, 0.8)';
      tooltip.style.color = 'white';
      tooltip.style.padding = '8px 12px';
      tooltip.style.borderRadius = '4px';
      tooltip.style.fontSize = '12px';
      tooltip.style.fontFamily = 'Arial, sans-serif';
      tooltip.style.pointerEvents = 'none';
      tooltip.style.zIndex = '10000';
      tooltip.style.whiteSpace = 'nowrap';
      tooltip.style.boxShadow = '0 2px 4px rgba(0,0,0,0.2)';

      document.body.appendChild(tooltip);

      // Position tooltip near the element
      var bbox = element.getBoundingClientRect();
      tooltip.style.left = (bbox.right + 10) + 'px';

      // Calculate vertical position with viewport boundary checks
      var top = bbox.top + bbox.height/2 - tooltip.offsetHeight/2;
      var maxTop = window.innerHeight - tooltip.offsetHeight - 10;
      if (top > maxTop) top = maxTop;
      if (top < 10) top = 10;
      tooltip.style.top = top + 'px';
    }

    function hideColorbarTooltip() {
      var existing = document.querySelector('.colorbar-hover-tooltip');
      if (existing) {
        existing.remove();
      }
    }

    // Main function to add hover to all colorbars
    function addColorbarHoverHandlers(graphDiv, data) {
      var maxAttempts = 10;
      var attemptDelay = 200;
      var attempt = 0;

      function tryAddHandlers() {
        attempt++;

        // Find all colorbar axis elements
        var cbaxisElements = graphDiv.querySelectorAll('.cbaxis');
        var cbfillsElements = graphDiv.querySelectorAll('.cbfills');

        // The colorbar groups are the parents of .cbaxis elements
        // Structure: .infolayer > .cb{id} > .cbaxis and .cbfills
        var colorbarGroups = [];

        for (var i = 0; i < cbaxisElements.length; i++) {
          var cbaxis = cbaxisElements[i];
          var colorbarGroup = cbaxis.parentElement;

          if (colorbarGroup) {
            // Verify this group also has cbfills as a child
            var hasCbfills = false;
            for (var j = 0; j < colorbarGroup.children.length; j++) {
              if (colorbarGroup.children[j].classList &&
                  colorbarGroup.children[j].classList.contains('cbfills')) {
                hasCbfills = true;
                break;
              }
            }

            if (hasCbfills) {
              colorbarGroups.push(colorbarGroup);
            }
          }
        }

        // If no colorbars found yet and we have more attempts, try again
        if (colorbarGroups.length === 0 && attempt < maxAttempts) {
          setTimeout(tryAddHandlers, attemptDelay);
          return;
        }

        var colorbars = colorbarGroups;

        // Collect all traces with colorbars
        var tracesWithColorbars = [];
        for (var i = 0; i < data.length; i++) {
          if (data[i].colorbar && data[i].showscale !== false) {
            tracesWithColorbars.push(data[i]);
          }
        }

        // Process each colorbar group
        colorbars.forEach(function(colorbarGroup, colorbarIndex) {
          // Skip if handlers already attached to this group
          if (colorbarGroup.getAttribute('data-hover-handlers-attached') === 'true') {
            return;
          }

          // Get the corresponding trace (colorbars appear in same order as traces)
          if (colorbarIndex >= tracesWithColorbars.length) {
            return;
          }

          var trace = tracesWithColorbars[colorbarIndex];

          if (!trace || !trace.colorbar || !trace.colorbar.ticktext_full) {
            return; // No full text available for this colorbar
          }

          var fullLabels = trace.colorbar.ticktext_full;

          // Validate fullLabels is an array with elements
          if (!Array.isArray(fullLabels) || fullLabels.length === 0) {
            return;
          }

          // Find the cbaxis group
          var cbaxisGroup = colorbarGroup.querySelector('.cbaxis');
          if (!cbaxisGroup) {
            return;
          }

          // Find tick labels - they are text elements within the cbaxis group
          var tickLabels = cbaxisGroup.querySelectorAll('text');
          if (!tickLabels || tickLabels.length === 0) {
            return;
          }

          // Find the cbfills group
          var cbfillsGroup = colorbarGroup.querySelector('.cbfills');
          if (!cbfillsGroup) {
            return;
          }

          // Get all color fill rectangles
          var colorFills = [];
          for (var m = 0; m < cbfillsGroup.children.length; m++) {
            colorFills.push(cbfillsGroup.children[m]);
          }

          // Add hover handlers to each tick label
          // Set pointer-events to ensure the text can receive mouse events
          tickLabels.forEach(function(textElement, tickIndex) {
            var fullText = fullLabels[tickIndex];

            if (fullText) {
              textElement.style.cursor = 'pointer';
              textElement.style.pointerEvents = 'all';

              textElement.addEventListener('mouseenter', function(e) {
                showColorbarTooltip(textElement, fullText);
              });
              textElement.addEventListener('mouseleave', function(e) {
                hideColorbarTooltip();
              });
            }
          });

          // Add hover to all the color rectangles
          // Map rectangles to labels based on vertical position using getBoundingClientRect
          var numLabels = fullLabels.length;

          // Get tick label screen positions
          var tickPositions = [];
          tickLabels.forEach(function(textElement, idx) {
            try {
              var rect = textElement.getBoundingClientRect();
              tickPositions.push({
                y: rect.top + rect.height / 2,
                index: idx
              });
            } catch (e) {
              // Skip if getBoundingClientRect fails
            }
          });

          if (tickPositions.length > 0) {
            colorFills.forEach(function(rect) {
              try {
                var rectBounds = rect.getBoundingClientRect();
                var rectCenterY = rectBounds.top + rectBounds.height / 2;

                // Find closest tick position
                var closestIndex = tickPositions[0].index;
                var minDistance = Math.abs(rectCenterY - tickPositions[0].y);

                for (var i = 1; i < tickPositions.length; i++) {
                  var distance = Math.abs(rectCenterY - tickPositions[i].y);
                  if (distance < minDistance) {
                    minDistance = distance;
                    closestIndex = tickPositions[i].index;
                  }
                }

                var fullText = fullLabels[closestIndex];

                if (fullText) {
                  rect.style.cursor = 'pointer';
                  rect.style.pointerEvents = 'all';
                  rect.addEventListener('mouseenter', function(e) {
                    showColorbarTooltip(rect, fullText);
                  });
                  rect.addEventListener('mouseleave', function(e) {
                    hideColorbarTooltip();
                  });
                }
              } catch (e) {
                // Skip rects that fail
              }
            });
          }

          // Mark this colorbar group as having handlers attached
          colorbarGroup.setAttribute('data-hover-handlers-attached', 'true');
        });
      }

      // Start trying to add handlers
      tryAddHandlers();
    }

  },

  // Cleanup method called when widget is destroyed
  destroy: function(el) {
    var graphDiv = document.getElementById(el.id);
    if (graphDiv) {
      // Clear any pending colorbar handler timeout
      if (graphDiv._colorbarHandlerTimeout) {
        clearTimeout(graphDiv._colorbarHandlerTimeout);
        graphDiv._colorbarHandlerTimeout = null;
      }

      // Clean up lazy tooltip handlers
      if (graphDiv._lazyTooltipHandlers) {
        if (graphDiv._lazyTooltipHandlers.mousemove) {
          graphDiv.removeEventListener('mousemove', graphDiv._lazyTooltipHandlers.mousemove);
        }
        graphDiv._lazyTooltipHandlers = null;
      }

      // Remove Plotly event listeners
      try {
        graphDiv.removeAllListeners('plotly_relayout');
        graphDiv.removeAllListeners('plotly_restyle');
        graphDiv.removeAllListeners('plotly_redraw');
        graphDiv.removeAllListeners('plotly_hover');
        graphDiv.removeAllListeners('plotly_unhover');
      } catch (e) {
        // Ignore errors if listeners don't exist
      }

      // Remove any existing tooltips
      var tooltip = document.querySelector('.colorbar-hover-tooltip');
      if (tooltip) {
        tooltip.remove();
      }

      var lazyTooltip = document.querySelector('.lazy-hover-tooltip');
      if (lazyTooltip) {
        lazyTooltip.remove();
      }

      // Purge Plotly graph state to free internal copies (_fullData, calcdata)
      try {
        Plotly.purge(graphDiv);
      } catch (e) {
        // Ignore errors if Plotly state doesn't exist
      }
    }
  }

});