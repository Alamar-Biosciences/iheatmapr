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

  if (buffer instanceof Uint8Array) {
    // Create a new ArrayBuffer to ensure proper byte alignment
    // This handles cases where the Uint8Array may be a slice with non-zero offset
    arrayBuffer = buffer.buffer.slice(
      buffer.byteOffset,
      buffer.byteOffset + buffer.byteLength
    );
  } else {
    arrayBuffer = buffer;
  }

  if (dtype === 'float32') {
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

// Decode binary-encoded matrix from trace (async for gzip support)
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
  var floatArray = bufferToTypedArray(decodedBytes, dtype);

  // Convert to 2D matrix
  var matrix = flatArrayToMatrix(floatArray, nrows, ncols);

  // Restore NA values
  restoreNAValues(matrix, binaryData.na_positions, nrows);

  return matrix;
}

// Synchronous fallback for non-compressed data
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
  var floatArray = bufferToTypedArray(rawBytes, dtype);
  var matrix = flatArrayToMatrix(floatArray, nrows, ncols);
  restoreNAValues(matrix, binaryData.na_positions, nrows);

  return matrix;
}

// Process traces to decode any binary-encoded matrices (async)
async function decodeBinaryTracesAsync(data) {
  var promises = [];

  for (var i = 0; i < data.length; i++) {
    var trace = data[i];
    if (trace.z_binary) {
      // IIFE to capture trace reference in closure
      (function(t) {
        var promise = decodeBinaryMatrixAsync(t.z_binary).then(function(matrix) {
          t.z = matrix;
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
      var matrix = decodeBinaryMatrixSync(trace.z_binary);
      if (matrix !== null) {
        trace.z = matrix;
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
      var n = trace.x_implicit;
      trace.x = [];
      for (var j = 1; j <= n; j++) {
        trace.x.push(j);
      }
      delete trace.x_implicit;
    }

    if (trace.y_implicit !== undefined) {
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

// Decode compact dendrogram binary data into Plotly shapes (async for zlib support)
async function decodeCompactDendrograms(compactDendros, existingShapes) {
  if (!compactDendros || compactDendros.length === 0) {
    return existingShapes || [];
  }

  var shapes = existingShapes ? existingShapes.slice() : [];

  for (var i = 0; i < compactDendros.length; i++) {
    var dendro = compactDendros[i];
    if (!dendro.coords_binary || !dendro.n_segments) continue;

    // Decode binary coordinates
    var rawBytes = base64ToUint8Array(dendro.coords_binary);

    // Decompress if zlib encoded
    if (dendro.encoding === 'base64-zlib') {
      rawBytes = await decompressData(rawBytes, 'zlib');
    }

    var dtype = dendro.dtype || 'float32';
    var floatArray = bufferToTypedArray(rawBytes, dtype);

    // Each segment has 4 coordinates: x0, x1, y0, y1
    var n = dendro.n_segments;
    for (var j = 0; j < n; j++) {
      var offset = j * 4;
      shapes.push({
        x0: floatArray[offset],
        x1: floatArray[offset + 1],
        y0: floatArray[offset + 2],
        y1: floatArray[offset + 3],
        type: 'line',
        xref: dendro.xref,
        yref: dendro.yref,
        line: { color: dendro.color || 'gray' }
      });
    }
  }

  return shapes;
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

  var parts = [];

  // Check bounds for row labels
  if (lt.show_row && lt.row_labels &&
      rowIdx < lt.row_labels.length &&
      lt.row_labels[rowIdx] !== undefined) {
    parts.push(sanitizeText(lt.prepend_row) + sanitizeText(lt.row_labels[rowIdx]));
  }

  // Check bounds for col labels
  if (lt.show_col && lt.col_labels &&
      colIdx < lt.col_labels.length &&
      lt.col_labels[colIdx] !== undefined) {
    parts.push(sanitizeText(lt.prepend_col) + sanitizeText(lt.col_labels[colIdx]));
  }

  // Read values from trace.z matrix (saves ~60% space vs storing in lazy_tooltip)
  if (lt.show_value && trace.z &&
      rowIdx < trace.z.length &&
      trace.z[rowIdx] &&
      colIdx < trace.z[rowIdx].length) {
    var value = trace.z[rowIdx][colIdx];
    if (value !== null && value !== undefined) {
      // Format to 3 significant figures for display
      var formatted = typeof value === 'number' ? value.toPrecision(4) : value;
      parts.push(sanitizeText(lt.prepend_value) + formatted);
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

  // Clean up any existing lazy tooltip handlers to prevent memory leaks
  if (graphDiv._lazyTooltipHandlers) {
    if (graphDiv._lazyTooltipHandlers.mousemove) {
      graphDiv.removeEventListener('mousemove', graphDiv._lazyTooltipHandlers.mousemove);
    }
    try {
      graphDiv.removeAllListeners('plotly_hover');
      graphDiv.removeAllListeners('plotly_unhover');
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

    // Expand implicit coordinates (x_implicit/y_implicit -> full arrays)
    expandImplicitCoordinates(x.data);

    // Function to complete rendering after all data is decoded
    function completeRender() {
      // if no plot exists yet, create one with a particular configuration
      if (!instance.plotly) {
        Plotly.plot(graphDiv, x.data, x.layout, x.config);
        instance.plotly = true;
        instance.autosize = x.layout.autosize;

        // Add colorbar hover handlers after rendering
        addColorbarHoverHandlers(graphDiv, x.data);

        // Set up lazy tooltip handlers for on-demand tooltip generation
        setupLazyTooltipHandlers(graphDiv, x.data);
      } else {
        Plotly.newPlot(graphDiv, x.data, x.layout);

        // Add colorbar hover handlers after re-rendering
        addColorbarHoverHandlers(graphDiv, x.data);

        // Set up lazy tooltip handlers for on-demand tooltip generation
        setupLazyTooltipHandlers(graphDiv, x.data);
      }
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
            x.layout.shapes = await decodeCompactDendrograms(
              x.compact_dendrograms,
              x.layout.shapes
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

    sendEventData = function(eventType) {
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
          // grab the trace corresponding to this point
          var tr = x.data[pt.curveNumber];
          // add on additional trace info, if it exists
          attachKey = function(keyName) {
            if (tr.hasOwnProperty(keyName) && tr[keyName] !== null) {
              if (typeof pt.pointNumber === "number") {
                obj[keyName] = tr[keyName][pt.pointNumber];
              } else {
                obj[keyName] = tr[keyName][pt.pointNumber[0]][pt.pointNumber[1]];
              }// TODO: can pointNumber be 3D?
            }
          };
          attachKey("z");
          attachKey("key");
          return obj;
        });
        Shiny.onInputChange(
          ".clientValue-" + eventType + "-" + x.source,
          JSON.stringify(d)
        );
      };
    };

    // Debounce timeout to prevent infinite event loops when re-attaching handlers.
    // Store on graphDiv so destroy method can clear it.
    graphDiv._colorbarHandlerTimeout = null;

    // Debounced function to re-attach colorbar hover handlers
    var scheduleColorbarHandlers = function() {
      if (graphDiv._colorbarHandlerTimeout) {
        clearTimeout(graphDiv._colorbarHandlerTimeout);
      }

      // Delay handler re-attachment to let DOM settle and prevent event loops
      graphDiv._colorbarHandlerTimeout = setTimeout(function() {
        try {
          addColorbarHoverHandlers(graphDiv, x.data);
        } catch (error) {
          console.error('Error attaching colorbar hover handlers:', error);
        } finally {
          graphDiv._colorbarHandlerTimeout = null;
        }
      }, 100);
    };

    // Re-attach colorbar hover handlers after plotly redraws
    graphDiv.on('plotly_relayout', function(d) {
      scheduleColorbarHandlers();

      if (shinyMode) {
        Shiny.onInputChange(
          ".clientValue-" + "iheatmapr_relayout" + "-" + x.source,
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
        Shiny.onInputChange(".clientValue-iheatmapr_hover-" + x.source, null);
      });
      graphDiv.on('plotly_doubleclick', function(eventData) {
        Shiny.onInputChange(".clientValue-iheatmapr_click-" + x.source, null);
      });
      // 'plotly_deselect' is code for doubleclick when in select mode
      graphDiv.on('plotly_deselect', function(eventData) {
        Shiny.onInputChange(".clientValue-iheatmapr_selected-" + x.source, null);
        Shiny.onInputChange(".clientValue-iheatmapr_click-" + x.source, null);
      });
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
    }
  }

});