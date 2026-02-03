// Function adapted from Plotly R Package 3.60,

// Decode base64 string to ArrayBuffer
function base64ToArrayBuffer(base64) {
  var binaryString = atob(base64);
  var bytes = new Uint8Array(binaryString.length);
  for (var i = 0; i < binaryString.length; i++) {
    bytes[i] = binaryString.charCodeAt(i);
  }
  return bytes.buffer;
}

// Decode binary-encoded matrix from trace
function decodeBinaryMatrix(binaryData) {
  if (!binaryData || !binaryData.data_binary || !binaryData.dims) {
    return null;
  }

  var buffer = base64ToArrayBuffer(binaryData.data_binary);
  var floatArray = new Float64Array(buffer);

  var nrows = binaryData.dims[0];
  var ncols = binaryData.dims[1];

  // Convert flat array to 2D array (column-major order from R)
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

// Process traces to decode any binary-encoded matrices
function decodeBinaryTraces(data) {
  for (var i = 0; i < data.length; i++) {
    var trace = data[i];
    if (trace.z_binary) {
      trace.z = decodeBinaryMatrix(trace.z_binary);
      delete trace.z_binary; // Clean up to save memory
    }
  }
  return data;
}

// Generate tooltip text on-demand for lazy tooltips
function generateLazyTooltip(trace, rowIdx, colIdx) {
  var lt = trace.lazy_tooltip;
  if (!lt) return null;

  var parts = [];

  if (lt.show_row && lt.row_labels && lt.row_labels[rowIdx] !== undefined) {
    parts.push(lt.prepend_row + lt.row_labels[rowIdx]);
  }

  if (lt.show_col && lt.col_labels && lt.col_labels[colIdx] !== undefined) {
    parts.push(lt.prepend_col + lt.col_labels[colIdx]);
  }

  if (lt.show_value && lt.values && lt.values[rowIdx] && lt.values[rowIdx][colIdx] !== undefined) {
    parts.push(lt.prepend_value + lt.values[rowIdx][colIdx]);
  }

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

  // Track mouse position for tooltip placement
  var mouseX = 0, mouseY = 0;
  graphDiv.addEventListener('mousemove', function(e) {
    mouseX = e.clientX;
    mouseY = e.clientY;
  });

  // Handle hover events
  graphDiv.on('plotly_hover', function(eventData) {
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
  });

  graphDiv.on('plotly_unhover', function() {
    hideLazyTooltip();
  });
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

    // Decode any binary-encoded matrices before plotting
    x.data = decodeBinaryTraces(x.data);

    var graphDiv = document.getElementById(el.id);

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

      // Remove Plotly event listeners
      try {
        graphDiv.removeAllListeners('plotly_relayout');
        graphDiv.removeAllListeners('plotly_restyle');
        graphDiv.removeAllListeners('plotly_redraw');
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