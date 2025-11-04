// Function adapted from Plotly R Package 3.60, 
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
    
    // if no plot exists yet, create one with a particular configuration
    if (!instance.plotly) {
      Plotly.plot(graphDiv, x.data, x.layout, x.config);
      instance.plotly = true;
      instance.autosize = x.layout.autosize;

      // Add colorbar hover handlers after rendering
      addColorbarHoverHandlers(graphDiv, x.data);
    } else {
      Plotly.newPlot(graphDiv, x.data, x.layout);

      // Add colorbar hover handlers after re-rendering
      addColorbarHoverHandlers(graphDiv, x.data);
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
    
    // send user input event data to shiny
    if (shinyMode) {
      // https://plot.ly/javascript/zoom-events/
      graphDiv.on('plotly_relayout', function(d) {
        Shiny.onInputChange(
          ".clientValue-" + "iheatmapr_relayout" + "-" + x.source, 
          JSON.stringify(d)
        );
      });
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
      tooltip.style.top = (bbox.top + bbox.height/2 - tooltip.offsetHeight/2) + 'px';
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

  }

});