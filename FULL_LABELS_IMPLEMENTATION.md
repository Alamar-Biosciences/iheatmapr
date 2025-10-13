# Full Label Reference for Truncated Colorbar Categories

## Overview
This implementation adds a **reference table annotation** that displays full category names when colorbar labels are truncated due to having more than 10 categories.

## Problem Statement
PR #6 introduced label truncation for colorbars with more than 10 categorical labels to prevent visual overlap. However, this truncation caused users to lose access to full label information. This implementation solves that problem by adding a visible reference annotation box with all full labels.

## Why Not Hover Tooltips?
Initial attempts to add hover tooltips to colorbar tick labels revealed that **Plotly does not support hover text on colorbar tick labels**. This is a known limitation of the Plotly library. Instead, we implemented a more user-friendly solution: a persistent reference table annotation that's always visible.

## Implementation Details

### 1. R Backend Changes

#### File: `R/AllClasses.R`
- **Modified**: `DiscreteColorbar` class definition (lines 405-421)
- **Change**: Added `ticktext_full` slot to store original, untruncated labels
- **Purpose**: Preserves full label information throughout the rendering pipeline

#### File: `R/colorbars.R`
- **Modified**: `discrete_colorbar()` function (lines 244-253)
  - Added `ticktext_full` parameter with default value `ticktext`
  - Ensures backward compatibility with existing code

- **Modified**: `make_colorbar()` method for `DiscreteColorbar` (lines 124-205)
  - Updates colorbar title to show `(shown/total)` count (e.g., "Categories (8/12)")
  - Sets smaller tickfont size for better visibility with many categories
  - Preserves full labels in the colorbar object

#### File: `R/to_widget.R`
- **Modified**: `to_plotly_list()` function (lines 30-82)
  - **New Feature**: Detects colorbars with >10 truncated labels
  - **Adds annotation**: Creates a bordered text box with full label listing
  - **Positioning**: Places reference table to the right of the plot
  - **Styling**: Light gray border, white background, small font for compactness

### 2. Annotation Reference Table

The annotation box includes:
- **Title**: "{Colorbar Name} - Full Labels:"
- **Content**: Complete list of all category labels (untruncated)
- **Styling**:
  - Border: Light gray (#c7c7c7)
  - Background: Light (#f9f9f9)
  - Font size: 8pt for compactness
  - Left-aligned text

## User Experience

### Visual Elements
1. **Colorbar Title**: Shows count of displayed vs. total labels (e.g., "Cell Type (8/15)")
2. **Reference Box**: Visible annotation box listing all full labels
3. **Positioning**: Appears to the right of the heatmap, doesn't obscure data

### Behavior
- Reference table appears automatically when >10 categories exist
- Always visible - no interaction required
- Shows ALL categories (not just the truncated ones)
- Multiple colorbars each get their own reference box

## Technical Notes

### Backward Compatibility
- Default parameter `ticktext_full = ticktext` ensures existing code works without modification
- All existing `discrete_colorbar()` calls automatically receive full labels
- No breaking changes to API

### Layout Considerations
- Annotations use "paper" coordinates for consistent positioning
- Multiple truncated colorbars get stacked reference boxes (vertical spacing: 0.3)
- May require wider plot area for very long label lists

### Advantages Over Hover Tooltips
1. **Always visible** - no interaction required
2. **Works everywhere** - not dependent on browser/device
3. **Printable** - appears in saved/exported images
4. **Accessible** - no reliance on hover capability
5. **More informative** - shows all labels at once

## Testing

### Manual Testing
1. Create a heatmap with >10 categorical row/column groups
2. Observe abbreviated labels in colorbar with count (e.g., "(8/12)")
3. Look to the right of the plot for the reference annotation box
4. Verify all full labels are listed

### Test Script
See `test_hover_labels.R` for an example test case with 12 long category names.

## Future Enhancements

Potential improvements:
1. Make reference box position/styling configurable via parameters
2. Option to toggle reference box on/off
3. Support for collapsible/expandable reference boxes
4. Add color swatches next to labels in reference box
5. Option to show reference as a separate table below the plot

## Files Modified

1. `R/AllClasses.R` - DiscreteColorbar class definition
2. `R/colorbars.R` - colorbar creation and rendering logic
3. `R/to_widget.R` - annotation creation for label reference
4. `inst/htmlwidgets/iheatmapr.js` - (reverted to original, no JS needed)

## Related Issues/PRs

- PR #6: Label truncation for >10 categories
- Original suggestion by @srsankhe to add hover information
- Implemented as visible reference table due to Plotly limitations
