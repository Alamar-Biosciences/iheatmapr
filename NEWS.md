# iheatmapr 0.4.11

* Matrices are sent row-major (`z_binary$layout`); the binding version is bumped so browsers do not reuse a cached script that reads them as column-major.
* Label annotations are thinned to the visible range, so heatmaps with tens of thousands of samples render.
* Large heatmaps (500,000 cells or more, row-major) are drawn as a binned overview when zoomed out and as the visible full-resolution window when zoomed in, so zoom, pan and reset redraw at most 400,000 cells. Tooltips show the exact value when zoomed in and the bin mean (labelled) in the overview.
* Matrices above `getOption("iheatmapr.quantize_threshold", 2e6)` cells are sent as 16-bit integers across the finite range, halving the payload. Values are off by at most range / 131070 (about 1.4e-4 for NPQ 0-19) and tooltips print only the digits that survive. Set the option to `Inf` to keep float32.
* Adds `fast_hclust()` and the `hclust_fun` argument of `add_row_clustering()` / `add_col_clustering()`.

# iheatmapr 0.4.10

* Exports to_plotly_list and adds new to_plotly_json to make it easier to convert to plotly spec

# iheatmapr 0.4.9

* Adds an option to main_heatmap to show colorbar or not, by [@fboehm](https://github.com/fboehm)

# iheatmapr 0.4.8

* Update to fix error in tests due to change in R-devel random sample
* Fix issue where add_col_groups uses continuous scale if all groups are equal
* Fix for iheatmapr_event not working on relayout 
* Fix issue where show_colorbar was always set to be TRUE
* Fix example for save_iheatmap

# iheatmapr 0.4.3

* Minor bug fix for issue on R-devel.

# iheatmapr 0.4.2

* Update tests for compatibility with new version of testthat

# iheatmapr 0.4.1

* Bug fix -- actually show titles for add_row_groups, add_col_groups, etc.

# iheatmapr 0.4.0

* Added option to customize text in tooltips for heatmaps. By default, only show
three sig figs for values.  This change will break rendering of Iheatmap objects
created via older versions of iheatmapr.

# iheatmapr 0.3.0

* Removed plotly r package dependency.  iheatmapr now directly interfaces with plotlyjs instead. While this may mean having two copies of the plotly.js library, the benefit is that the htmlwidget for iheatmapr can be customized for plots created by iheatmapr and that iheatmapr now has more control over when the js component gets updated.

# iheatmapr 0.2.1

* Added a `NEWS.md` file to track changes to the package.



