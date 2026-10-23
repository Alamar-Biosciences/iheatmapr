# iheatmapr 0.4.11

* Matrices are sent row-major (`z_binary$layout`); the binding version is bumped so browsers do not reuse a cached script that reads them as column-major.
* Label annotations are thinned to the visible range, so heatmaps with tens of thousands of samples render.
* Large heatmaps (500,000 cells or more, row-major) are drawn as a binned overview when zoomed out and as the visible full-resolution window when zoomed in, so zoom, pan and reset redraw at most 400,000 cells. Tooltips show the exact value when zoomed in and the bin mean (labelled) in the overview.
* Matrices above `getOption("iheatmapr.quantize_threshold", 2e6)` cells are sent as 16-bit integers across the finite range, halving the payload. Values are off by at most range / 131070 (about 1.4e-4 for NPQ 0-19) and tooltips print only the digits that survive. Set the option to `Inf` to keep float32.
* `compact_label_annotations()` ships big label groups (one annotation object per sample) as a template plus the label text, about 3 MB less at 26,400 samples; `to_widget()` applies it and the widget expands and thins the labels.
* Lazy windows (opt-in, Shiny only): with `options(iheatmapr.lazy_windows = TRUE)`, a matrix above `iheatmapr.lazy_threshold` (2e6) cells stays in the Shiny session. The widget gets a binned overview inline and fetches full-resolution windows as raw float32 bytes (`session$registerDataObj`) when you zoom. Zoomed-in values are exact, and a 26,400 x 250 widget is 1.7 MB instead of 17 MB. Set `iheatmapr.lazy_id` per output; the URL is tied to the session, so do not cache the widget across sessions. `to_plotly_json()` and `save_iheatmap()` always embed the data.
* `compact_hover_text()` shortens annotation bar hover text (about 2 MB at 26,400 samples) to a prefix, the main heatmap's labels, and a dictionary.
* `options(iheatmapr.quantize_bits = 12)` stores quantized matrices in 12 bits (error range / 8190, tooltips show fewer digits); the default stays 16.
* Adds `fast_hclust()` and the `hclust_fun` argument of `add_row_clustering()` / `add_col_clustering()`.
* `fast_hclust(approx = "kmeans")` accepts any `stats::dist` metric: k-means groups the rows, and the group centers are clustered with the chosen metric. With Euclidean distance on PCA components only, canberra and maximum trees had no relation to the exact tree on real data.
* `fast_hclust(approx = "none")` uses `fastcluster` only for `ward.D2`; centroid and median use `stats::hclust`, because `hclust.vector` works on squared distances. `hclust_fun` is the last argument of `add_row_clustering()` / `add_col_clustering()`.

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



