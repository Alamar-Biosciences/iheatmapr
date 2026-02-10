# Threshold for using binary encoding (number of cells)
# Matrices larger than this will be encoded as base64 binary
BINARY_ENCODING_THRESHOLD <- 100000

#' Convert double vector to float32 raw bytes
#' @param x numeric vector (NA values should be handled before calling)
#' @return raw vector containing float32 bytes
#' @details
#' - Inf/-Inf values are clamped to float32 max/min (acceptable for visualization)
#' - NaN values pass through as NaN
#' - Uses in-memory raw connection for efficiency
#' @keywords internal
double_to_float32_raw <- function(x) {
  # Clamp to float32 range to avoid overflow (Inf becomes float32 max)
  x <- pmax(pmin(x, 3.4028235e+38), -3.4028235e+38)

  # Use in-memory raw connection (faster than temp file)
  con <- rawConnection(raw(0), "wb")
  on.exit(close(con), add = TRUE)
  writeBin(x, con, size = 4)

  rawConnectionValue(con)
}

#' Encode a numeric matrix as base64 binary
#' @param mat A numeric matrix
#' @param use_float32 Use 32-bit floats instead of 64-bit (default TRUE, halves size)
#' @param compress Use gzip compression (default FALSE). Enable for data with
#'   compressible patterns. Note: requires browser with DecompressionStream API
#'   (Chrome 80+, Firefox 113+, Safari 16.4+).
#' @return A list with base64 data, dimensions, NA positions, and encoding metadata
#' @keywords internal
#' @importFrom base64enc base64encode
encode_matrix_binary <- function(mat, use_float32 = TRUE, compress = FALSE) {

  # Validate matrix
  if (!is.matrix(mat) || nrow(mat) == 0 || ncol(mat) == 0) {
    stop("encode_matrix_binary requires a non-empty matrix")
  }

  # Track NA positions (0-indexed for JavaScript)
  na_mask <- is.na(mat)
  has_na <- any(na_mask)

  # Replace NA with 0 for binary encoding (will be restored in JS)
  mat_clean <- mat
  if (has_na) {
    mat_clean[na_mask] <- 0
  }

  # Convert matrix to raw bytes (column-major order)
  if (use_float32) {
    raw_data <- double_to_float32_raw(as.vector(mat_clean))
    dtype <- "float32"
  } else {
    raw_data <- writeBin(as.vector(mat_clean), raw(), size = 8)
    dtype <- "float64"
  }

  # Apply gzip compression if requested
  if (compress) {
    raw_data <- memCompress(raw_data, type = "gzip")
    encoding <- "base64-gzip"
  } else {
    encoding <- "base64"
  }

  # Encode as base64
  b64_data <- base64enc::base64encode(raw_data)

  result <- list(
    data_binary = b64_data,
    dims = dim(mat),
    dtype = dtype,
    encoding = encoding
  )

  # Include NA positions if present
  if (has_na) {
    result$na_positions <- I(which(na_mask) - 1L)  # 0-indexed for JS
  }

  result
}

#' Check if a trace should use binary encoding
#' @param trace A plotly trace list
#' @return TRUE if the trace has a large z matrix
#' @keywords internal
should_use_binary <- function(trace) {
  if (is.null(trace$z)) return(FALSE)
  if (!is.matrix(trace$z)) return(FALSE)
  # Check for valid dimensions
  if (nrow(trace$z) == 0 || ncol(trace$z) == 0) return(FALSE)
  length(trace$z) > BINARY_ENCODING_THRESHOLD
}

#' Convert trace z matrix to binary if large enough
#' @param trace A plotly trace list
#' @return The trace with z optionally converted to binary
#' @keywords internal
maybe_encode_trace_binary <- function(trace) {
  if (should_use_binary(trace)) {
    # Store original z matrix as binary
    trace$z_binary <- encode_matrix_binary(trace$z)
    # Replace z with placeholder (will be decoded in JS)
    trace$z <- NULL
  }
  trace
}

#' @name to_plotly
#' @export
to_plotly_list <- function(p){
  traces <- unname(lapply(p@plots,
                          make_trace,
                          xaxes = xaxes(p),
                          yaxes = yaxes(p),
                          colorbars = p@colorbars,
                          colorbar_grid = p@colorbar_grid))
  all_shapes <- unname(lapply(p@shapes,
                                 make_shapes,
                                 xaxes = xaxes(p),
                                 yaxes = yaxes(p)))

  # Separate compact dendrograms from regular shapes
  compact_dendros <- list()
  regular_shapes <- list()

  for (shape_list in all_shapes) {
    if (!is.null(shape_list$dendro_compact)) {
      # This is a compact dendrogram
      compact_dendros <- c(compact_dendros, list(shape_list$dendro_compact))
    } else if (is.list(shape_list)) {
      # Regular shapes (list of shape objects)
      regular_shapes <- c(regular_shapes, shape_list)
    }
  }

  annotations <- unlist(unname(lapply(p@annotations,
                                      make_annotations,
                                      xaxes = xaxes(p),
                                      yaxes = yaxes(p))),
                        recursive = FALSE)
  layout_setting <- c(get_layout(p@xaxes),
                      get_layout(p@yaxes),
                      p@layout)
  if (length(regular_shapes) && !is.null(unlist(regular_shapes))){
    layout_setting$shapes <- regular_shapes
  }
  if (length(annotations) && !is.null(unlist(annotations))){
    layout_setting$annotations <- annotations
  }
  if (is.null(layout_setting$legend$x)){
    layout_setting$legend$x <- get_legend_position(p)
    layout_setting$legend$xanchor <- "left"
  }
  # Apply binary encoding to large matrices
  traces <- lapply(traces, maybe_encode_trace_binary)

  # Extract custom iheatmapr fields from traces to avoid Plotly validation stripping them
  # These fields are not standard Plotly attributes and would be removed by plotly's schema
  custom_trace_data <- list()
  custom_fields <- c("z_binary", "x_implicit", "y_implicit", "lazy_tooltip")

  for (i in seq_along(traces)) {
    trace_custom <- list()
    for (field in custom_fields) {
      if (!is.null(traces[[i]][[field]])) {
        trace_custom[[field]] <- traces[[i]][[field]]
        traces[[i]][[field]] <- NULL  # Remove from trace
      }
    }
    if (length(trace_custom) > 0) {
      custom_trace_data[[as.character(i)]] <- trace_custom
    }
  }

  out <- list(data = traces,
              layout = layout_setting,
              source = p@source,
              config = list(modeBarButtonsToRemove =
                              c("sendDataToCloud",
                                "autoScale2d")))

  # Add custom trace data separately (won't be validated by plotly)
  if (length(custom_trace_data) > 0) {
    out$iheatmapr_custom <- custom_trace_data
  }

  # Add compact dendrograms if present (decoded in JS)
  if (length(compact_dendros) > 0) {
    out$compact_dendrograms <- compact_dendros
  }

  # Use reduced precision (6 digits) for faster JSON serialization
  # Binary-encoded matrices don't need high precision in JSON
  attr(out, "TOJSON_FUNC") <- function(x, ...) {
    toJSON(x, digits = 6, auto_unbox = TRUE, force = TRUE,
           null = "null", na = "null", ...)
  }
  out
}

#' Convert Iheatmap to plotly spec
#' 
#' Function  to convert \code{link{Iheatmap-class}} object to a plotly spec 
#' either as a list or json
#' 
#' @param p \code{\link{Iheatmap-class}} object to convert
#' 
#' @return Returns a JSON for a plotly spec for to_plotly_spec and
#' as a list of same plotly object for to_plotly_list.
#' 
#' @name to_plotly
#' @export
#' @examples
#' 
#' mat <- matrix(rnorm(24), nrow = 6)
#' hm_json <- iheatmap(mat) %>% to_plotly_json()
#' hm_list <- iheatmap(mat) %>% to_plotly_list()
to_plotly_json <- function(p){
  as_list <- to_plotly_list(p)
  as_json <- attr(as_list, "TOJSON_FUNC")(as_list)
  as_json
}

plotlyDependency <- function() {
  htmltools::htmlDependency(
    name = "plotly",
    version = "2.10.1",
    package = "iheatmapr",
    src = file.path("htmlwidgets", "lib", "plotlyjs"),
    script = "plotly-latest.min.js",
    all_files = FALSE
  )
}

getPlotlySource <- function() {
  if (!is.null(getOption('iheatmapr.plotly.source'))) {
    getOption('iheatmapr.plotly.source')
  } else {
    plotlyDependency()
  }
}

#' to_widget
#' 
#' Function to convert \code{link{Iheatmap-class}} object to widget object
#' 
#' @param p \code{\link{Iheatmap-class}} object to convert
#' @return htmlwidgets object
#' @seealso \code{\link{iheatmap}}, \code{\link{main_heatmap}}
#' @export
#' @rdname to_widget
#' @name to_widget
#' @aliases to_widget,Iheatmap-method
#' @author Alicia Schep
#' @importFrom htmlwidgets sizingPolicy createWidget
#' @importFrom jsonlite toJSON
#' @examples 
#' 
#' mat <- matrix(rnorm(24), nrow = 6)
#' hm <- iheatmap(mat) %>% to_widget()
#' class(hm)
#' 
#' # Print heatmap if interactive session 
#' if (interactive()) hm 
setMethod(to_widget,
          signature = c("Iheatmap"),
          function(p){
            out <- to_plotly_list(p)
            createWidget(name = "iheatmapr",
                         x = out,
                         width = out$layout$width,
                         height = out$layout$height,
                         sizingPolicy = sizingPolicy(browser.fill = TRUE,
                                                     defaultWidth = "100%",
                                                     defaultHeight = 400),
                         dependencies = list(getPlotlySource()))
          })



setMethod("show", "Iheatmap",
          function(object){
            print(to_widget(object))
          })

#' knit_print.Iheatmap
#' 
#' @param x Iheatmap object
#' @param options knitr options
#' @keywords internal
#' @export
#' @importFrom knitr knit_print
knit_print.Iheatmap <- function(x, options){
  knit_print(to_widget(x), options = options)
}


#' save_iheatmap
#' 
#' save an \code{link{Iheatmap-class}} object, either as standalone HTML or as static 
#' pdf/png/jpeg
#' @param p \code{link{Iheatmap-class}} object
#' @param filename name of file
#' @param ... additional arguments to \code{\link[htmlwidgets]{saveWidget}} for 
#' saving as html or \code{\link[webshot]{webshot}} for saving as pdf/png/jpeg
#' @export
#' @rdname save_iheatmap
#' @name save_iheatmap
#' @aliases save_iheatmap,Iheatmap,character-method
#' @importFrom htmlwidgets saveWidget
#' @author Alicia Schep
#' @md
#' @details Note that this function requires the webshot package. If deploying
#' a shiny app that calls this function in shinyapps.io, loading the webshot 
#' library and calling `webshot::install_phantomjs()` is needed for the the save
#' functionality to work. 
#' @examples
#' mat <- matrix(rnorm(24), nrow = 6)
#' hm <- iheatmap(mat)
#' \dontrun{
#' save_iheatmap(hm, "example_iheatmap.png")
#' }
setMethod(save_iheatmap, c("Iheatmap","character"),
          function(p, filename, ...){
            
            fileType <- tolower(tools::file_ext(filename))
            if (!fileType %in% c('jpeg', 'png', 'html','pdf')) {
              stop("File type ", fileType, " not supported", call. = FALSE)
            }          
            if (fileType == "html"){
              saveWidget(to_widget(p), filename, ...)
            } else{
              if (!requireNamespace("webshot",quietly = TRUE))
                stop('Please install the webshot package for saving static plot')
              f <- basename(tempfile('iheatmapr', '.', '.html'))
              on.exit(unlink(f), add = TRUE)
              html <- saveWidget(to_widget(p), f, selfcontained = TRUE)
              webshot::webshot(f, filename, ...)
            }
          })




