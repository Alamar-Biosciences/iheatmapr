# Threshold for using binary encoding (number of cells)
# Matrices larger than this will be encoded as base64 binary
BINARY_ENCODING_THRESHOLD <- 100000

#' Encode a numeric matrix as base64 binary
#' @param mat A numeric matrix
#' @return A list with base64 data, dimensions, and encoding flag
#' @keywords internal
encode_matrix_binary <- function(mat) {
  # Convert matrix to raw bytes (column-major order, 8-byte doubles)
  raw_data <- writeBin(as.vector(mat), raw(), size = 8)
  # Encode as base64
  b64_data <- base64enc::base64encode(raw_data)
  list(
    data_binary = b64_data,
    dims = dim(mat),
    dtype = "float64",
    encoding = "base64"
  )
}

#' Check if a trace should use binary encoding
#' @param trace A plotly trace list
#' @return TRUE if the trace has a large z matrix
#' @keywords internal
should_use_binary <- function(trace) {
  if (is.null(trace$z)) return(FALSE)
  if (!is.matrix(trace$z)) return(FALSE)
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
  shapes <- unlist(unname(lapply(p@shapes,
                                 make_shapes,
                                 xaxes = xaxes(p),
                                 yaxes = yaxes(p))),
                   recursive = FALSE, use.names = FALSE)
  annotations <- unlist(unname(lapply(p@annotations,
                                      make_annotations,
                                      xaxes = xaxes(p),
                                      yaxes = yaxes(p))),
                        recursive = FALSE)
  layout_setting <- c(get_layout(p@xaxes),
                      get_layout(p@yaxes),
                      p@layout)
  if (length(shapes) && !is.null(unlist(shapes))){
    layout_setting$shapes <- shapes
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

  out <- list(data = traces,
              layout = layout_setting,
              source = p@source,
              config = list(modeBarButtonsToRemove =
                              c("sendDataToCloud",
                                "autoScale2d")))

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




