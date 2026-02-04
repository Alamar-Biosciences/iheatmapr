dendro_to_coords <- function(dendro,
                             orientation = c("left","bottom","right","top")){

  orientation <- match.arg(orientation)

  # Prevent graphics device opening by using a null device
  # ggdendro::dendro_data() triggers device creation - open null device first
  pdf(NULL)
  on.exit(dev.off(), add = TRUE)

  d <- ggdendro::dendro_data(dendro)$segments
  shapes <- vector("list", nrow(d))

  d$x <- d$x 
  d$xend <- d$xend 
  if (orientation == "left"){
    d[,c("y","yend")] <- (d[,c("y","yend")])*-1
    out <-  data.frame(x0 = d$y, x1 = d$yend, y0 = d$x, y1 = d$xend)
  } else if (orientation == "top"){
    out <- data.frame(x0 = d$x, x1 = d$xend, y0 = d$y, y1 = d$yend)
  } else if (orientation == "right"){
    out <- data.frame(x0 = d$y, x1 = d$yend, y0 = d$x, y1 = d$xend)
  } else if (orientation == "bottom"){
    d[,c("y","yend")] <- (d[,c("y","yend")])*-1
    out <- data.frame(x0 = d$x, x1 = d$xend, y0 = d$y, y1 = d$yend)
  }
  return(out)
}

dendro_to_segments <- function(dendro,
                               xaxis,
                               yaxis,
                               orientation = c("left","bottom","right","top"),
                               color = "gray"){
  orientation <- match.arg(orientation)
  d <- dendro_to_coords(dendro, orientation)
  shapes <- lapply(seq_len(nrow(d)),
                   function(x) c(as.list(d[x,]),
                                 list(type = "line",
                                      xref = xaxis,
                                      yref = yaxis,
                                      line = list(color = color))))
  return(shapes)
}

# Threshold for using compact dendrogram encoding (number of segments)
DENDRO_COMPACT_THRESHOLD <- 100

#' Create compact dendrogram representation for large dendrograms
#' @param dendro hclust object
#' @param xaxis x axis reference
#' @param yaxis y axis reference
#' @param orientation orientation of dendrogram
#' @param color line color
#' @return list with compact binary representation or NULL if not beneficial
#' @keywords internal
dendro_to_compact <- function(dendro,
                              xaxis,
                              yaxis,
                              orientation = c("left","bottom","right","top"),
                              color = "gray") {
  orientation <- match.arg(orientation)
  d <- dendro_to_coords(dendro, orientation)

  # Only use compact format for large dendrograms
  if (nrow(d) < DENDRO_COMPACT_THRESHOLD) {
    return(NULL)
  }

  # Interleave coordinates: [x0, x1, y0, y1, x0, x1, y0, y1, ...]
  coords <- as.vector(t(as.matrix(d)))

  # Convert to Float32 binary (uses in-memory approach from to_widget.R)
  raw_data <- double_to_float32_raw(coords)

  # Apply zlib compression (significant savings for dendrogram coordinates)
  compressed_data <- memCompress(raw_data, type = "gzip")
  b64_data <- base64enc::base64encode(compressed_data)

  list(
    dendro_compact = list(
      coords_binary = b64_data,
      n_segments = nrow(d),
      dtype = "float32",
      encoding = "base64-zlib",
      xref = xaxis,
      yref = yaxis,
      color = color
    )
  )
}

dendro_layout <- c(no_axis,
                   fixedrange = TRUE)

setMethod("make_shapes", signature = c(x = "Dendrogram"),
          function(x, xaxes, yaxes, ...){
            dendro <- get_data(x)
            side <- x@side

            xaxis <- id(xaxes[[xaxis_name(x)]])
            yaxis <- id(yaxes[[yaxis_name(x)]])

            # Try compact format first for large dendrograms
            compact <- dendro_to_compact(dendro, xaxis, yaxis, orientation = side)
            if (!is.null(compact)) {
              return(compact)
            }

            # Fall back to standard format for small dendrograms
            out <- dendro_to_segments(dendro, xaxis, yaxis, orientation = side)
            out
          })


#' add_row_dendro
#'
#' Adds row dendrogram to iheatmap object. Optionally adds cluster group
#' annotation if k is specified.
#' @param p iheatmap object
#' @param dendro hclust object
#' @param reorder reorder rows based on dendrogram order?
#' @param side side of plot on which to add dendrogram
#' @param size relative size of dendrogram (relative to the main heatmap)
#' @param buffer amount of space to leave empty before this plot, relative to
#' size of first heatmap
#' @param xname internal name of xaxis
#' @param yname internal name of yaxis
#' @param sname internal name of shapes
#' @param name name of colorbar indicating cluster membership (required if k is
#' specified)
#' @param k number of clusters to cut the dendrogram into
#' @param colors colors to use for annotation of grouping
#' @param show_colorbar show the colorbar for cluster membership
#' @param tooltip tooltip options for cluster annotation, see
#' \code{\link{setup_tooltip_options}}
#'
#' @return \code{\link{Iheatmap-class}} object, which can be printed to generate
#' an interactive graphic
#' @export
#' @author Alicia Schep
#' @rdname add_row_dendro
#' @name add_row_dendro
#' @aliases add_row_dendro,Iheatmap,hclust-method
#' @seealso \code{\link{add_row_clustering}}, \code{\link{iheatmap}},
#' \code{\link{add_col_dendro}}
#' @examples
#'
#' mat <- matrix(rnorm(20), ncol = 5, nrow = 4)
#' dend <- hclust(dist(mat))
#' hm <- iheatmap(mat) %>% add_row_dendro(dend)
#'
#' # With cluster annotation
#' hm2 <- iheatmap(mat) %>% add_row_dendro(dend, k = 2, name = "Clusters")
#'
#' # Print heatmap if interactive session
#' if (interactive()) hm 
setMethod(add_row_dendro, c(p = "Iheatmap", dendro = "hclust"),
          function(p,
                   dendro,
                   reorder = TRUE,
                   side = c("left","right"),
                   size = 0.15,
                   buffer = 0.005,
                   xname = NULL,
                   yname = current_yaxis(p),
                   sname = "row_dendro",
                   name = NULL,
                   k = NULL,
                   colors = NULL,
                   show_colorbar = TRUE,
                   tooltip = setup_tooltip_options()){

            side <- match.arg(side)
            new_x <- new_xaxis(p, yname, layout = dendro_layout)

            if (is.null(xname)) xname <- id(new_x)

            new_shape <- new("Dendrogram",
                            xaxis = xname,
                            yaxis = yname,
                            data = dendro,
                            side = side)

            if (reorder) axis_order(yaxes(p)[[yname]]) <- dendro$order

            # Add cluster groups annotation if k is specified
            if (!is.null(k) && !is.null(name)) {
              groups <- stats::cutree(dendro, k = k)
              if (is.null(colors)) colors <- pick_discrete_colors(groups, p)
              p <- add_row_groups(p,
                                  groups,
                                  name = name,
                                  colors = colors,
                                  side = side,
                                  show_colorbar = show_colorbar,
                                  show_title = FALSE,
                                  tooltip = tooltip)
            }

            p <- add_axis(p,
                          new_x,
                          xname = xname,
                          yname = yname,
                          size = size,
                          buffer = buffer,
                          side = side)
            p <-  add_shape(p, new_shape, sname)
            validObject(p)
            p

          })

#' add_col_dendro
#'
#' Adds column dendrogram to iheatmap object. Optionally adds cluster group
#' annotation if k is specified.
#' @param p iheatmap object
#' @param dendro hclust object
#' @param reorder reorder columns based on dendrogram order?
#' @param side side of plot on which to add dendro
#' @param size relative size of dendrogram (relative to the main heatmap)
#' @param buffer amount of space to leave empty before this plot, relative to
#' size of first heatmap
#' @param xname internal name of xaxis
#' @param yname internal name of yaxis
#' @param sname internal name of shape
#' @param name name of colorbar indicating cluster membership (required if k is
#' specified)
#' @param k number of clusters to cut the dendrogram into
#' @param colors colors to use for annotation of grouping
#' @param show_colorbar show the colorbar for cluster membership
#' @param tooltip tooltip options for cluster annotation, see
#' \code{\link{setup_tooltip_options}}
#'
#' @return \code{\link{Iheatmap-class}} object, which can be printed to generate
#' an interactive graphic
#' @export
#' @author Alicia Schep
#' @rdname add_col_dendro
#' @name add_col_dendro
#' @aliases add_col_dendro,Iheatmap,hclust-method
#' @seealso \code{\link{add_col_clustering}}, \code{\link{iheatmap}},
#' \code{\link{add_row_dendro}}
#' @examples
#'
#' mat <- matrix(rnorm(20), ncol = 5, nrow = 4)
#' dend <- hclust(dist(t(mat)))
#' hm <- iheatmap(mat) %>% add_col_dendro(dend)
#'
#' # With cluster annotation
#' hm2 <- iheatmap(mat) %>% add_col_dendro(dend, k = 2, name = "Clusters")
#'
#' # Print heatmap if interactive session
#' if (interactive()) hm 
setMethod(add_col_dendro, c(p = "Iheatmap", dendro = "hclust"),
          function(p,
                   dendro,
                   reorder = TRUE,
                   side = c("top","bottom"),
                   size = 0.15,
                   buffer = 0.005,
                   xname = current_xaxis(p),
                   yname = NULL,
                   sname = "col_dendro",
                   name = NULL,
                   k = NULL,
                   colors = NULL,
                   show_colorbar = TRUE,
                   tooltip = setup_tooltip_options()){

            side <- match.arg(side)
            new_y <- new_yaxis(p, xname, layout = dendro_layout)

            if (is.null(yname)) yname <- id(new_y)

            new_shape <- new("Dendrogram",
                             xaxis = xname,
                             yaxis = yname,
                             data = dendro,
                             side = side)

            if (reorder) axis_order(xaxes(p)[[xname]]) <- dendro$order

            # Add cluster groups annotation if k is specified
            if (!is.null(k) && !is.null(name)) {
              groups <- stats::cutree(dendro, k = k)
              if (is.null(colors)) colors <- pick_discrete_colors(groups, p)
              p <- add_col_groups(p,
                                  groups,
                                  name = name,
                                  colors = colors,
                                  side = side,
                                  show_colorbar = show_colorbar,
                                  show_title = FALSE,
                                  tooltip = tooltip)
            }

            p <- add_axis(p,
                          new_y,
                          xname = xname,
                          yname = yname,
                          size = size,
                          buffer = buffer,
                          side = side)
            p <-  add_shape(p, new_shape, sname)
            validObject(p)
            p

          })
