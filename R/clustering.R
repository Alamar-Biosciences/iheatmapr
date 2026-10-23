#' add_row_clustering
#'
#' @param p iheatmap object
#' @param method "hclust" or "kmeans" for hierarchical or k-means clustering, 
#' respectively
#' @param k number of clusters for rows, needed if order is kmeans or optional 
#' if hclust
#' @param clust_dist distance function to use for clustering if hierarchical 
#' clustering
#' @param clust_method clustering methods passed to `hclust`
#' @param hclust_fun optional function taking the matrix (rows = items to
#' cluster) and returning an `hclust` object. Overrides `clust_dist` and
#' `clust_method`; see \code{\link{fast_hclust}} for a low-memory option.
#' @param groups vector of group assignments
#' @param colors colors to use for annotation of grouping, can be RColorBrewer 
#' palette name or
#' vector of colors
#' @param name name of colorbar indicating cluster membership
#' @param show_colorbar show the colorbar for the heatmap indicating cluster 
#' membership
#' @param xname name of xaxis
#' @param yname name of yaxis
#' @param side side of plot on which to add subplot
#' 
#' @return \code{\link{Iheatmap-class}} object, which can be printed to generate 
#' an interactive graphic
#' @export
#' @rdname add_row_clustering
#' @name add_row_clustering
#' @aliases add_row_clustering,Iheatmap-method
#' @author Alicia Schep
#' @seealso \code{\link{add_col_clustering}}, \code{\link{iheatmap}}
#' @examples
#'
#' mat <- matrix(rnorm(20), ncol = 5, nrow = 4)
#' hm <- iheatmap(mat) %>% add_row_clustering(method = "hclust", k = 2)
#' 
#' # Print heatmap if interactive session 
#' if (interactive()) hm 
setMethod(add_row_clustering,
          c(p = "Iheatmap"),
          function(p,
                   method = c("hclust","kmeans","groups"),
                   name = "Row<br>Clusters",
                   k = NULL,
                   groups = NULL,
                   clust_dist = stats::dist,
                   clust_method = 'complete',
                   colors = NULL,
                   show_colorbar = TRUE,
                   side = c("left","right"),
                   tooltip = setup_tooltip_options(),
                   xname = NULL,
                   yname = current_yaxis(p),
                   hclust_fun = NULL){
            
            method <- match.arg(method)
            side <- match.arg(side)
            
            if (!yaxes(p)[[yname]]@categorical)
              stop("Cannot cluster continuous axis!")
            
            hm <- get_heatmap(p, xname = xname, yname = yname, side = side)
            mat <- get_data(hm)
            
            if (method == "hclust"){
              dendro <- if (is.null(hclust_fun)) hclust(clust_dist(mat), method = clust_method) else hclust_fun(mat)
              if (!is.null(k)){
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
              p <- add_row_dendro(p, dendro, side = side)
            } else if (method == "kmeans"){
              if (is.null(k)) stop("No k provided")
              groups <- stats::kmeans(mat, centers = k)$cluster
              if (is.null(colors)) colors <- pick_discrete_colors(groups, p)
              p <- add_row_clusters(p,
                                    groups, 
                                    colors = colors, 
                                    name = name,
                                    side = side, 
                                    show_colorbar = show_colorbar,
                                    tooltip = tooltip)
            } else if (method == "groups"){
              if (is.null(colors)) colors <- pick_discrete_colors(groups, p)
              p <- add_row_clusters(p,
                                    groups, 
                                    colors = colors, 
                                    name = name,
                                    side = side, 
                                    show_colorbar = show_colorbar,
                                    tooltip = tooltip)
              
            }
            validObject(p)
            p
          })

#' add_col_clustering
#'
#' @param p iheatmap object
#' @param method "hclust" or "kmeans" for hierarchical or k-means clustering, 
#' respectively
#' @param k number of clusters for rows, needed if order is kmeans or optional 
#' if hclust
#' @param clust_dist distance function to use for clustering if hierarchical 
#' clustering
#' #' @param dist_method distance methods passed to `stats::dist`
#' @param clust_method clustering methods passed to `hclust`
#' @param hclust_fun optional function taking the matrix (rows = items to
#' cluster) and returning an `hclust` object. Overrides `clust_dist` and
#' `clust_method`; see \code{\link{fast_hclust}} for a low-memory option.
#' @param groups vector of group assignments
#' @param colors colors to use for annotation of grouping, can be RColorBrewer 
#' palette name or
#' vector of colors
#' @param name name of colorbar indicating cluster membership
#' @param show_colorbar show the colorbar for the heatmap indicating cluster 
#' membership
#' @param side side of plot on which to add subplot
#' @param xname name of xaxis
#' @param yname name of yaxis
#' 
#' @return \code{\link{Iheatmap-class}} object, which can be printed to generate 
#' an interactive graphic
#' @export
#' @rdname add_col_clustering
#' @name add_col_clustering
#' @aliases add_col_clustering,Iheatmap-method
#' @author Alicia Schep
#' @seealso \code{\link{add_row_clustering}}, \code{\link{iheatmap}}
#'
#' @examples
#'
#' mat <- matrix(rnorm(20), ncol = 5, nrow = 4)
#' hm <- iheatmap(mat) %>% add_col_clustering(method = "hclust", k = 2)
#' 
#' # Print heatmap if interactive session 
#' if (interactive()) hm 
setMethod(add_col_clustering,
          c(p = "Iheatmap"),
          function(p,
                   method = c("hclust","kmeans","groups"),
                   name = "Col<br>Clusters",
                   k = NULL,
                   groups = NULL,
                   clust_dist = stats::dist,
                   clust_method = 'complete',
                   colors = NULL,
                   show_colorbar = TRUE,
                   side = c("top","bottom"),
                   tooltip = setup_tooltip_options(),
                   yname = NULL,
                   xname = current_xaxis(p),
                   hclust_fun = NULL){
            
            method <- match.arg(method)
            side <- match.arg(side)
            
            if (!xaxes(p)[[xname]]@categorical)
              stop("Cannot cluster continuous axis!")
            
            hm <- get_heatmap(p, xname = xname, yname = yname, side = side)
            mat <- get_data(hm)
            
            if (method == "hclust"){
              dendro <- if (is.null(hclust_fun)) hclust(clust_dist(t(mat)), method = clust_method) else hclust_fun(t(mat))
              if (!is.null(k)){
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
              p <- add_col_dendro(p, dendro, side = side)
            } else if (method == "kmeans"){
              if (is.null(k)) stop("No k provided")
              groups <- stats::kmeans(t(mat), centers = k)$cluster
              if (is.null(colors)) colors <- pick_discrete_colors(groups, p)
              p <- add_col_clusters(p,
                                    groups,
                                    name = name,
                                    colors = colors,
                                    side = side,
                                    show_colorbar = show_colorbar,
                                    tooltip = tooltip)
            } else if (method == "groups"){
              if (is.null(colors)) colors <- pick_discrete_colors(groups, p)
              p <- add_col_clusters(p,
                                    groups,
                                    name = name,
                                    colors = colors,
                                    side = side,
                                    show_colorbar = show_colorbar,
                                    tooltip = tooltip)
            }
            validObject(p)
            p
          })


#' Scalable hierarchical clustering
#'
#' Clusters the rows of `x`. Exact clustering needs the O(n^2) distance matrix
#' of `stats::dist`, or O(n^2 * ncol) time with `fastcluster::hclust.vector`.
#' Both are too costly beyond roughly 10,000 rows, so two approximations are
#' offered. Both return an `hclust` object with one leaf per row.
#'
#' * `approx = "pca"`: cluster on the first `npcs` principal components.
#'   Euclidean distance only.
#' * `approx = "kmeans"`: reduce to `npcs` principal components, run k-means
#'   with `ncenters` centers, cluster the centers, then graft each center's
#'   members back in as a zero-height subtree. Members of one center stay
#'   adjacent and are not ordered among themselves. With another
#'   `dist_method`, k-means still groups by Euclidean distance on the
#'   components, but each center is the mean of its members in the original
#'   space and the centers are clustered with `dist_method`. The tree then
#'   follows that metric between groups and Euclidean distance within them.
#'   Ward linkage is only defined for Euclidean distance; with another metric
#'   it is applied to the centers as `hclust` does for exact trees.
#'
#' The approximations need a matrix without NA. With fewer distinct rows than
#' `ncenters`, k-means uses one center per distinct row.
#'
#' With `approx = "none"`, Euclidean distance and `ward.D2` linkage use
#' `fastcluster::hclust.vector` (O(n) memory); other combinations use
#' `stats::dist` and `hclust`.
#'
#' @param x numeric matrix, one item to cluster per row
#' @param method linkage method, as in `stats::hclust`
#' @param dist_method distance method, as in `stats::dist`
#' @param approx approximation to use, see Details
#' @param npcs number of principal components for `approx = "pca"`
#' @param ncenters number of k-means centers for `approx = "kmeans"`
#' @param seed seed for the k-means starts, so repeated calls give the same
#'   tree; the caller's RNG state is restored. `NULL` leaves the RNG alone.
#' @return an `hclust` object
#' @export
fast_hclust <- function(x, method = "ward.D2", dist_method = "euclidean",
                        approx = c("none", "pca", "kmeans"),
                        npcs = 20, ncenters = 2000, seed = 1L) {
  approx <- match.arg(approx)
  if (approx != "none" && anyNA(x)) {
    stop("approx = \"", approx, "\" requires a matrix without NA")
  }
  if (approx == "pca" && dist_method != "euclidean") {
    stop("approx = \"pca\" requires dist_method = \"euclidean\"")
  }
  full <- x
  if (approx != "none" && ncol(x) > npcs && nrow(x) > npcs) {
    x <- stats::prcomp(x, center = TRUE, scale. = FALSE, rank. = npcs)$x
  }
  if (approx == "kmeans") {
    if (nrow(x) > ncenters) {
      return(with_local_seed(seed,
                             hclust_via_kmeans(x, method, ncenters, dist_method, full)))
    }
    # Too few rows to group: the components would change a non-Euclidean metric
    if (dist_method != "euclidean") x <- full
  }
  exact_hclust(x, method, dist_method)
}

# k-means starts from random centers; without a fixed seed the tree, and any
# cluster cut from it, changes on every call. The caller's RNG state is kept.
with_local_seed <- function(seed, expr) {
  if (is.null(seed)) return(expr)
  had <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  if (had) old <- get(".Random.seed", envir = globalenv())
  on.exit(if (had) assign(".Random.seed", old, envir = globalenv())
          else if (exists(".Random.seed", envir = globalenv(), inherits = FALSE))
            rm(".Random.seed", envir = globalenv()))
  set.seed(seed)
  expr
}

exact_hclust <- function(x, method, dist_method) {
  # centroid and median in hclust.vector work on squared distances, so they
  # differ from stats::hclust; only ward.D2 matches
  vector_methods <- c(ward.D2 = "ward")
  # hclust.vector cannot see NA; dist() rescales over the non-missing pairs
  if (dist_method == "euclidean" && method %in% names(vector_methods) && !anyNA(x) &&
      requireNamespace("fastcluster", quietly = TRUE)) {
    return(fastcluster::hclust.vector(x, method = unname(vector_methods[method]),
                                      metric = "euclidean"))
  }
  stats::hclust(stats::dist(x, method = dist_method), method = method)
}

hclust_via_kmeans <- function(x, method, ncenters, dist_method = "euclidean", full = x) {
  # kmeans refuses more centers than distinct rows (repeated samples)
  ncenters <- min(ncenters, nrow(unique(x)))
  if (ncenters < 2) stop("approx = \"kmeans\" needs at least 2 distinct rows")
  km <- stats::kmeans(x, centers = ncenters, iter.max = 30, nstart = 1)
  sizes <- as.numeric(table(factor(km$cluster, levels = seq_len(ncenters))))
  keep <- which(sizes > 0)
  centers <- if (dist_method == "euclidean") {
    km$centers[keep, , drop = FALSE]
  } else {
    # The metric must see the original values, not the k-means components
    rowsum(full, km$cluster) / sizes[keep]
  }
  d <- stats::dist(centers, method = dist_method)
  if (method %in% c("ward.D", "ward.D2")) {
    # With `members`, hclust expects the Ward merge cost between clusters of
    # those sizes as the start distance, not the distance between centers
    n <- sizes[keep]
    f <- 2 * outer(n, n) / outer(n, n, "+")
    f <- f[lower.tri(f)]
    d <- d * if (method == "ward.D2") sqrt(f) else f
  }
  hc_c <- stats::hclust(d, method = method, members = sizes[keep])
  graft_leaves(hc_c, split(seq_len(nrow(x)), factor(km$cluster, levels = keep)),
               labels = rownames(x))
}

# Replace each leaf of `hc` by a chain of zero-height merges over its members
graft_leaves <- function(hc, members, labels = NULL) {
  n_chain <- sum(lengths(members) - 1L)
  chain_merge <- matrix(0L, n_chain, 2)
  top <- integer(length(members))  # merge-matrix id standing for each leaf
  row <- 0L
  for (k in seq_along(members)) {
    m <- members[[k]]
    node <- -m[1]
    for (j in seq_along(m)[-1]) {
      row <- row + 1L
      chain_merge[row, ] <- c(node, -m[j])
      node <- row
    }
    top[k] <- node
  }
  upper <- hc$merge
  neg <- upper < 0
  upper[neg] <- top[-upper[neg]]
  upper[!neg] <- upper[!neg] + n_chain
  merge <- rbind(chain_merge, upper)
  structure(list(merge = merge, height = c(rep(0, n_chain), hc$height),
                 order = hclust_order(merge), labels = labels,
                 method = hc$method, call = match.call(),
                 dist.method = hc$dist.method),
            class = "hclust")
}

hclust_order <- function(merge) {
  # Iterative: chained merges are too deep for recursion
  out <- integer(nrow(merge) + 1L)
  pos <- 0L
  stack <- nrow(merge)
  while (length(stack)) {
    id <- stack[length(stack)]
    stack <- stack[-length(stack)]
    if (id < 0) {
      pos <- pos + 1L
      out[pos] <- -id
    } else {
      stack <- c(stack, merge[id, 2], merge[id, 1])
    }
  }
  out
}
