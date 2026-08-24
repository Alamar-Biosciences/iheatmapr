'%ni%' <- Negate('%in%')

blue_red <- c("#0571b0","#92c5de","white","#f4a582","#ca0020")


DIVERGING_PALETTES <- list(blue_red,
                           "PiYG",
                           "PuOr",
                           "PRGn",
                           "BrBG")


SEQ_PALETTES <- c("RdPu","BuGn","YlOrRd","PuBu","Reds","Greens","Blues",
                  "Oranges","PuRd","YlGn","OrRd","BuPu","BuPuGn","YlOrBr",
                  "Purples","Greys")

DEFAULT_COLORS <- "Dark2"
DEFAULT_CONTINUOUS_PALETTE <- "BuPu"
DEFAULT_DIVERGING_PALETTE <- c(scales::muted("blue"),
                              "white",
                              scales::muted("red"))

#' @importFrom RColorBrewer brewer.pal brewer.pal.info
discrete_colors <- function(x = 2, palette = DEFAULT_COLORS){

  stopifnot(x >= 1)

  if (length(palette) == 1){
    if (x == 1){
      cols <- brewer.pal(3, palette)[1]
    } else if (x == 2){
      cols <- brewer.pal(3, palette)[c(1,3)]
    } else if ( x <= brewer.pal.info[palette, "maxcolors"]){
      cols <- brewer.pal(x, palette)
    } else{
      cols <- rep(brewer.pal(brewer.pal.info[palette, "maxcolors"],
                                          palette), length.out = x)
    }
  } else{
    if (x == 0){
      cols <- palette[1]
    } else {
      cols <- rep(palette, length.out = x)
    }
  }

  return(cols)
}


setMethod(colorscale, "DiscreteColorbar",
          function(colorbar, ...){
            x <- length(colorbar@ticktext)
            cols <- discrete_colors(x, colorbar@colors)

            br <- rep(seq(0,1,length.out = x  + 1),each = 2)[2:(2*x + 1)]

            out <- data.frame(br, rep(cols, each = 2), stringsAsFactors = FALSE)
            colnames(out) <- NULL
            return(out)
          })

# Maximum number of color stops before switching to quantile-based sampling
# Using 256 provides smooth color gradients while limiting computation
MAX_COLORSCALE_STOPS <- 256

setMethod(colorscale, "ContinuousColorbar",
          function(colorbar, z){
            zmax <- colorbar@zmax
            zmin <- colorbar@zmin
            zmid <- colorbar@zmid
            palette <- colorbar@colors
            diverging <- (zmax > zmid && zmin < zmid)

            # Check number of unique values to decide approach
            z_vec <- as.vector(z)
            n_unique <- length(unique(stats::na.omit(z_vec)))

            if (n_unique > MAX_COLORSCALE_STOPS) {
              # Use quantile-based sampling for large datasets (faster)
              probs <- seq(0, 1, length.out = MAX_COLORSCALE_STOPS)
              sampled_z <- stats::quantile(z_vec, probs = probs, na.rm = TRUE)
              vals <- scales::rescale(sampled_z, from = c(zmin, zmax))
              vals <- pmin(pmax(vals, 0), 1)  # Clamp to [0, 1]
              # Ensure boundary values are included for custom zmin/zmax
              if (!any(vals == 0)) vals <- c(0, vals)
              if (!any(vals == 1)) vals <- c(vals, 1)
            } else {
              # Use all unique values for small datasets (current behavior)
              unique_z <- stats::na.omit(unique(z_vec))
              vals <- scales::rescale(unique_z, from = c(zmin, zmax))
              vals <- vals[which(vals >= 0)]
              vals <- vals[which(vals <= 1)]
              if (zmin != min(z, na.rm = TRUE)) vals <- c(vals, 0)
              if (zmax != max(z, na.rm = TRUE)) vals <- c(vals, 1)
            }

            if (diverging) {
              mid <- scales::rescale(zmid, from = c(zmin, zmax))
              vals2 <- scales::rescale_mid(vals, mid = mid)
              cols <- scales::col_numeric(palette, domain = c(0,1))(vals2)
            } else {
              cols <- scales::col_numeric(palette, domain = c(0,1))(vals)
            }

            # Remove duplicates, sort, and return
            df <- data.frame(vals = vals, cols = cols, stringsAsFactors = FALSE)
            df <- unique(df)
            o <- order(df$vals, decreasing = FALSE)
            colz <- stats::setNames(df[o, ], NULL)
            return(colz)
          })


pick_discrete_colors <- function(groups, p = NULL){
  x <- length(levels(as.factor(groups)))
  if (!is.null(p)){
    existing <- color_palette(p, "discrete")
  } else{
    existing <- c()
  }
  choose_discrete_palette(x, existing)
}


pick_continuous_colors <- function(zmid, zmin, zmax, p = NULL){
  diverging <- (zmid > zmin && zmid < zmax)
  if (!is.null(p)){
    existing <- color_palette(p, "continuous")
  } else{
    existing <- c()
  }
  choose_continuous_palette(existing = existing, diverging = diverging)
}


choose_discrete_palette <- function(x, existing = c()){
  qual_colors <- brewer.pal.info[which(brewer.pal.info$category == "qual"),]
  new <- which(rownames(qual_colors) %ni% existing)
  if (length(new) == 0){
    warning("Reusing color palette")
    tmp1 <- vapply(rownames(qual_colors), paste0, "", collapse = "")
    tmp2 <- vapply(existing, paste0, "",collapse = "")
    existing_matches<- vapply(tmp2[which(tmp2 %in% tmp1)],
                              function(x) which(tmp1 == x), "")
    existing_tab <- tabulate(existing_matches)
    new <- which(existing_tab < max(existing_tab))
  }
  enough <- which(qual_colors$maxcolors >= x)
  if (length(intersect(new,enough)) > 0){
    qual_colors <- qual_colors[intersect(new,enough),]
    cbs <- which(qual_colors$colorblind)
    if (length(cbs) > 0){
      qual_colors <- qual_colors[cbs,]
    }
  } else if (length(enough) > 0){
    qual_colors <- qual_colors[enough,]
    cbs <- which(qual_colors$colorblind)
    if (length(cbs) > 0){
      qual_colors <- qual_colors[cbs,]
    }
  } else if (length(new) > 0){
    qual_colors <- qual_colors[new,]
    cbs <- which(qual_colors$colorblind)
    if (length(cbs) > 0){
      qual_colors <- qual_colors[cbs,]
    }
  } else{
    cbs <- which(qual_colors$colorblind)
    if (length(cbs) > 0){
      qual_colors <- qual_colors[cbs,]
    }
  }
  return(rownames(qual_colors)[1])
}

choose_continuous_palette <- function(existing = c(), diverging = TRUE){
  if (diverging){
    cand_colors <- DIVERGING_PALETTES
  } else{
    cand_colors <- SEQ_PALETTES
  }
  new <- which(cand_colors %ni% existing)
  if (length(new) == 0){
    warning("Reusing color palette")
    tmp1 <- vapply(cand_colors, paste0, "", collapse = "")
    tmp2 <- vapply(existing, paste0, "", collapse = "")
    existing_matches<- vapply(tmp2[which(tmp2 %in% tmp1)],
                              function(x) which(tmp1 == x), "")
    existing_tab <- tabulate(existing_matches)
    new <- which(existing_tab < max(existing_tab))
  }
  if (length(new) > 0){
    out <- cand_colors[new][[1]]
  } else{
    out <- cand_colors[[1]]
  }
  return(out)
}
