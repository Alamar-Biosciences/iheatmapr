context("scaling helpers")

test_that("fast_hclust matches stats::hclust for exact ward.D2", {
  set.seed(1)
  x <- matrix(rnorm(200), 40, 5)
  expect_equal(fast_hclust(x)$height, stats::hclust(dist(x), "ward.D2")$height)
  expect_equal(fast_hclust(x)$order, stats::hclust(dist(x), "ward.D2")$order)
})

test_that("fast_hclust handles NA like stats::hclust, and approximations reject it", {
  set.seed(1)
  x <- matrix(rnorm(200), 40, 5)
  x[3, 2] <- NA
  expect_equal(fast_hclust(x)$order, stats::hclust(dist(x), "ward.D2")$order)
  expect_error(fast_hclust(x, approx = "pca"), "without NA")
  expect_error(fast_hclust(x, approx = "kmeans"), "without NA")
})

test_that("approximate fast_hclust returns a valid tree over every row", {
  set.seed(1)
  x <- matrix(rnorm(600 * 8), 600, 8)
  for (ap in c("pca", "kmeans")) {
    h <- fast_hclust(x, approx = ap, npcs = 3, ncenters = 50)
    expect_equal(sort(h$order), 1:600)
    expect_equal(nrow(h$merge), 599)
    expect_false(is.unsorted(h$height))
    expect_length(stats::cutree(h, 4), 600)
  }
  expect_error(fast_hclust(x, dist_method = "manhattan", approx = "pca"))
})

test_that("hclust_fun replaces clust_dist in add_col_clustering", {
  set.seed(1)
  m <- matrix(rnorm(60), 6, 10)
  called <- FALSE
  hm <- main_heatmap(m) %>%
    add_col_clustering(hclust_fun = function(x) { called <<- TRUE; fast_hclust(x) })
  expect_true(called)
})

test_that("row-major binary encoding lists NA positions in buffer order", {
  m <- matrix(1:6 + 0.5, 2, 3)
  m[2, 1] <- NA
  enc <- encode_matrix_binary(m)
  expect_equal(enc$layout, "row-major")
  buf <- readBin(base64enc::base64decode(enc$data_binary), "numeric", n = 6, size = 4)
  # row 2 starts at offset 3, so [2, 1] is flat index 3
  expect_equal(as.integer(enc$na_positions), 3L)
  expect_equal(buf[c(1, 2, 3, 5, 6)], as.vector(t(m))[c(1, 2, 3, 5, 6)])
  col <- encode_matrix_binary(m, row_major = FALSE)
  expect_equal(as.integer(col$na_positions), 1L)
})

decode_uint16 <- function(enc) {
  readBin(base64enc::base64decode(enc$data_binary), "integer",
          n = prod(enc$dims), size = 2, signed = FALSE, endian = "little")
}

test_that("quantized encoding is within half a step and keeps NA positions", {
  set.seed(1)
  m <- matrix(rnorm(60, sd = 5), 6, 10)
  m[2, 3] <- NA
  m[4, 5] <- Inf
  enc <- encode_matrix_binary(m, quantize = TRUE)
  expect_equal(enc$dtype, "uint16")
  lo <- as.numeric(enc$q_min)
  hi <- as.numeric(enc$q_max)
  expect_equal(lo, min(m[is.finite(m)]))
  expect_equal(hi, max(m[is.finite(m)]))
  decoded <- lo + decode_uint16(enc) * (hi - lo) / 65535
  truth <- as.vector(t(pmin(pmax(replace(m, is.na(m), 0), lo), hi)))
  expect_lte(max(abs(decoded - truth)), (hi - lo) / 131070 + 1e-12)
  expect_equal(as.integer(enc$na_positions), which(t(is.na(m))) - 1L)
})

test_that("quantization handles a constant matrix and halves the payload", {
  enc <- encode_matrix_binary(matrix(3, 4, 5), quantize = TRUE)
  expect_equal(as.numeric(enc$q_min), 3)
  expect_equal(as.numeric(enc$q_max), 3)
  expect_true(all(decode_uint16(enc) == 0))
  m <- matrix(rnorm(20000), 100, 200)
  expect_equal(nchar(encode_matrix_binary(m, quantize = TRUE)$data_binary) /
                 nchar(encode_matrix_binary(m)$data_binary), 0.5, tolerance = 0.01)
})

test_that("quantize_threshold controls when traces are quantized", {
  trace <- list(z = matrix(rnorm(200000), 400, 500))
  local({
    old <- options(iheatmapr.quantize_threshold = 1e5)
    on.exit(options(old))
    expect_equal(maybe_encode_trace_binary(trace)$z_binary$dtype, "uint16")
  })
  local({
    old <- options(iheatmapr.quantize_threshold = Inf)
    on.exit(options(old))
    expect_equal(maybe_encode_trace_binary(trace)$z_binary$dtype, "float32")
  })
  # default threshold (2e6 cells): a 200k-cell trace stays float32
  expect_equal(maybe_encode_trace_binary(trace)$z_binary$dtype, "float32")
})

expand_label_groups <- function(hm_list) {
  anns <- hm_list$layout$annotations
  for (g in hm_list$label_groups) {
    for (k in seq_along(g$text)) {
      a <- g$template
      a[[g$along]] <- if (is.null(g$pos)) k else g$pos[k]
      a$text <- g$text[k]
      anns[[length(anns) + 1]] <- a
    }
  }
  anns
}

test_that("compact_label_annotations round-trips large label groups", {
  set.seed(1)
  m <- matrix(rnorm(450 * 6), 450, 6, dimnames = list(sprintf("S%03d", 1:450), paste0("T", 1:6)))
  l <- to_plotly_list(main_heatmap(m) %>% add_row_labels() %>% add_col_labels())
  before <- l$layout$annotations
  out <- compact_label_annotations(l)
  expect_length(out$label_groups, 1)
  expect_equal(out$label_groups[[1]]$along, "y")
  expect_length(out$layout$annotations, 6)  # the 6 target labels stay: group too small
  key <- function(a) paste(a$xref, a$yref, a$text, a$x, a$y, sep = "|")
  expect_setequal(vapply(expand_label_groups(out), key, ""), vapply(before, key, ""))
  expect_lt(nchar(jsonlite::toJSON(out$layout$annotations, auto_unbox = TRUE)) +
              nchar(jsonlite::toJSON(out$label_groups, auto_unbox = TRUE)),
            0.5 * nchar(jsonlite::toJSON(before, auto_unbox = TRUE)))
  # small label sets are untouched
  small <- to_plotly_list(main_heatmap(m[1:20, ]) %>% add_row_labels())
  expect_null(compact_label_annotations(small)$label_groups)
})

test_that("compact_label_annotations keeps positions that are a permutation", {
  set.seed(1)
  m <- matrix(rnorm(450 * 6), 6, 450, dimnames = list(paste0("T", 1:6), sprintf("S%03d", 1:450)))
  hm <- main_heatmap(m) %>% add_col_labels()
  l <- to_plotly_list(hm %>% reorder_cols(sample(450)))
  before <- l$layout$annotations
  out <- compact_label_annotations(l)
  expect_false(is.null(out$label_groups[[1]]$pos))
  key <- function(a) paste(a$xref, a$yref, a$text, a$x, a$y, sep = "|")
  expect_setequal(vapply(expand_label_groups(out), key, ""), vapply(before, key, ""))
})
