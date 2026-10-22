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

test_that("window_bins averages blocks counted from the window corner and skips NA", {
  set.seed(1)
  m <- matrix(rnorm(7 * 10), 7, 10)
  m[2, 3] <- NA
  expect_equal(window_bins(m, 1, 6, 2, 9, 1, 1), m[2:6, 3:9])
  b <- window_bins(m, 1, 6, 2, 9, 2, 3)  # 5 x 7 window -> 3 x 3 blocks
  expect_equal(dim(b), c(3, 3))
  sub <- m[2:6, 3:9]
  expect_equal(b[1, 1], mean(sub[1:2, 1:3], na.rm = TRUE))
  expect_equal(b[3, 3], mean(sub[5, 7]))           # ragged last block
  expect_equal(b[2, 2], mean(sub[3:4, 4:6]))
  m2 <- matrix(NA_real_, 4, 4)
  expect_true(all(is.nan(window_bins(m2, 0, 4, 0, 4, 2, 2))))
})

test_that("lod_factors keeps a window under the budget", {
  expect_equal(unname(lod_factors(250, 26400)), c(1, 32))
  expect_equal(unname(lod_factors(26400, 250)), c(32, 1))
  expect_equal(unname(lod_factors(100, 100)), c(1, 1))
  f <- lod_factors(5000, 5000)
  expect_lte(ceiling(5000 / f[["fr"]]) * ceiling(5000 / f[["fc"]]), 4e5)
})

test_that("lazy_window_handler serves float32 windows and rejects bad requests", {
  skip_if_not_installed("shiny")
  m <- matrix(as.numeric(1:60), 6, 10)
  m[2, 2] <- NA
  req <- function(q) list(QUERY_STRING = q)
  ok <- lazy_window_handler(list(m = m), req("?w=x&r0=1&r1=4&c0=2&c1=8&fr=1&fc=1"))
  expect_equal(ok$status, 200L)
  vals <- readBin(ok$content, "numeric", n = 3 * 6, size = 4)
  expect_equal(vals, as.vector(t(m[2:4, 3:8])))
  nan <- lazy_window_handler(list(m = m), req("?r0=0&r1=2&c0=0&c1=2&fr=2&fc=2"))
  expect_equal(readBin(nan$content, "numeric", n = 1, size = 4), mean(m[1:2, 1:2], na.rm = TRUE),
               tolerance = 1e-6)
  for (q in c("?r0=0&r1=2&c0=0&c1=2&fr=0&fc=1", "?r0=0&r1=99&c0=0&c1=2&fr=1&fc=1",
              "?r0=a&r1=2&c0=0&c1=2&fr=1&fc=1", "?r0=0&r1=2&c0=0&c1=2")) {
    expect_equal(lazy_window_handler(list(m = m), req(q))$status, 400L)
  }
})

test_that("compact_hover_text round-trips annotation bar hover text", {
  set.seed(1)
  n <- 1500
  m <- matrix(rnorm(8 * n), 8, n, dimnames = list(paste0("T", 1:8), sprintf("S%04d", 1:n)))
  grp <- sample(c("A", "B", "C"), n, TRUE)
  hm <- main_heatmap(m, tooltip = setup_tooltip_options(prepend_col = "Sample: ")) %>%
    add_col_groups(grp, name = "Group", title = "Group")
  l <- to_plotly_list(hm)
  bar <- which(vapply(l$data, function(d) identical(d$hoverinfo, "text"), NA))
  expect_length(bar, 1)
  # Callers (NAS) rewrite the bar text to "<prefix><sample label><br><title>: <value>"
  labels <- l$iheatmapr_custom[["1"]]$lazy_tooltip$col_labels
  before <- sprintf("Sample: %s<br>Group: %s", labels, grp)
  l$data[[bar]]$text <- matrix(before, nrow = 1)
  out <- compact_hover_text(l)
  h <- out$data[[bar]]$hover_compact
  expect_null(out$data[[bar]]$text)
  expect_equal(h$orient, "col")
  expect_equal(h$prefix, "Sample: ")
  expect_lte(length(h$tails), 3)
  expect_equal(paste0(h$prefix, labels, "<br>", h$tails[h$idx + 1L]), before)
  # a bar whose text does not start with the main heatmap labels is left alone
  l2 <- l
  l2$iheatmapr_custom[["1"]]$lazy_tooltip$col_labels <- rev(labels)
  expect_false(is.null(compact_hover_text(l2)$data[[bar]]$text))
})

decode_uint12 <- function(enc) {
  b <- as.integer(base64enc::base64decode(enc$data_binary))
  i <- seq(1, length(b), by = 3)
  as.vector(rbind(b[i] + bitwShiftL(bitwAnd(b[i + 1], 15L), 8L),
                  bitwShiftR(b[i + 1], 4L) + bitwShiftL(b[i + 2], 4L)))[seq_len(prod(enc$dims))]
}

test_that("12-bit quantization is within half a step and 25% smaller than 16-bit", {
  set.seed(1)
  for (dims in list(c(6, 10), c(5, 7))) {  # even and odd cell counts
    m <- matrix(rnorm(prod(dims), sd = 5), dims[1], dims[2])
    m[1, 2] <- NA
    enc <- encode_matrix_binary(m, quantize = TRUE, quantize_bits = 12)
    expect_equal(enc$dtype, "uint12")
    expect_equal(enc$q_bits, 12)
    lo <- as.numeric(enc$q_min)
    hi <- as.numeric(enc$q_max)
    decoded <- lo + decode_uint12(enc) * (hi - lo) / 4095
    truth <- as.vector(t(pmin(pmax(replace(m, is.na(m), 0), lo), hi)))
    expect_lte(max(abs(decoded - truth)), (hi - lo) / 8190 + 1e-12)
  }
  big <- matrix(rnorm(40000), 200, 200)
  r12 <- nchar(encode_matrix_binary(big, quantize = TRUE, quantize_bits = 12)$data_binary)
  r16 <- nchar(encode_matrix_binary(big, quantize = TRUE)$data_binary)
  expect_equal(r12 / r16, 0.75, tolerance = 0.01)
  expect_error(encode_matrix_binary(big, quantize = TRUE, quantize_bits = 8), "12 or 16")
})

test_that("maybe_encode_trace_binary serves a big matrix lazily only with the option, a session and sequential axes", {
  skip_if_not_installed("shiny")
  registered <- list()
  session <- list(registerDataObj = function(name, data, filterFunc) {
    registered[[name]] <<- data
    paste0("session/tok/dataobj/", name, "?w=1&nonce=2")
  })
  trace <- list(z = I(matrix(rnorm(300 * 400), 300, 400)), x_implicit = 400, y_implicit = 300)
  lazy <- function(tr, ...) shiny::withReactiveDomain(session, maybe_encode_trace_binary(tr, ...))

  old <- options(iheatmapr.lazy_windows = TRUE, iheatmapr.lazy_threshold = 1e5, iheatmapr.lazy_id = "hm")
  on.exit(options(old))
  out <- lazy(trace, 2L)
  expect_null(out[["z"]])  # [[ avoids $ partial matching of z_binary
  expect_equal(out$lazy_window$dims, c(300, 400))
  expect_equal(out$lazy_window$url, "session/tok/dataobj/hm_2?w=1&nonce=2")
  expect_equal(names(registered), "hm_2")
  expect_equal(dim(registered[[1]]$m), c(300, 400))
  # the inline overview is the whole matrix at the factors the browser expects
  f <- lod_factors(300, 400)
  expect_equal(out$z_binary$dims, c(ceiling(300 / f[["fr"]]), ceiling(400 / f[["fc"]])))
  # no session, option off, non-sequential axes, or below the threshold: inline
  expect_null(maybe_encode_trace_binary(trace)$lazy_window)
  options(iheatmapr.lazy_windows = FALSE)
  expect_null(lazy(trace)$lazy_window)
  options(iheatmapr.lazy_windows = TRUE)
  expect_null(lazy(modifyList(trace, list(x_implicit = NULL)))$lazy_window)
  options(iheatmapr.lazy_threshold = 1e7)
  expect_null(lazy(trace)$lazy_window)
})
