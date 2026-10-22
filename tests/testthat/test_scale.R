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
