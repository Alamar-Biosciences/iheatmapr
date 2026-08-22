test_that("vendored plotly.js file version matches plotlyDependency()'s declared version", {
  bundle_path <- system.file("htmlwidgets", "lib", "plotlyjs", "plotly-latest.min.js",
                              package = "iheatmapr")
  banner <- paste(readLines(bundle_path, n = 6, warn = FALSE), collapse = " ")
  bundled_version <- sub(".*plotly\\.js v([0-9][^ ]*).*", "\\1", banner)

  declared_version <- iheatmapr:::plotlyDependency()$version

  expect_identical(bundled_version, declared_version)
})
