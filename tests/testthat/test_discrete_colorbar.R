context("DiscreteColorbar class validation")

# Load package for S4 class definitions
library(iheatmapr)

# Tests for DiscreteColorbar class validity ------------------------------------

test_that("DiscreteColorbar can be created with matching ticktext_full", {
  cb <- new("DiscreteColorbar",
            title = "Test",
            position = 1L,
            colors = c("red", "blue", "green"),
            ticktext = c("A", "B", "C"),
            tickvals = 1:3,
            ticktext_full = c("Alpha", "Beta", "Gamma"))

  expect_s4_class(cb, "DiscreteColorbar")
  expect_equal(length(cb@ticktext_full), length(cb@ticktext))
  expect_equal(length(cb@ticktext_full), length(cb@tickvals))
})

test_that("DiscreteColorbar validation fails when ticktext_full length != ticktext length", {
  expect_error(
    new("DiscreteColorbar",
        title = "Test",
        position = 1L,
        colors = c("red", "blue", "green"),
        ticktext = c("A", "B", "C"),
        tickvals = 1:3,
        ticktext_full = c("Alpha", "Beta")),  # Only 2 elements
    "ticktext_full must have same length as ticktext"
  )
})

test_that("DiscreteColorbar validation fails when ticktext_full length != tickvals length", {
  expect_error(
    new("DiscreteColorbar",
        title = "Test",
        position = 1L,
        colors = c("red", "blue", "green", "yellow"),
        ticktext = c("A", "B", "C", "D"),
        tickvals = 1:3,  # Only 3 tickvals but 4 ticktext
        ticktext_full = c("Alpha", "Beta", "Gamma", "Delta")),  # 4 elements matching ticktext
    "ticktext_full must have same length as tickvals"
  )
})

test_that("DiscreteColorbar can be created with empty ticktext_full", {
  # Empty ticktext_full should be allowed (validation only checks if length > 0)
  cb <- new("DiscreteColorbar",
            title = "Test",
            position = 1L,
            colors = c("red", "blue"),
            ticktext = c("A", "B"),
            tickvals = 1:2,
            ticktext_full = character(0))

  expect_s4_class(cb, "DiscreteColorbar")
  expect_equal(length(cb@ticktext_full), 0)
})

test_that("DiscreteColorbar validation passes when all lengths match", {
  cb <- new("DiscreteColorbar",
            title = "Categories",
            position = 2L,
            colors = c("red", "blue", "green", "yellow"),
            ticktext = c("Cat1", "Cat2", "Cat3", "Cat4"),
            tickvals = 1:4,
            ticktext_full = c("Category1", "Category2", "Category3", "Category4"))

  expect_equal(length(cb@ticktext), 4)
  expect_equal(length(cb@tickvals), 4)
  expect_equal(length(cb@ticktext_full), 4)
})

test_that("DiscreteColorbar validation works with single element", {
  cb <- new("DiscreteColorbar",
            title = "Single",
            position = 1L,
            colors = "red",
            ticktext = "A",
            tickvals = 1L,
            ticktext_full = "Alpha")

  expect_s4_class(cb, "DiscreteColorbar")
  expect_equal(length(cb@ticktext), 1)
  expect_equal(length(cb@tickvals), 1)
  expect_equal(length(cb@ticktext_full), 1)
})

test_that("DiscreteColorbar allows ticktext_full with special characters", {
  cb <- new("DiscreteColorbar",
            title = "Special",
            position = 1L,
            colors = c("red", "blue"),
            ticktext = c("A..", "B.."),
            tickvals = 1:2,
            ticktext_full = c("A_very.long-name!", "B_another.long-name$"))

  expect_equal(cb@ticktext_full, c("A_very.long-name!", "B_another.long-name$"))
})

test_that("DiscreteColorbar validation catches length mismatch with large vectors", {
  expect_error(
    new("DiscreteColorbar",
        title = "Large",
        position = 1L,
        colors = rep("red", 10),
        ticktext = paste0("Cat", 1:10),
        tickvals = 1:10,
        ticktext_full = paste0("Category", 1:9)),  # Only 9 elements
    "ticktext_full must have same length as ticktext"
  )
})

# Tests verifying ticktext_full is properly stored and accessible --------------

test_that("DiscreteColorbar ticktext_full slot is accessible", {
  cb <- new("DiscreteColorbar",
            title = "Accessible",
            position = 1L,
            colors = c("red", "blue"),
            ticktext = c("Short1", "Short2"),
            tickvals = 1:2,
            ticktext_full = c("VeryLongName1", "VeryLongName2"))

  expect_equal(cb@ticktext_full[1], "VeryLongName1")
  expect_equal(cb@ticktext_full[2], "VeryLongName2")
})

test_that("DiscreteColorbar preserves ticktext_full values exactly", {
  original_full <- c("Group_A_with.dots", "Group_B_with_underscores", "Group_C-with-dashes")
  cb <- new("DiscreteColorbar",
            title = "Preservation",
            position = 1L,
            colors = c("red", "blue", "green"),
            ticktext = c("A", "B", "C"),
            tickvals = 1:3,
            ticktext_full = original_full)

  expect_identical(cb@ticktext_full, original_full)
})
