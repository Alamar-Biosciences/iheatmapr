context("colorbar helper functions")

# Tests for add_wrap_points ----------------------------------------------------

test_that("add_wrap_points does not modify text shorter than max_length", {
  text <- "short"
  result <- iheatmapr:::add_wrap_points(text, max_length = 12)
  expect_equal(result, "short")
})

test_that("add_wrap_points does not modify text exactly at max_length", {
  text <- "exactly12chr"
  result <- iheatmapr:::add_wrap_points(text, max_length = 12)
  expect_equal(result, "exactly12chr")
  expect_equal(nchar(text), 12)
})

test_that("add_wrap_points adds <br> after dots in long text", {
  text <- "this.is.a.very.long.text"
  result <- iheatmapr:::add_wrap_points(text, max_length = 12)
  expected <- "this.<br>is.<br>a.<br>very.<br>long.<br>text"
  expect_equal(result, expected)
})

test_that("add_wrap_points adds <br> after underscores in long text", {
  text <- "this_is_a_very_long_text"
  result <- iheatmapr:::add_wrap_points(text, max_length = 12)
  expected <- "this_<br>is_<br>a_<br>very_<br>long_<br>text"
  expect_equal(result, expected)
})

test_that("add_wrap_points adds <br> after both dots and underscores", {
  text <- "mixed.text_with.both_separators"
  result <- iheatmapr:::add_wrap_points(text, max_length = 12)
  expected <- "mixed.<br>text_<br>with.<br>both_<br>separators"
  expect_equal(result, expected)
})

test_that("add_wrap_points works with different max_length values", {
  text <- "a.moderately.long.text"

  result_5 <- iheatmapr:::add_wrap_points(text, max_length = 5)
  expect_match(result_5, "<br>")

  result_30 <- iheatmapr:::add_wrap_points(text, max_length = 30)
  expect_equal(result_30, text)  # Should not modify since text < 30 chars
})

test_that("add_wrap_points handles text with no dots or underscores", {
  text <- "verylongtextwithoutanyseparators"
  result <- iheatmapr:::add_wrap_points(text, max_length = 12)
  # Should return text unchanged since no dots or underscores to replace
  expect_equal(result, text)
})

test_that("add_wrap_points handles text with dots/underscores at boundaries", {
  text <- ".starts_with_separators."
  result <- iheatmapr:::add_wrap_points(text, max_length = 10)
  expected <- ".<br>starts_<br>with_<br>separators.<br>"
  expect_equal(result, expected)
})

# Tests for discrete_colorbar --------------------------------------------------

test_that("discrete_colorbar creates valid DiscreteColorbar object", {
  cb <- iheatmapr:::discrete_colorbar(
    name = "Test",
    position = 1L,
    colors = c("red", "blue", "green"),
    ticktext = c("A", "B", "C"),
    tickvals = 1:3
  )

  expect_s4_class(cb, "DiscreteColorbar")
  expect_equal(cb@title, "Test")
  expect_equal(cb@position, 1L)
  expect_equal(cb@colors, c("red", "blue", "green"))
  expect_equal(cb@ticktext, c("A", "B", "C"))
  expect_equal(cb@tickvals, 1:3)
  expect_equal(cb@ticktext_full, c("A", "B", "C"))  # Should default to ticktext
})

test_that("discrete_colorbar uses ticktext_full when provided", {
  cb <- iheatmapr:::discrete_colorbar(
    name = "Test",
    position = 1L,
    colors = c("red", "blue"),
    ticktext = c("A..", "B.."),
    tickvals = 1:2,
    ticktext_full = c("AlphaGroup", "BetaGroup")
  )

  expect_equal(cb@ticktext, c("A..", "B.."))
  expect_equal(cb@ticktext_full, c("AlphaGroup", "BetaGroup"))
})

test_that("discrete_colorbar defaults ticktext_full to ticktext when NULL", {
  cb <- iheatmapr:::discrete_colorbar(
    name = "Test",
    position = 1L,
    colors = c("red", "blue"),
    ticktext = c("Short", "Text"),
    tickvals = 1:2,
    ticktext_full = NULL
  )

  expect_equal(cb@ticktext_full, c("Short", "Text"))
})

# Tests for create_groups_colorbar ---------------------------------------------

test_that("create_groups_colorbar creates valid DiscreteColorbar", {
  groups <- factor(c("A", "B", "A", "C", "B"))
  cb <- iheatmapr:::create_groups_colorbar(
    name = "Groups",
    colorbar_position = 1L,
    colors = c("red", "blue", "green"),
    groups = groups
  )

  expect_s4_class(cb, "DiscreteColorbar")
  expect_equal(cb@title, "Groups")
  expect_equal(cb@position, 1L)
  expect_equal(cb@ticktext, c("A", "B", "C"))
  expect_equal(cb@tickvals, 1:3)
  expect_equal(cb@ticktext_full, c("A", "B", "C"))
})

test_that("create_groups_colorbar handles character vector groups", {
  groups <- c("Group1", "Group2", "Group1", "Group3")
  cb <- iheatmapr:::create_groups_colorbar(
    name = "MyGroups",
    colorbar_position = 2L,
    colors = c("red", "blue", "green"),
    groups = groups
  )

  expect_s4_class(cb, "DiscreteColorbar")
  expect_equal(cb@ticktext, c("Group1", "Group2", "Group3"))
  expect_equal(cb@tickvals, 1:3)
})

test_that("create_groups_colorbar preserves factor level order", {
  groups <- factor(c("B", "A", "C", "A"), levels = c("C", "B", "A"))
  cb <- iheatmapr:::create_groups_colorbar(
    name = "OrderedGroups",
    colorbar_position = 1L,
    colors = c("red", "blue", "green"),
    groups = groups
  )

  expect_equal(cb@ticktext, c("C", "B", "A"))
  expect_equal(cb@tickvals, 1:3)
})

test_that("create_groups_colorbar handles single group", {
  groups <- rep("OnlyGroup", 5)
  cb <- iheatmapr:::create_groups_colorbar(
    name = "Single",
    colorbar_position = 1L,
    colors = "red",
    groups = groups
  )

  expect_equal(cb@ticktext, "OnlyGroup")
  expect_equal(cb@tickvals, 1L)
  expect_equal(cb@ticktext_full, "OnlyGroup")
})

test_that("create_groups_colorbar sets ticktext and ticktext_full to same values", {
  groups <- c("LongGroupName1", "LongGroupName2", "LongGroupName3")
  cb <- iheatmapr:::create_groups_colorbar(
    name = "Test",
    colorbar_position = 1L,
    colors = c("red", "blue", "green"),
    groups = groups
  )

  # Both should contain the full group names (no truncation in create_groups_colorbar)
  expect_equal(cb@ticktext, c("LongGroupName1", "LongGroupName2", "LongGroupName3"))
  expect_equal(cb@ticktext_full, c("LongGroupName1", "LongGroupName2", "LongGroupName3"))
  expect_equal(cb@ticktext, cb@ticktext_full)
})

# Tests for colorbar merging (bug fix verification) --------------------------

test_that("Merging discrete colorbars updates tickvals correctly", {
  # Create initial heatmap with groups
  mat <- matrix(rnorm(100), nrow = 10)
  groups1 <- c(rep("A", 5), rep("B", 5))

  p1 <- iheatmapr:::main_heatmap(mat) %>%
    iheatmapr:::add_row_groups(groups1, "TestGroup")

  # Check initial colorbar has correct lengths
  cb1 <- iheatmapr:::colorbars(p1)[["TestGroup"]]
  expect_equal(length(cb1@ticktext), length(cb1@tickvals))
  expect_equal(length(cb1@ticktext_full), length(cb1@tickvals))

  # Add more groups to same heatmap (triggers merge)
  groups2 <- c(rep("B", 5), rep("C", 5))
  p2 <- p1 %>% iheatmapr:::add_row_groups(groups2, "TestGroup")

  # Check merged colorbar has correct lengths
  cb2 <- iheatmapr:::colorbars(p2)[["TestGroup"]]
  expect_equal(length(cb2@ticktext), length(cb2@tickvals))
  expect_equal(length(cb2@ticktext_full), length(cb2@tickvals))

  # Verify tickvals is sequential
  expect_equal(cb2@tickvals, seq_along(cb2@ticktext))

  # Verify we have all unique groups (A, B, C)
  expect_equal(sort(cb2@ticktext), c("A", "B", "C"))
})
