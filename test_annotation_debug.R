#!/usr/bin/env Rscript
# Debug test to verify annotation is created

library(iheatmapr)

# Create test data
set.seed(123)
mat <- matrix(rnorm(120), nrow = 12, ncol = 10)

# Create 12 long category names (>10 threshold)
long_names <- c(
  "Very_Long_Category_Name_Alpha",
  "Very_Long_Category_Name_Beta",
  "Very_Long_Category_Name_Gamma",
  "Very_Long_Category_Name_Delta",
  "Very_Long_Category_Name_Epsilon",
  "Very_Long_Category_Name_Zeta",
  "Very_Long_Category_Name_Eta",
  "Very_Long_Category_Name_Theta",
  "Very_Long_Category_Name_Iota",
  "Very_Long_Category_Name_Kappa",
  "Very_Long_Category_Name_Lambda",
  "Very_Long_Category_Name_Mu"
)

# Create groups
groups <- factor(long_names, levels = long_names)

# Create heatmap
cat("Creating heatmap...\n")
hm <- iheatmap(mat) %>%
  add_row_groups(groups, name = "Cell Types")

# Debug: Check colorbar
cat("\n=== Colorbar Info ===\n")
cat("Number of colorbars:", length(hm@colorbars), "\n")
if (length(hm@colorbars) > 0) {
  cb <- hm@colorbars[[1]]
  cat("Colorbar class:", class(cb)[1], "\n")
  cat("Title:", cb@title, "\n")
  cat("Number of ticktext:", length(cb@ticktext), "\n")
  cat("Number of ticktext_full:", length(cb@ticktext_full), "\n")
  cat("First 3 ticktext_full:\n")
  print(head(cb@ticktext_full, 3))
}

# Convert to plotly list
cat("\n=== Converting to Plotly ===\n")
plotly_list <- to_plotly_list(hm)

# Check annotations
cat("\n=== Annotations ===\n")
if (!is.null(plotly_list$layout$annotations)) {
  cat("Number of annotations:", length(plotly_list$layout$annotations), "\n")
  for (i in seq_along(plotly_list$layout$annotations)) {
    ann <- plotly_list$layout$annotations[[i]]
    cat("\nAnnotation", i, ":\n")
    cat("  Text (first 50 chars):", substr(ann$text, 1, 50), "...\n")
    cat("  Position: x=", ann$x, ", y=", ann$y, "\n")
    cat("  Has border:", !is.null(ann$bordercolor), "\n")
  }
} else {
  cat("WARNING: No annotations found!\n")
}

# Display the heatmap
cat("\n=== Displaying Heatmap ===\n")
cat("Opening in viewer/browser...\n")
print(hm)

# Also save to file
cat("\nSaving to test_annotation_output.html\n")
save_iheatmap(hm, "test_annotation_output.html")
cat("Done! Check the HTML file to see the result.\n")
