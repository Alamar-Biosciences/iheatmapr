#!/usr/bin/env Rscript
# Test script for hover text on colorbar labels with >10 categories

library(iheatmapr)

# Create a test matrix
set.seed(123)
mat <- matrix(rnorm(100), nrow = 10, ncol = 10)

# Create categorical groups with more than 10 categories (long names)
long_category_names <- c(
  "Very_Long_Category_Name_Alpha_Extended",
  "Very_Long_Category_Name_Beta_Extended",
  "Very_Long_Category_Name_Gamma_Extended",
  "Very_Long_Category_Name_Delta_Extended",
  "Very_Long_Category_Name_Epsilon_Extended",
  "Very_Long_Category_Name_Zeta_Extended",
  "Very_Long_Category_Name_Eta_Extended",
  "Very_Long_Category_Name_Theta_Extended",
  "Very_Long_Category_Name_Iota_Extended",
  "Very_Long_Category_Name_Kappa_Extended",
  "Very_Long_Category_Name_Lambda_Extended",
  "Very_Long_Category_Name_Mu_Extended"
)

# Create row groups with 12 categories
row_groups <- factor(long_category_names, levels = long_category_names)

# Create heatmap with row groups
hm <- iheatmap(mat) %>%
  add_row_groups(row_groups, name = "Long Categories", side = "left")

# Print the heatmap (this should show abbreviated labels with hover text)
print(hm)

# Save as HTML to inspect the hover behavior
cat("Saving heatmap to test_hover_output.html\n")
save_iheatmap(hm, "test_hover_output.html")
cat("Done! Open test_hover_output.html in a browser to test hover behavior.\n")
