#!/usr/bin/Rscript

# Simple test script to verify the implementation

# Load required packages
library(ggplot2)
library(cowplot)
library(dplyr)
library(tibble)
library(purrr)

# Source the function
source("R/plot_cluster_scatter.R")

# Create test data
set.seed(1)
test_data <- data.frame(
  cluster = rep(paste0("C", 1:3), each = 20),
  var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2)),
  var2 = c(rnorm(20, -1), rnorm(20, 1), rnorm(20, 0)),
  var3 = c(rnorm(20, 1), rnorm(20, -1), rnorm(20, 0))
)

cat("Test 1: PCA method (default)\n")
tryCatch({
  p1 <- plot_cluster_scatter(test_data, cluster = "cluster")
  cat("✓ PCA plot created successfully\n")
  class(p1)
}, error = function(e) {
  cat("✗ Error:", e$message, "\n")
})

cat("\nTest 2: Raw method\n")
tryCatch({
  p2 <- plot_cluster_scatter(test_data, cluster = "cluster", dim_red = "none")
  cat("✓ Raw plot created successfully\n")
}, error = function(e) {
  cat("✗ Error:", e$message, "\n")
})

cat("\nTest 3: Custom colors\n")
tryCatch({
  colors <- c("C1" = "red", "C2" = "blue", "C3" = "green")
  p3 <- plot_cluster_scatter(test_data, cluster = "cluster", point_col = colors)
  cat("✓ Plot with custom colors created successfully\n")
}, error = function(e) {
  cat("✗ Error:", e$message, "\n")
})

cat("\nTest 4: Subset of variables\n")
tryCatch({
  p4 <- plot_cluster_scatter(test_data, cluster = "cluster", vars = c("var1", "var2"))
  cat("✓ Plot with subset of variables created successfully\n")
}, error = function(e) {
  cat("✗ Error:", e$message, "\n")
})

cat("\nTest 5: Missing values\n")
tryCatch({
  test_data_na <- test_data
  test_data_na$var2[c(5, 10, 15)] <- NA
  p5 <- plot_cluster_scatter(test_data_na, cluster = "cluster")
  cat("✓ Plot with missing values handled successfully\n")
}, error = function(e) {
  cat("✗ Error:", e$message, "\n")
})

cat("\nAll basic tests completed!\n")
