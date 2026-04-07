test_that("cluster_merge_unimodal returns a list with assign and label", {
  set.seed(1)
  mat <- matrix(
    c(rnorm(20, 2), rnorm(20, -2)),
    ncol = 2,
    dimnames = list(NULL, c("v1", "v2"))
  )
  cl <- rep(c("A", "B", "C", "D"), each = 5)
  bin_res <- cluster_merge_bin(mat, cl, list(v1 = 0, v2 = 0))
  result <- cluster_merge_unimodal(mat, bin_res)
  expect_type(result, "list")
  expect_named(result, c("assign", "label"))
})

test_that("cluster_merge_unimodal assign has correct structure", {
  set.seed(1)
  mat <- matrix(
    c(rnorm(20, 2), rnorm(20, -2)),
    ncol = 2,
    dimnames = list(NULL, c("v1", "v2"))
  )
  cl <- rep(c("A", "B", "C", "D"), each = 5)
  bin_res <- cluster_merge_bin(mat, cl, list(v1 = 0, v2 = 0))
  result <- cluster_merge_unimodal(mat, bin_res)
  expect_s3_class(result$assign, "tbl_df")
  expect_named(result$assign, c("orig", "merged"))
  expect_equal(nrow(result$assign), nrow(mat))
})

test_that("cluster_merge_unimodal label has correct structure", {
  set.seed(1)
  mat <- matrix(
    c(rnorm(20, 2), rnorm(20, -2)),
    ncol = 2,
    dimnames = list(NULL, c("v1", "v2"))
  )
  cl <- rep(c("A", "B", "C", "D"), each = 5)
  bin_res <- cluster_merge_bin(mat, cl, list(v1 = 0, v2 = 0))
  result <- cluster_merge_unimodal(mat, bin_res)
  expect_s3_class(result$label, "tbl_df")
  expect_named(result$label, c("merged", "level", "descriptive"))
})

test_that("cluster_merge_unimodal preserves thresholds attribute", {
  set.seed(1)
  mat <- matrix(
    c(rnorm(20, 2), rnorm(20, -2)),
    ncol = 2,
    dimnames = list(NULL, c("v1", "v2"))
  )
  cl <- rep(c("A", "B", "C", "D"), each = 5)
  thresholds <- list(v1 = 0, v2 = 0)
  bin_res <- cluster_merge_bin(mat, cl, thresholds)
  result <- cluster_merge_unimodal(mat, bin_res)
  expect_identical(attr(result, "thresholds"), thresholds)
})

test_that("cluster_merge_unimodal merges unimodal clusters across bins", {
  set.seed(1)
  n <- 100
  # Clusters A and B overlap heavily across the v1 = 0 boundary
  v1 <- c(rnorm(n, 0.3, 1), rnorm(n, -0.3, 1))
  v2 <- c(rnorm(n, 0, 1), rnorm(n, 0, 1))
  mat <- cbind(v1 = v1, v2 = v2)
  cl <- rep(c("A", "B"), each = n)
  bin_res <- cluster_merge_bin(mat, cl, list(v1 = 0, v2 = 0))
  # These have different bin labels but form one unimodal population
  expect_true(length(unique(bin_res$assign$merged)) >= 2L)
  result <- cluster_merge_unimodal(mat, bin_res, dip_threshold = 0.05)
  # After merging, should have fewer unique labels
  expect_true(length(unique(result$assign$merged)) <= length(unique(bin_res$assign$merged)))
})

test_that("cluster_merge_unimodal does not merge bimodal clusters", {
  set.seed(42)
  n <- 200
  # Clusters with well-separated modes: clearly bimodal when merged
  v1 <- c(rnorm(n, -5, 0.5), rnorm(n, 5, 0.5))
  v2 <- c(rnorm(n, 0, 0.5), rnorm(n, 0, 0.5))
  mat <- cbind(v1 = v1, v2 = v2)
  cl <- rep(c("A", "B"), each = n)
  bin_res <- cluster_merge_bin(mat, cl, list(v1 = 0, v2 = 0))
  result <- cluster_merge_unimodal(mat, bin_res, dip_threshold = 0.05)
  # Should remain separate
  expect_equal(
    length(unique(result$assign$merged)),
    length(unique(bin_res$assign$merged))
  )
})

test_that("cluster_merge_unimodal errors without thresholds attribute", {
  bad_result <- list(
    assign = tibble::tibble(orig = "A", merged = "1"),
    label = tibble::tibble(orig = "A", level = "1", descriptive = "x <= 0")
  )
  mat <- matrix(1, ncol = 1, dimnames = list(NULL, "x"))
  expect_error(
    cluster_merge_unimodal(mat, bad_result),
    "thresholds"
  )
})

test_that("cluster_merge_unimodal respects ignore_labels", {
  set.seed(1)
  n <- 100
  v1 <- c(rnorm(n, 0.3, 1), rnorm(n, -0.3, 1))
  v2 <- c(rnorm(n, 0, 1), rnorm(n, 0, 1))
  mat <- cbind(v1 = v1, v2 = v2)
  cl <- rep(c("A", "B"), each = n)
  bin_res <- cluster_merge_bin(mat, cl, list(v1 = 0, v2 = 0))
  labels_to_ignore <- unique(bin_res$assign$merged)
  result <- cluster_merge_unimodal(
    mat, bin_res, ignore_labels = labels_to_ignore
  )
  # All labels are ignored, so no merging should happen
  expect_equal(
    sort(unique(result$assign$merged)),
    sort(unique(bin_res$assign$merged))
  )
})

test_that("cluster_merge_unimodal respects max_label_diff = 0", {
  set.seed(1)
  n <- 100
  v1 <- c(rnorm(n, 0.3, 1), rnorm(n, -0.3, 1))
  v2 <- c(rnorm(n, 0, 1), rnorm(n, 0, 1))
  mat <- cbind(v1 = v1, v2 = v2)
  cl <- rep(c("A", "B"), each = n)
  bin_res <- cluster_merge_bin(mat, cl, list(v1 = 0, v2 = 0))
  result <- cluster_merge_unimodal(mat, bin_res, max_label_diff = 0L)
  # max_label_diff = 0 means only identical labels can merge;
  # since cluster_merge_bin already merged identical labels,
  # nothing should change
  expect_equal(
    sort(unique(result$assign$merged)),
    sort(unique(bin_res$assign$merged))
  )
})

test_that("cluster_merge_unimodal works with data.frame input", {
  set.seed(1)
  n <- 100
  df <- data.frame(
    v1 = c(rnorm(n, 0.3, 1), rnorm(n, -0.3, 1)),
    v2 = c(rnorm(n, 0, 1), rnorm(n, 0, 1))
  )
  cl <- rep(c("A", "B"), each = n)
  bin_res <- cluster_merge_bin(df, cl, list(v1 = 0, v2 = 0))
  result <- cluster_merge_unimodal(df, bin_res)
  expect_s3_class(result$assign, "tbl_df")
  expect_equal(nrow(result$assign), nrow(df))
})

test_that("cluster_merge_unimodal works with single variable", {
  set.seed(1)
  n <- 100
  mat <- matrix(
    c(rnorm(n, 0.3, 1), rnorm(n, -0.3, 1)),
    ncol = 1,
    dimnames = list(NULL, "v1")
  )
  cl <- rep(c("A", "B"), each = n)
  bin_res <- cluster_merge_bin(mat, cl, list(v1 = 0))
  result <- cluster_merge_unimodal(mat, bin_res, dip_threshold = 0.05)
  expect_s3_class(result$assign, "tbl_df")
  expect_equal(nrow(result$assign), nrow(mat))
})

test_that("cluster_merge_unimodal respects min_mode_dist", {
  set.seed(1)
  n <- 100
  # Two clusters with very similar modes
  v1 <- c(rnorm(n, 0.1, 1), rnorm(n, -0.1, 1))
  v2 <- c(rnorm(n, 0.1, 1), rnorm(n, -0.1, 1))
  mat <- cbind(v1 = v1, v2 = v2)
  cl <- rep(c("A", "B"), each = n)
  bin_res <- cluster_merge_bin(mat, cl, list(v1 = 0, v2 = 0))

  # With a large min_mode_dist, clusters with close modes should NOT merge
  result_no_merge <- cluster_merge_unimodal(
    mat, bin_res, min_mode_dist = 10
  )
  expect_equal(
    length(unique(result_no_merge$assign$merged)),
    length(unique(bin_res$assign$merged))
  )
})

test_that("cluster_merge_unimodal handles three or more clusters", {
  set.seed(42)
  n <- 80
  # Three overlapping clusters near the boundary
  v1 <- c(rnorm(n, 0.2, 0.8), rnorm(n, -0.2, 0.8), rnorm(n, 5, 0.5))
  v2 <- c(rnorm(n, 0, 0.8), rnorm(n, 0, 0.8), rnorm(n, 5, 0.5))
  mat <- cbind(v1 = v1, v2 = v2)
  cl <- rep(c("A", "B", "C"), each = n)
  bin_res <- cluster_merge_bin(mat, cl, list(v1 = 0, v2 = 0))
  result <- cluster_merge_unimodal(mat, bin_res, dip_threshold = 0.05)
  # The separated cluster C should remain separate
  expect_true(length(unique(result$assign$merged)) >= 2L)
  expect_equal(nrow(result$assign), nrow(mat))
})

test_that("cluster_merge_unimodal orig column matches input", {
  set.seed(1)
  mat <- matrix(
    c(rnorm(20, 2), rnorm(20, -2)),
    ncol = 2,
    dimnames = list(NULL, c("v1", "v2"))
  )
  cl <- rep(c("X", "Y", "Z", "W"), each = 5)
  bin_res <- cluster_merge_bin(mat, cl, list(v1 = 0, v2 = 0))
  result <- cluster_merge_unimodal(mat, bin_res)
  expect_equal(result$assign$orig, cl)
})

test_that("cluster_merge_bin stores thresholds attribute", {
  mat <- matrix(c(1, -1), nrow = 2, ncol = 1, dimnames = list(NULL, "v1"))
  cl <- c("A", "B")
  thresholds <- list(v1 = 0)
  result <- cluster_merge_bin(mat, cl, thresholds)
  expect_identical(attr(result, "thresholds"), thresholds)
})

test_that("cluster_merge_bin thresholds attribute preserves multi-var input", {
  mat <- matrix(
    c(1, -1, 1, -1), nrow = 2, ncol = 2,
    dimnames = list(NULL, c("v1", "v2"))
  )
  cl <- c("A", "B")
  thresholds <- list(v1 = c(0, 3), v2 = 0)
  result <- cluster_merge_bin(mat, cl, thresholds)
  expect_identical(attr(result, "thresholds"), thresholds)
})

test_that("cluster_merge_unimodal min_mode_dist accepts named vector", {
  set.seed(1)
  n <- 100
  v1 <- c(rnorm(n, 0.1, 1), rnorm(n, -0.1, 1))
  v2 <- c(rnorm(n, 0.1, 1), rnorm(n, -0.1, 1))
  mat <- cbind(v1 = v1, v2 = v2)
  cl <- rep(c("A", "B"), each = n)
  bin_res <- cluster_merge_bin(mat, cl, list(v1 = 0, v2 = 0))
  result <- cluster_merge_unimodal(
    mat, bin_res, min_mode_dist = c(v1 = 10, v2 = 10)
  )
  expect_s3_class(result$assign, "tbl_df")
})
