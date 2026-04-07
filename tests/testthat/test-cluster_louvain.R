test_that("cluster_louvain returns character vector of correct length", {
  set.seed(1)
  mat <- matrix(
    c(rnorm(50, -3), rnorm(50, 3), rnorm(100, 0)),
    ncol = 2,
    dimnames = list(NULL, c("v1", "v2"))
  )
  cl <- cluster_louvain(mat, k = 10)
  expect_type(cl, "character")
  expect_length(cl, nrow(mat))
})

test_that("cluster_louvain finds multiple clusters for separated data", {
  set.seed(42)
  mat <- matrix(
    c(rnorm(50, -10), rnorm(50, 10), rnorm(100, 0)),
    ncol = 2,
    dimnames = list(NULL, c("v1", "v2"))
  )
  cl <- cluster_louvain(mat, k = 10)
  expect_true(length(unique(cl)) >= 2L)
})

test_that("cluster_louvain works with data.frame input", {
  set.seed(1)
  df <- data.frame(
    v1 = c(rnorm(30, -3), rnorm(30, 3)),
    v2 = c(rnorm(30, 0), rnorm(30, 0))
  )
  cl <- cluster_louvain(df, k = 10)
  expect_type(cl, "character")
  expect_length(cl, nrow(df))
})

test_that("cluster_louvain respects vars argument", {
  set.seed(1)
  mat <- matrix(
    c(rnorm(60, -3), rnorm(60, 3), rnorm(60, 0)),
    ncol = 3,
    dimnames = list(NULL, c("v1", "v2", "v3"))
  )
  cl <- cluster_louvain(mat, vars = c("v1", "v2"), k = 10)
  expect_length(cl, nrow(mat))
})

test_that("cluster_louvain errors with fewer than 2 rows", {
  mat <- matrix(1:2, nrow = 1, dimnames = list(NULL, c("v1", "v2")))
  expect_error(cluster_louvain(mat), "at least 2 rows")
})

test_that("cluster_louvain errors with invalid vars", {
  mat <- matrix(1:4, nrow = 2, dimnames = list(NULL, c("v1", "v2")))
  expect_error(cluster_louvain(mat, vars = "bad"), "column names")
})

test_that("cluster_louvain errors with invalid k", {
  mat <- matrix(1:4, nrow = 2, dimnames = list(NULL, c("v1", "v2")))
  expect_error(cluster_louvain(mat, k = -1), "positive integer")
})

test_that("cluster_louvain errors with invalid resolution", {
  mat <- matrix(1:4, nrow = 2, dimnames = list(NULL, c("v1", "v2")))
  expect_error(cluster_louvain(mat, resolution = 0), "positive number")
})

test_that("cluster_louvain output works with cluster_merge_bin", {
  set.seed(1)
  mat <- matrix(
    c(rnorm(50, -3), rnorm(50, 3), rnorm(100, 0)),
    ncol = 2,
    dimnames = list(NULL, c("v1", "v2"))
  )
  cl <- cluster_louvain(mat, k = 10)
  result <- cluster_merge_bin(mat, cl, list(v1 = 0, v2 = 0))
  expect_type(result, "list")
  expect_named(result, c("assign", "label"))
  expect_equal(nrow(result$assign), nrow(mat))
})

test_that("cluster_louvain_merge returns correct structure", {
  set.seed(1)
  mat <- matrix(
    c(rnorm(50, -3), rnorm(50, 3), rnorm(100, 0)),
    ncol = 2,
    dimnames = list(NULL, c("v1", "v2"))
  )
  result <- cluster_louvain_merge(
    mat,
    vars = c("v1", "v2"),
    thresholds = list(v1 = 0, v2 = 0),
    k = 10
  )
  expect_type(result, "list")
  expect_named(result, c("assign", "label"))
  expect_s3_class(result$assign, "tbl_df")
  expect_named(result$assign, c("orig", "merged"))
  expect_equal(nrow(result$assign), nrow(mat))
  expect_false(is.null(attr(result, "thresholds")))
})

test_that("cluster_louvain_merge reduces cluster count", {
  set.seed(1)
  mat <- matrix(
    c(rnorm(50, -3), rnorm(50, 3), rnorm(100, 0)),
    ncol = 2,
    dimnames = list(NULL, c("v1", "v2"))
  )
  cl <- cluster_louvain(mat, k = 10)
  result <- cluster_louvain_merge(
    mat,
    vars = c("v1", "v2"),
    thresholds = list(v1 = 0, v2 = 0),
    k = 10
  )
  # Merged result should have fewer or equal clusters than raw Louvain
  expect_true(
    length(unique(result$assign$merged)) <= length(unique(cl))
  )
})

test_that("cluster_louvain_merge works with data.frame", {
  set.seed(1)
  df <- data.frame(
    v1 = c(rnorm(30, -3), rnorm(30, 3)),
    v2 = c(rnorm(30, 0), rnorm(30, 0))
  )
  result <- cluster_louvain_merge(
    df,
    thresholds = list(v1 = 0, v2 = 0),
    k = 10
  )
  expect_s3_class(result$assign, "tbl_df")
  expect_equal(nrow(result$assign), nrow(df))
})

test_that("cluster_louvain_merge passes merge params through", {
  set.seed(1)
  mat <- matrix(
    c(rnorm(50, -3), rnorm(50, 3), rnorm(100, 0)),
    ncol = 2,
    dimnames = list(NULL, c("v1", "v2"))
  )
  result <- cluster_louvain_merge(
    mat,
    thresholds = list(v1 = 0, v2 = 0),
    k = 10,
    max_label_diff = 0L
  )
  expect_type(result, "list")
  expect_equal(nrow(result$assign), nrow(mat))
})
