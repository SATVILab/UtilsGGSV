test_that("cluster_som returns character vector of correct length", {
  set.seed(1)
  mat <- matrix(
    c(rnorm(50, -3), rnorm(50, 3), rnorm(100, 0)),
    ncol = 2,
    dimnames = list(NULL, c("v1", "v2"))
  )
  cl <- cluster_som(mat, x_dim = 3, y_dim = 3)
  expect_type(cl, "character")
  expect_length(cl, nrow(mat))
})

test_that("cluster_som produces multiple clusters", {
  set.seed(1)
  mat <- matrix(
    c(rnorm(50, -10), rnorm(50, 10), rnorm(100, 0)),
    ncol = 2,
    dimnames = list(NULL, c("v1", "v2"))
  )
  cl <- cluster_som(mat, x_dim = 3, y_dim = 3)
  expect_true(length(unique(cl)) >= 2L)
})

test_that("cluster_som works with data.frame input", {
  set.seed(1)
  df <- data.frame(
    v1 = c(rnorm(30, -3), rnorm(30, 3)),
    v2 = c(rnorm(30, 0), rnorm(30, 0))
  )
  cl <- cluster_som(df, x_dim = 3, y_dim = 3)
  expect_type(cl, "character")
  expect_length(cl, nrow(df))
})

test_that("cluster_som respects vars argument", {
  set.seed(1)
  mat <- matrix(
    c(rnorm(60, -3), rnorm(60, 3), rnorm(60, 0)),
    ncol = 3,
    dimnames = list(NULL, c("v1", "v2", "v3"))
  )
  cl <- cluster_som(mat, vars = c("v1", "v2"), x_dim = 3, y_dim = 3)
  expect_length(cl, nrow(mat))
})

test_that("cluster_som errors with fewer than 2 rows", {
  mat <- matrix(1:2, nrow = 1, dimnames = list(NULL, c("v1", "v2")))
  expect_error(cluster_som(mat), "at least 2 rows")
})

test_that("cluster_som errors with invalid vars", {
  mat <- matrix(1:4, nrow = 2, dimnames = list(NULL, c("v1", "v2")))
  expect_error(cluster_som(mat, vars = "bad"), "column names")
})

test_that("cluster_som errors with invalid x_dim", {
  mat <- matrix(1:4, nrow = 2, dimnames = list(NULL, c("v1", "v2")))
  expect_error(cluster_som(mat, x_dim = -1), "positive integer")
})

test_that("cluster_som errors with invalid y_dim", {
  mat <- matrix(1:4, nrow = 2, dimnames = list(NULL, c("v1", "v2")))
  expect_error(cluster_som(mat, y_dim = 0), "positive integer")
})

test_that("cluster_som output works with cluster_merge_bin", {
  set.seed(1)
  mat <- matrix(
    c(rnorm(50, -3), rnorm(50, 3), rnorm(100, 0)),
    ncol = 2,
    dimnames = list(NULL, c("v1", "v2"))
  )
  cl <- cluster_som(mat, x_dim = 3, y_dim = 3)
  result <- cluster_merge_bin(mat, cl, list(v1 = 0, v2 = 0))
  expect_type(result, "list")
  expect_named(result, c("assign", "label"))
  expect_equal(nrow(result$assign), nrow(mat))
})

test_that("cluster_som_merge returns correct structure", {
  set.seed(1)
  mat <- matrix(
    c(rnorm(50, -3), rnorm(50, 3), rnorm(100, 0)),
    ncol = 2,
    dimnames = list(NULL, c("v1", "v2"))
  )
  result <- cluster_som_merge(
    mat,
    vars = c("v1", "v2"),
    thresholds = list(v1 = 0, v2 = 0),
    x_dim = 3, y_dim = 3
  )
  expect_type(result, "list")
  expect_named(result, c("assign", "label"))
  expect_s3_class(result$assign, "tbl_df")
  expect_named(result$assign, c("orig", "merged"))
  expect_equal(nrow(result$assign), nrow(mat))
  expect_false(is.null(attr(result, "thresholds")))
})

test_that("cluster_som_merge reduces cluster count", {
  set.seed(1)
  mat <- matrix(
    c(rnorm(50, -3), rnorm(50, 3), rnorm(100, 0)),
    ncol = 2,
    dimnames = list(NULL, c("v1", "v2"))
  )
  cl <- cluster_som(mat, x_dim = 3, y_dim = 3)
  result <- cluster_som_merge(
    mat,
    vars = c("v1", "v2"),
    thresholds = list(v1 = 0, v2 = 0),
    x_dim = 3, y_dim = 3
  )
  expect_true(
    length(unique(result$assign$merged)) <= length(unique(cl))
  )
})

test_that("cluster_som_merge works with data.frame", {
  set.seed(1)
  df <- data.frame(
    v1 = c(rnorm(30, -3), rnorm(30, 3)),
    v2 = c(rnorm(30, 0), rnorm(30, 0))
  )
  result <- cluster_som_merge(
    df,
    thresholds = list(v1 = 0, v2 = 0),
    x_dim = 3, y_dim = 3
  )
  expect_s3_class(result$assign, "tbl_df")
  expect_equal(nrow(result$assign), nrow(df))
})

test_that("cluster_som_merge passes merge params through", {
  set.seed(1)
  mat <- matrix(
    c(rnorm(50, -3), rnorm(50, 3), rnorm(100, 0)),
    ncol = 2,
    dimnames = list(NULL, c("v1", "v2"))
  )
  result <- cluster_som_merge(
    mat,
    thresholds = list(v1 = 0, v2 = 0),
    x_dim = 3, y_dim = 3,
    max_label_diff = 0L
  )
  expect_type(result, "list")
  expect_equal(nrow(result$assign), nrow(mat))
})

test_that("cluster_som accepts hexagonal topology", {
  set.seed(1)
  mat <- matrix(
    c(rnorm(50, -3), rnorm(50, 3), rnorm(100, 0)),
    ncol = 2,
    dimnames = list(NULL, c("v1", "v2"))
  )
  cl <- cluster_som(mat, x_dim = 3, y_dim = 3, topo = "hexagonal")
  expect_type(cl, "character")
  expect_length(cl, nrow(mat))
})
