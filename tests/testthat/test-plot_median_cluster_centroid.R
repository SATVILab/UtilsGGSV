test_that("plot_median_cluster_centroid creates ggplot with PCA method", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(paste0("C", 1:3), each = 20),
    var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2)),
    var2 = c(rnorm(20, -1), rnorm(20, 1), rnorm(20, 0)),
    var3 = c(rnorm(20, 1), rnorm(20, -1), rnorm(20, 0))
  )

  result <- plot_median_cluster_centroid(data, cluster = "cluster")

  expect_s3_class(result, "ggplot")
})

test_that("plot_median_cluster_centroid creates ggplot with raw method", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(paste0("C", 1:3), each = 20),
    var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2)),
    var2 = c(rnorm(20, -1), rnorm(20, 1), rnorm(20, 0)),
    var3 = c(rnorm(20, 1), rnorm(20, -1), rnorm(20, 0))
  )

  result <- plot_median_cluster_centroid(
    data,
    cluster = "cluster",
    method = "raw"
  )

  expect_s3_class(result, "ggplot")
})

test_that("plot_median_cluster_centroid works with subset of variables", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(paste0("C", 1:3), each = 20),
    var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2)),
    var2 = c(rnorm(20, -1), rnorm(20, 1), rnorm(20, 0)),
    var3 = c(rnorm(20, 1), rnorm(20, -1), rnorm(20, 0))
  )

  result <- plot_median_cluster_centroid(
    data,
    cluster = "cluster",
    vars = c("var1", "var2")
  )

  expect_s3_class(result, "ggplot")
})

test_that("plot_median_cluster_centroid applies custom colors", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(paste0("C", 1:3), each = 20),
    var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2)),
    var2 = c(rnorm(20, -1), rnorm(20, 1), rnorm(20, 0)),
    var3 = c(rnorm(20, 1), rnorm(20, -1), rnorm(20, 0))
  )

  colors <- c("C1" = "red", "C2" = "blue", "C3" = "green")
  result <- plot_median_cluster_centroid(
    data,
    cluster = "cluster",
    col_clusters = colors
  )

  expect_s3_class(result, "ggplot")
})

test_that("plot_median_cluster_centroid errors with insufficient variables for PCA", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(paste0("C", 1:2), each = 10),
    var1 = rnorm(20)
  )

  expect_error(
    plot_median_cluster_centroid(data, cluster = "cluster", method = "pca"),
    "requires at least 2 variables"
  )
})

test_that("plot_median_cluster_centroid errors with insufficient variables for raw", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(paste0("C", 1:2), each = 10),
    var1 = rnorm(20)
  )

  expect_error(
    plot_median_cluster_centroid(data, cluster = "cluster", method = "raw"),
    "requires at least 2 variables"
  )
})

test_that("plot_median_cluster_centroid handles missing values", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(paste0("C", 1:3), each = 20),
    var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2)),
    var2 = c(rnorm(20, -1), NA, rnorm(19, 1), rnorm(20, 0)),
    var3 = c(rnorm(20, 1), rnorm(20, -1), rnorm(20, 0))
  )

  result <- plot_median_cluster_centroid(data, cluster = "cluster")

  expect_s3_class(result, "ggplot")
})

test_that("plot_median_cluster_centroid works with custom point sizes", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(paste0("C", 1:3), each = 20),
    var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2)),
    var2 = c(rnorm(20, -1), rnorm(20, 1), rnorm(20, 0)),
    var3 = c(rnorm(20, 1), rnorm(20, -1), rnorm(20, 0))
  )

  result <- plot_median_cluster_centroid(
    data,
    cluster = "cluster",
    point_size = 3,
    centroid_size = 7
  )

  expect_s3_class(result, "ggplot")
})

test_that("plot_median_cluster_centroid removes theme when thm = NULL", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(paste0("C", 1:3), each = 20),
    var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2)),
    var2 = c(rnorm(20, -1), rnorm(20, 1), rnorm(20, 0)),
    var3 = c(rnorm(20, 1), rnorm(20, -1), rnorm(20, 0))
  )

  result <- plot_median_cluster_centroid(
    data,
    cluster = "cluster",
    thm = NULL,
    grid = NULL
  )

  expect_s3_class(result, "ggplot")
})

test_that("plot_median_cluster_centroid works with two clusters", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(paste0("C", 1:2), each = 30),
    var1 = c(rnorm(30, 2), rnorm(30, -2)),
    var2 = c(rnorm(30, -1), rnorm(30, 1)),
    var3 = c(rnorm(30, 1), rnorm(30, -1))
  )

  result <- plot_median_cluster_centroid(data, cluster = "cluster")

  expect_s3_class(result, "ggplot")
})

test_that("plot_median_cluster_centroid works with many clusters", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(paste0("C", 1:5), each = 20),
    var1 = rep(c(2, 1, 0, -1, -2), each = 20) + rnorm(100),
    var2 = rep(c(-2, -1, 0, 1, 2), each = 20) + rnorm(100),
    var3 = rep(c(1, 0.5, 0, -0.5, -1), each = 20) + rnorm(100)
  )

  result <- plot_median_cluster_centroid(data, cluster = "cluster")

  expect_s3_class(result, "ggplot")
})
