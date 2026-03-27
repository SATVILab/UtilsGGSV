test_that("plot_cluster_scatter creates ggplot with default (pca) method", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(paste0("C", 1:3), each = 20),
    var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2)),
    var2 = c(rnorm(20, -1), rnorm(20, 1), rnorm(20, 0)),
    var3 = c(rnorm(20, 1), rnorm(20, -1), rnorm(20, 0))
  )

  result <- plot_cluster_scatter(data, cluster = "cluster")

  expect_s3_class(result, "ggplot")
})

test_that("plot_cluster_scatter creates ggplot with dim_red = 'none'", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(paste0("C", 1:3), each = 20),
    var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2)),
    var2 = c(rnorm(20, -1), rnorm(20, 1), rnorm(20, 0)),
    var3 = c(rnorm(20, 1), rnorm(20, -1), rnorm(20, 0))
  )

  result <- plot_cluster_scatter(
    data,
    cluster = "cluster",
    dim_red = "none",
    vars = c("var1", "var2")
  )

  expect_s3_class(result, "ggplot")
})

test_that("plot_cluster_scatter creates ggplot with dim_red = 'pca'", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(paste0("C", 1:3), each = 20),
    var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2)),
    var2 = c(rnorm(20, -1), rnorm(20, 1), rnorm(20, 0)),
    var3 = c(rnorm(20, 1), rnorm(20, -1), rnorm(20, 0))
  )

  result <- plot_cluster_scatter(data, cluster = "cluster", dim_red = "pca")

  expect_s3_class(result, "ggplot")
})

test_that("plot_cluster_scatter creates ggplot with dim_red = 'tsne'", {
  skip_if_not_installed("Rtsne")

  set.seed(1)
  data <- data.frame(
    cluster = rep(paste0("C", 1:3), each = 20),
    var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2)),
    var2 = c(rnorm(20, -1), rnorm(20, 1), rnorm(20, 0)),
    var3 = c(rnorm(20, 1), rnorm(20, -1), rnorm(20, 0))
  )

  result <- plot_cluster_scatter(data, cluster = "cluster", dim_red = "tsne")

  expect_s3_class(result, "ggplot")
})

test_that("plot_cluster_scatter creates ggplot with dim_red = 'umap'", {
  skip_if_not_installed("umap")

  set.seed(1)
  data <- data.frame(
    cluster = rep(paste0("C", 1:3), each = 20),
    var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2)),
    var2 = c(rnorm(20, -1), rnorm(20, 1), rnorm(20, 0)),
    var3 = c(rnorm(20, 1), rnorm(20, -1), rnorm(20, 0))
  )

  result <- plot_cluster_scatter(data, cluster = "cluster", dim_red = "umap")

  expect_s3_class(result, "ggplot")
})

test_that("plot_cluster_scatter works with a subset of variables", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(paste0("C", 1:3), each = 20),
    var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2)),
    var2 = c(rnorm(20, -1), rnorm(20, 1), rnorm(20, 0)),
    var3 = c(rnorm(20, 1), rnorm(20, -1), rnorm(20, 0))
  )

  result <- plot_cluster_scatter(
    data,
    cluster = "cluster",
    vars = c("var1", "var2")
  )

  expect_s3_class(result, "ggplot")
})

test_that("plot_cluster_scatter applies custom colours", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(paste0("C", 1:3), each = 20),
    var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2)),
    var2 = c(rnorm(20, -1), rnorm(20, 1), rnorm(20, 0)),
    var3 = c(rnorm(20, 1), rnorm(20, -1), rnorm(20, 0))
  )

  colors <- c("C1" = "red", "C2" = "blue", "C3" = "green")
  result <- plot_cluster_scatter(
    data,
    cluster = "cluster",
    point_col = colors
  )

  expect_s3_class(result, "ggplot")
})

test_that("plot_cluster_scatter handles missing values", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(paste0("C", 1:3), each = 20),
    var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2)),
    var2 = c(rnorm(20, -1), NA, rnorm(19, 1), rnorm(20, 0)),
    var3 = c(rnorm(20, 1), rnorm(20, -1), rnorm(20, 0))
  )

  result <- plot_cluster_scatter(data, cluster = "cluster")

  expect_s3_class(result, "ggplot")
})

test_that("plot_cluster_scatter works with custom point_col_var", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(paste0("C", 1:3), each = 20),
    var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2)),
    var2 = c(rnorm(20, -1), rnorm(20, 1), rnorm(20, 0)),
    score = runif(60)
  )

  result <- plot_cluster_scatter(
    data,
    cluster = "cluster",
    point_col_var = "score",
    dim_red = "none",
    vars = c("var1", "var2")
  )

  expect_s3_class(result, "ggplot")
})

test_that("plot_cluster_scatter works with show_legend = FALSE", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(paste0("C", 1:3), each = 20),
    var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2)),
    var2 = c(rnorm(20, -1), rnorm(20, 1), rnorm(20, 0)),
    var3 = c(rnorm(20, 1), rnorm(20, -1), rnorm(20, 0))
  )

  result <- plot_cluster_scatter(
    data,
    cluster = "cluster",
    show_legend = FALSE
  )

  expect_s3_class(result, "ggplot")
})

test_that("plot_cluster_scatter works with custom point sizes", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(paste0("C", 1:3), each = 20),
    var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2)),
    var2 = c(rnorm(20, -1), rnorm(20, 1), rnorm(20, 0)),
    var3 = c(rnorm(20, 1), rnorm(20, -1), rnorm(20, 0))
  )

  result <- plot_cluster_scatter(
    data,
    cluster = "cluster",
    point_size = 3,
    centroid_size = 7
  )

  expect_s3_class(result, "ggplot")
})

test_that("plot_cluster_scatter removes theme when thm = NULL and grid = NULL", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(paste0("C", 1:3), each = 20),
    var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2)),
    var2 = c(rnorm(20, -1), rnorm(20, 1), rnorm(20, 0)),
    var3 = c(rnorm(20, 1), rnorm(20, -1), rnorm(20, 0))
  )

  result <- plot_cluster_scatter(
    data,
    cluster = "cluster",
    thm = NULL,
    grid = NULL
  )

  expect_s3_class(result, "ggplot")
})

test_that("plot_cluster_scatter works with two clusters", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(paste0("C", 1:2), each = 30),
    var1 = c(rnorm(30, 2), rnorm(30, -2)),
    var2 = c(rnorm(30, -1), rnorm(30, 1)),
    var3 = c(rnorm(30, 1), rnorm(30, -1))
  )

  result <- plot_cluster_scatter(data, cluster = "cluster")

  expect_s3_class(result, "ggplot")
})

test_that("plot_cluster_scatter works with many clusters", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(paste0("C", 1:5), each = 20),
    var1 = rep(c(2, 1, 0, -1, -2), each = 20) + rnorm(100),
    var2 = rep(c(-2, -1, 0, 1, 2), each = 20) + rnorm(100),
    var3 = rep(c(1, 0.5, 0, -0.5, -1), each = 20) + rnorm(100)
  )

  result <- plot_cluster_scatter(data, cluster = "cluster")

  expect_s3_class(result, "ggplot")
})

test_that("plot_cluster_scatter errors with insufficient variables for pca", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(paste0("C", 1:2), each = 10),
    var1 = rnorm(20)
  )

  expect_error(
    plot_cluster_scatter(data, cluster = "cluster", dim_red = "pca", vars = "var1"),
    "requires at least two variables"
  )
})

test_that("plot_cluster_scatter errors with insufficient variables for none", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(paste0("C", 1:2), each = 10),
    var1 = rnorm(20)
  )

  expect_error(
    plot_cluster_scatter(data, cluster = "cluster", dim_red = "none", vars = "var1"),
    "requires at least two variables"
  )
})

test_that("plot_cluster_scatter passes dim_red_args to prcomp (pca)", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(paste0("C", 1:3), each = 20),
    var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2)),
    var2 = c(rnorm(20, -1), rnorm(20, 1), rnorm(20, 0)),
    var3 = c(rnorm(20, 1), rnorm(20, -1), rnorm(20, 0))
  )

  result <- plot_cluster_scatter(
    data,
    cluster = "cluster",
    dim_red = "pca",
    dim_red_args = list(scale. = FALSE)
  )

  expect_s3_class(result, "ggplot")
})

test_that("plot_cluster_scatter passes dim_red_args to Rtsne (tsne)", {
  skip_if_not_installed("Rtsne")

  set.seed(1)
  data <- data.frame(
    cluster = rep(paste0("C", 1:3), each = 20),
    var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2)),
    var2 = c(rnorm(20, -1), rnorm(20, 1), rnorm(20, 0)),
    var3 = c(rnorm(20, 1), rnorm(20, -1), rnorm(20, 0))
  )

  result <- plot_cluster_scatter(
    data,
    cluster = "cluster",
    dim_red = "tsne",
    dim_red_args = list(perplexity = 5)
  )

  expect_s3_class(result, "ggplot")
})

test_that("plot_cluster_scatter passes dim_red_args to umap", {
  skip_if_not_installed("umap")

  set.seed(1)
  data <- data.frame(
    cluster = rep(paste0("C", 1:3), each = 20),
    var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2)),
    var2 = c(rnorm(20, -1), rnorm(20, 1), rnorm(20, 0)),
    var3 = c(rnorm(20, 1), rnorm(20, -1), rnorm(20, 0))
  )

  result <- plot_cluster_scatter(
    data,
    cluster = "cluster",
    dim_red = "umap",
    dim_red_args = list(config = umap::umap.defaults)
  )

  expect_s3_class(result, "ggplot")
})

test_that("plot_cluster_scatter errors when data is not a data.frame", {
  expect_error(
    plot_cluster_scatter(list(cluster = c("A", "B"), var1 = 1:2, var2 = 1:2),
                         cluster = "cluster"),
    "`\\.data` must be a data.frame"
  )
})

test_that("plot_cluster_scatter errors when cluster is not a character string", {
  data <- data.frame(
    cluster = rep(c("A", "B"), each = 10),
    var1 = rnorm(20),
    var2 = rnorm(20)
  )
  expect_error(
    plot_cluster_scatter(data, cluster = 1),
    "single non-NA character string"
  )
})

test_that("plot_cluster_scatter errors when cluster column is missing", {
  data <- data.frame(
    grp = rep(c("A", "B"), each = 10),
    var1 = rnorm(20),
    var2 = rnorm(20)
  )
  expect_error(
    plot_cluster_scatter(data, cluster = "cluster"),
    "not found in"
  )
})

test_that("plot_cluster_scatter errors when cluster column is numeric", {
  data <- data.frame(
    cluster = rep(c(1.0, 2.0), each = 10),
    var1 = rnorm(20),
    var2 = rnorm(20)
  )
  expect_error(
    plot_cluster_scatter(data, cluster = "cluster"),
    "numeric"
  )
})

test_that("plot_cluster_scatter errors when cluster column is integer", {
  data <- data.frame(
    cluster = rep(c(1L, 2L), each = 10),
    var1 = rnorm(20),
    var2 = rnorm(20)
  )
  expect_error(
    plot_cluster_scatter(data, cluster = "cluster"),
    "numeric"
  )
})

test_that("plot_cluster_scatter errors when cluster has only one unique value", {
  data <- data.frame(
    cluster = rep("A", 20),
    var1 = rnorm(20),
    var2 = rnorm(20)
  )
  expect_error(
    plot_cluster_scatter(data, cluster = "cluster"),
    "at least 2 unique"
  )
})

test_that("plot_cluster_scatter errors when vars column is missing", {
  data <- data.frame(
    cluster = rep(c("A", "B"), each = 10),
    var1 = rnorm(20),
    var2 = rnorm(20)
  )
  expect_error(
    plot_cluster_scatter(data, cluster = "cluster",
                         vars = c("var1", "var_missing")),
    "not found in"
  )
})

test_that("plot_cluster_scatter errors when vars column is not numeric", {
  data <- data.frame(
    cluster = rep(c("A", "B"), each = 10),
    var1 = rnorm(20),
    label = rep(c("x", "y"), 10)
  )
  expect_error(
    plot_cluster_scatter(data, cluster = "cluster",
                         vars = c("var1", "label"),
                         dim_red = "none"),
    "not numeric"
  )
})

test_that("plot_cluster_scatter errors when point_col_var column is missing", {
  data <- data.frame(
    cluster = rep(c("A", "B"), each = 10),
    var1 = rnorm(20),
    var2 = rnorm(20)
  )
  expect_error(
    plot_cluster_scatter(data, cluster = "cluster",
                         point_col_var = "no_such_col"),
    "not found"
  )
})

test_that("plot_cluster_scatter errors when ggrepel is not logical", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(c("A", "B"), each = 10),
    var1 = rnorm(20),
    var2 = rnorm(20)
  )
  expect_error(
    plot_cluster_scatter(data, cluster = "cluster", ggrepel = "yes"),
    "`ggrepel` must be TRUE or FALSE"
  )
})

test_that("plot_cluster_scatter errors when show_legend is not logical", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(c("A", "B"), each = 10),
    var1 = rnorm(20),
    var2 = rnorm(20)
  )
  expect_error(
    plot_cluster_scatter(data, cluster = "cluster", show_legend = "yes"),
    "`show_legend` must be TRUE or FALSE"
  )
})

test_that("plot_cluster_scatter errors when point_size is not a positive number", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(c("A", "B"), each = 10),
    var1 = rnorm(20),
    var2 = rnorm(20)
  )
  expect_error(
    plot_cluster_scatter(data, cluster = "cluster", point_size = -1),
    "`point_size` must be a single positive number"
  )
})

test_that("plot_cluster_scatter errors when point_alpha is out of range", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(c("A", "B"), each = 10),
    var1 = rnorm(20),
    var2 = rnorm(20)
  )
  expect_error(
    plot_cluster_scatter(data, cluster = "cluster", point_alpha = 1.5),
    "`point_alpha` must be a single number in"
  )
})

test_that("plot_cluster_scatter errors when dim_red_args is not a list", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(c("A", "B"), each = 10),
    var1 = rnorm(20),
    var2 = rnorm(20)
  )
  expect_error(
    plot_cluster_scatter(data, cluster = "cluster", dim_red_args = "bad"),
    "`dim_red_args` must be a list"
  )
})

test_that("plot_cluster_scatter works with factor cluster column", {
  set.seed(1)
  data <- data.frame(
    cluster = factor(rep(c("A", "B"), each = 20)),
    var1 = c(rnorm(20, 2), rnorm(20, -2)),
    var2 = c(rnorm(20, 1), rnorm(20, -1)),
    var3 = rnorm(40)
  )
  result <- plot_cluster_scatter(data, cluster = "cluster")
  expect_s3_class(result, "ggplot")
})
