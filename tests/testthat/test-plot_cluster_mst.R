test_that("plot_cluster_mst returns a ggplot object", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(paste0("C", 1:3), each = 20),
    var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2)),
    var2 = c(rnorm(20, -1), rnorm(20, 1), rnorm(20, 0))
  )
  p <- plot_cluster_mst(data, cluster = "cluster")
  expect_s3_class(p, "ggplot")
})

test_that("plot_cluster_mst has segment and point layers", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(paste0("C", 1:3), each = 20),
    var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2)),
    var2 = c(rnorm(20, -1), rnorm(20, 1), rnorm(20, 0))
  )
  p <- plot_cluster_mst(data, cluster = "cluster")
  has_segment <- any(sapply(p$layers, function(l) inherits(l$geom, "GeomSegment")))
  has_point <- any(sapply(p$layers, function(l) inherits(l$geom, "GeomPoint")))
  expect_true(has_segment)
  expect_true(has_point)
})

test_that("plot_cluster_mst MST has n_clusters - 1 edges", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(paste0("C", 1:4), each = 20),
    var1 = c(rnorm(20, 3), rnorm(20, 1), rnorm(20, -1), rnorm(20, -3)),
    var2 = rnorm(80)
  )
  p <- plot_cluster_mst(data, cluster = "cluster")
  seg_layer <- p$layers[sapply(p$layers, function(l) inherits(l$geom, "GeomSegment"))][[1]]
  expect_equal(nrow(seg_layer$data), 3L)
})

test_that("plot_cluster_mst errors with fewer than two clusters", {
  data <- data.frame(
    cluster = rep("C1", 10),
    var1 = rnorm(10)
  )
  expect_error(
    plot_cluster_mst(data, cluster = "cluster"),
    "At least two clusters"
  )
})

test_that("plot_cluster_mst works with exactly two clusters", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(c("A", "B"), each = 15),
    var1 = c(rnorm(15, 2), rnorm(15, -2)),
    var2 = c(rnorm(15, 0), rnorm(15, 1))
  )
  p <- plot_cluster_mst(data, cluster = "cluster")
  expect_s3_class(p, "ggplot")
  seg_layer <- p$layers[sapply(p$layers, function(l) inherits(l$geom, "GeomSegment"))][[1]]
  expect_equal(nrow(seg_layer$data), 1L)
})

test_that("plot_cluster_mst respects vars argument", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(paste0("C", 1:3), each = 20),
    var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2)),
    var2 = c(rnorm(20, -1), rnorm(20, 1), rnorm(20, 0)),
    var3 = rnorm(60)
  )
  p1 <- plot_cluster_mst(data, cluster = "cluster", vars = c("var1", "var2"))
  p2 <- plot_cluster_mst(data, cluster = "cluster", vars = c("var1", "var3"))
  expect_s3_class(p1, "ggplot")
  expect_s3_class(p2, "ggplot")
})

test_that("plot_cluster_mst uses all non-cluster columns when vars is NULL", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(paste0("C", 1:3), each = 20),
    var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2)),
    var2 = c(rnorm(20, -1), rnorm(20, 1), rnorm(20, 0))
  )
  p <- plot_cluster_mst(data, cluster = "cluster")
  expect_s3_class(p, "ggplot")
})

test_that("plot_cluster_mst col_clusters applies colour scale", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(paste0("C", 1:3), each = 20),
    var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2)),
    var2 = c(rnorm(20, -1), rnorm(20, 1), rnorm(20, 0))
  )
  cols <- c(C1 = "#FF0000", C2 = "#00FF00", C3 = "#0000FF")
  p <- plot_cluster_mst(data, cluster = "cluster", col_clusters = cols)
  expect_s3_class(p, "ggplot")
  colour_scale <- p$scales$get_scales("colour")
  expect_false(is.null(colour_scale))
})

test_that("plot_cluster_mst NULL theme applies no theme", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(paste0("C", 1:3), each = 20),
    var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2)),
    var2 = c(rnorm(20, -1), rnorm(20, 1), rnorm(20, 0))
  )
  p <- plot_cluster_mst(data, cluster = "cluster", thm = NULL)
  expect_s3_class(p, "ggplot")
})

test_that("plot_cluster_mst NULL grid applies no grid", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(paste0("C", 1:3), each = 20),
    var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2)),
    var2 = c(rnorm(20, -1), rnorm(20, 1), rnorm(20, 0))
  )
  p <- plot_cluster_mst(data, cluster = "cluster", grid = NULL)
  expect_s3_class(p, "ggplot")
})

test_that("plot_cluster_density col_clusters applies colour scale (list mode)", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(paste0("C", 1:3), each = 20),
    var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2)),
    var2 = c(rnorm(20, -1), rnorm(20, 1), rnorm(20, 0))
  )
  cols <- c(C1 = "#FF0000", C2 = "#00FF00", C3 = "#0000FF")
  result <- plot_cluster_density(
    data, cluster = "cluster", col_clusters = cols
  )
  for (p in result) {
    colour_scale <- p$scales$get_scales("colour")
    expect_false(is.null(colour_scale))
  }
})

test_that("plot_cluster_density col_clusters applies colour scale (facet mode)", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(paste0("C", 1:3), each = 20),
    var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2)),
    var2 = c(rnorm(20, -1), rnorm(20, 1), rnorm(20, 0))
  )
  cols <- c(C1 = "#FF0000", C2 = "#00FF00", C3 = "#0000FF")
  p <- plot_cluster_density(
    data, cluster = "cluster", n_col = 2, col_clusters = cols
  )
  colour_scale <- p$scales$get_scales("colour")
  expect_false(is.null(colour_scale))
})
