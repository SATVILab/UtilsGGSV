test_that("plot_group_heatmap returns a ggplot object", {
  set.seed(1)
  data <- data.frame(
    grp = rep(paste0("G", 1:3), each = 20),
    var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2)),
    var2 = c(rnorm(20, -1), rnorm(20, 1), rnorm(20, 0))
  )
  p <- plot_group_heatmap(data, group = "grp")
  expect_s3_class(p, "ggplot")
})

test_that("plot_group_heatmap uses geom_raster layer", {
  set.seed(1)
  data <- data.frame(
    grp = rep(paste0("G", 1:3), each = 20),
    var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2)),
    var2 = c(rnorm(20, -1), rnorm(20, 1), rnorm(20, 0))
  )
  p <- plot_group_heatmap(data, group = "grp")
  has_raster <- any(sapply(p$layers, function(l) inherits(l$geom, "GeomRaster")))
  expect_true(has_raster)
})

test_that("plot_group_heatmap x-axis label is Group", {
  set.seed(1)
  data <- data.frame(
    grp = rep(paste0("G", 1:3), each = 20),
    var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2)),
    var2 = c(rnorm(20, -1), rnorm(20, 1), rnorm(20, 0))
  )
  p <- plot_group_heatmap(data, group = "grp")
  expect_equal(p$labels$x, "Group")
})

test_that("plot_cluster_heatmap is an alias for plot_group_heatmap", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(paste0("C", 1:3), each = 20),
    var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2)),
    var2 = c(rnorm(20, -1), rnorm(20, 1), rnorm(20, 0))
  )
  p <- plot_cluster_heatmap(data, cluster = "cluster")
  expect_s3_class(p, "ggplot")
  has_raster <- any(sapply(p$layers, function(l) inherits(l$geom, "GeomRaster")))
  expect_true(has_raster)
})
