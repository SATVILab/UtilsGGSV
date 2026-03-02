test_that("plot_heatmap_cluster returns a ggplot object", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(paste0("C", 1:3), each = 20),
    var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2)),
    var2 = c(rnorm(20, -1), rnorm(20, 1), rnorm(20, 0))
  )
  p <- plot_heatmap_cluster(data, cluster = "cluster")
  expect_s3_class(p, "ggplot")
})

test_that("plot_heatmap_cluster uses geom_raster layer", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(paste0("C", 1:3), each = 20),
    var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2)),
    var2 = c(rnorm(20, -1), rnorm(20, 1), rnorm(20, 0))
  )
  p <- plot_heatmap_cluster(data, cluster = "cluster")
  has_raster <- any(sapply(p$layers, function(l) inherits(l$geom, "GeomRaster")))
  expect_true(has_raster)
})

test_that("plot_heatmap_cluster respects vars argument", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(paste0("C", 1:3), each = 20),
    var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2)),
    var2 = c(rnorm(20, -1), rnorm(20, 1), rnorm(20, 0)),
    var3 = rnorm(60)
  )
  p <- plot_heatmap_cluster(data, cluster = "cluster", vars = c("var1", "var2"))
  expect_equal(length(unique(p$data$variable)), 2L)
  expect_true(all(unique(as.character(p$data$variable)) %in% c("var1", "var2")))
})

test_that("plot_heatmap_cluster uses all non-cluster columns when vars is NULL", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(paste0("C", 1:3), each = 20),
    var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2)),
    var2 = c(rnorm(20, -1), rnorm(20, 1), rnorm(20, 0)),
    var3 = rnorm(60)
  )
  p <- plot_heatmap_cluster(data, cluster = "cluster")
  expect_equal(
    sort(unique(as.character(p$data$variable))),
    sort(c("var1", "var2", "var3"))
  )
})

test_that("plot_heatmap_cluster NULL theme applies no theme", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(paste0("C", 1:3), each = 20),
    var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2)),
    var2 = c(rnorm(20, -1), rnorm(20, 1), rnorm(20, 0))
  )
  expect_s3_class(
    plot_heatmap_cluster(data, cluster = "cluster", thm = NULL),
    "ggplot"
  )
})

test_that("plot_heatmap_cluster NULL grid applies no grid", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(paste0("C", 1:3), each = 20),
    var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2)),
    var2 = c(rnorm(20, -1), rnorm(20, 1), rnorm(20, 0))
  )
  expect_s3_class(
    plot_heatmap_cluster(data, cluster = "cluster", grid = NULL),
    "ggplot"
  )
})

test_that("plot_heatmap_cluster perc values are between 0 and 1", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(paste0("C", 1:3), each = 20),
    var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2)),
    var2 = c(rnorm(20, -1), rnorm(20, 1), rnorm(20, 0))
  )
  p <- plot_heatmap_cluster(data, cluster = "cluster")
  expect_true(all(p$data$perc >= 0 & p$data$perc <= 1))
})

test_that("plot_heatmap_cluster works with two clusters", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(c("A", "B"), each = 15),
    var1 = c(rnorm(15, 2), rnorm(15, -2)),
    var2 = c(rnorm(15, 0), rnorm(15, 1))
  )
  p <- plot_heatmap_cluster(data, cluster = "cluster")
  expect_s3_class(p, "ggplot")
  expect_equal(length(unique(p$data$cluster)), 2L)
})

test_that("plot_heatmap_cluster custom colours are applied to the scale", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(paste0("C", 1:3), each = 20),
    var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2)),
    var2 = c(rnorm(20, -1), rnorm(20, 1), rnorm(20, 0))
  )
  p <- plot_heatmap_cluster(
    data,
    cluster = "cluster",
    col_high = "#FF0000",
    col_mid = "#FFFFFF",
    col_low = "#0000FF"
  )
  expect_s3_class(p, "ggplot")
  fill_scale <- p$scales$get_scales("fill")
  expect_equal(fill_scale$palette(0), "#0000FF")
  expect_equal(fill_scale$palette(1), "#FF0000")
  expect_equal(fill_scale$palette(0.5), "#FFFFFF")
})

test_that("plot_heatmap_cluster custom white_range is applied to the scale", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(paste0("C", 1:3), each = 20),
    var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2)),
    var2 = c(rnorm(20, -1), rnorm(20, 1), rnorm(20, 0))
  )
  # With white_range c(0.3, 0.7), the value 0.35 falls inside the white zone
  p_wide <- plot_heatmap_cluster(
    data,
    cluster = "cluster",
    white_range = c(0.3, 0.7)
  )
  # With default white_range c(0.4, 0.6), the value 0.35 falls outside the white zone
  p_default <- plot_heatmap_cluster(data, cluster = "cluster")
  sc_wide <- p_wide$scales$get_scales("fill")
  sc_default <- p_default$scales$get_scales("fill")
  expect_equal(sc_wide$palette(0.35), "#F7F7F7")
  expect_false(sc_default$palette(0.35) == "#F7F7F7")
})
