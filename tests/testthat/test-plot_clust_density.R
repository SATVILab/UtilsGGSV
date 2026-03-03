test_that("plot_clust_density returns a ggplot object", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(paste0("C", 1:3), each = 20),
    var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2)),
    var2 = c(rnorm(20, -1), rnorm(20, 1), rnorm(20, 0))
  )
  p <- plot_clust_density(data, cluster = "cluster")
  expect_s3_class(p, "ggplot")
})

test_that("plot_clust_density has a density layer", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(paste0("C", 1:3), each = 20),
    var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2)),
    var2 = c(rnorm(20, -1), rnorm(20, 1), rnorm(20, 0))
  )
  p <- plot_clust_density(data, cluster = "cluster")
  has_density <- any(sapply(p$layers, function(l) inherits(l$geom, "GeomDensity")))
  expect_true(has_density)
})

test_that("plot_clust_density has a vline layer", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(paste0("C", 1:3), each = 20),
    var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2)),
    var2 = c(rnorm(20, -1), rnorm(20, 1), rnorm(20, 0))
  )
  p <- plot_clust_density(data, cluster = "cluster")
  has_vline <- any(sapply(p$layers, function(l) inherits(l$geom, "GeomVline")))
  expect_true(has_vline)
})

test_that("plot_clust_density respects vars argument", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(paste0("C", 1:3), each = 20),
    var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2)),
    var2 = c(rnorm(20, -1), rnorm(20, 1), rnorm(20, 0)),
    var3 = rnorm(60)
  )
  p <- plot_clust_density(data, cluster = "cluster", vars = c("var1", "var2"))
  expect_equal(length(unique(p$data$variable)), 2L)
  expect_true(all(unique(p$data$variable) %in% c("var1", "var2")))
})

test_that("plot_clust_density uses all non-cluster columns when vars is NULL", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(paste0("C", 1:3), each = 20),
    var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2)),
    var2 = c(rnorm(20, -1), rnorm(20, 1), rnorm(20, 0)),
    var3 = rnorm(60)
  )
  p <- plot_clust_density(data, cluster = "cluster")
  expect_equal(
    sort(unique(p$data$variable)),
    sort(c("var1", "var2", "var3"))
  )
})

test_that("plot_clust_density NULL theme applies no theme", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(paste0("C", 1:3), each = 20),
    var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2)),
    var2 = c(rnorm(20, -1), rnorm(20, 1), rnorm(20, 0))
  )
  expect_s3_class(
    plot_clust_density(data, cluster = "cluster", thm = NULL),
    "ggplot"
  )
})

test_that("plot_clust_density NULL grid applies no grid", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(paste0("C", 1:3), each = 20),
    var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2)),
    var2 = c(rnorm(20, -1), rnorm(20, 1), rnorm(20, 0))
  )
  expect_s3_class(
    plot_clust_density(data, cluster = "cluster", grid = NULL),
    "ggplot"
  )
})

test_that("plot_clust_density vline data contains one row per cluster per variable", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(paste0("C", 1:3), each = 20),
    var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2)),
    var2 = c(rnorm(20, -1), rnorm(20, 1), rnorm(20, 0))
  )
  p <- plot_clust_density(data, cluster = "cluster")
  vline_layer <- p$layers[sapply(p$layers, function(l) inherits(l$geom, "GeomVline"))][[1]]
  vline_data <- vline_layer$data
  expect_equal(nrow(vline_data), 6L)
  expect_equal(length(unique(vline_data$cluster)), 3L)
  expect_equal(length(unique(vline_data$variable)), 2L)
})
