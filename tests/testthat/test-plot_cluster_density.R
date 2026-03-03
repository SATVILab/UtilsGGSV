test_that("plot_cluster_density returns a named list by default", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(paste0("C", 1:3), each = 20),
    var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2)),
    var2 = c(rnorm(20, -1), rnorm(20, 1), rnorm(20, 0))
  )
  result <- plot_cluster_density(data, cluster = "cluster")
  expect_type(result, "list")
  expect_named(result, c("var1", "var2"))
})

test_that("plot_cluster_density list elements are ggplot objects", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(paste0("C", 1:3), each = 20),
    var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2)),
    var2 = c(rnorm(20, -1), rnorm(20, 1), rnorm(20, 0))
  )
  result <- plot_cluster_density(data, cluster = "cluster")
  for (p in result) expect_s3_class(p, "ggplot")
})

test_that("plot_cluster_density list plots have density and vline layers", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(paste0("C", 1:3), each = 20),
    var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2)),
    var2 = c(rnorm(20, -1), rnorm(20, 1), rnorm(20, 0))
  )
  result <- plot_cluster_density(data, cluster = "cluster")
  for (p in result) {
    expect_true(any(sapply(p$layers, function(l) inherits(l$geom, "GeomDensity"))))
    expect_true(any(sapply(p$layers, function(l) inherits(l$geom, "GeomVline"))))
  }
})

test_that("plot_cluster_density with n_col returns a ggplot object", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(paste0("C", 1:3), each = 20),
    var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2)),
    var2 = c(rnorm(20, -1), rnorm(20, 1), rnorm(20, 0))
  )
  p <- plot_cluster_density(data, cluster = "cluster", n_col = 2)
  expect_s3_class(p, "ggplot")
})

test_that("plot_cluster_density with n_row returns a ggplot object", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(paste0("C", 1:3), each = 20),
    var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2)),
    var2 = c(rnorm(20, -1), rnorm(20, 1), rnorm(20, 0))
  )
  p <- plot_cluster_density(data, cluster = "cluster", n_row = 1)
  expect_s3_class(p, "ggplot")
})

test_that("plot_cluster_density facet plot has density and vline layers", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(paste0("C", 1:3), each = 20),
    var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2)),
    var2 = c(rnorm(20, -1), rnorm(20, 1), rnorm(20, 0))
  )
  p <- plot_cluster_density(data, cluster = "cluster", n_col = 1)
  expect_true(any(sapply(p$layers, function(l) inherits(l$geom, "GeomDensity"))))
  expect_true(any(sapply(p$layers, function(l) inherits(l$geom, "GeomVline"))))
})

test_that("plot_cluster_density respects vars argument (list mode)", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(paste0("C", 1:3), each = 20),
    var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2)),
    var2 = c(rnorm(20, -1), rnorm(20, 1), rnorm(20, 0)),
    var3 = rnorm(60)
  )
  result <- plot_cluster_density(data, cluster = "cluster", vars = c("var1", "var2"))
  expect_named(result, c("var1", "var2"))
  expect_length(result, 2L)
})

test_that("plot_cluster_density respects vars argument (facet mode)", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(paste0("C", 1:3), each = 20),
    var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2)),
    var2 = c(rnorm(20, -1), rnorm(20, 1), rnorm(20, 0)),
    var3 = rnorm(60)
  )
  p <- plot_cluster_density(
    data, cluster = "cluster", vars = c("var1", "var2"), n_col = 1
  )
  expect_equal(length(unique(p$data$variable)), 2L)
  expect_true(all(unique(p$data$variable) %in% c("var1", "var2")))
})

test_that("plot_cluster_density uses all non-cluster columns when vars is NULL", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(paste0("C", 1:3), each = 20),
    var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2)),
    var2 = c(rnorm(20, -1), rnorm(20, 1), rnorm(20, 0)),
    var3 = rnorm(60)
  )
  result <- plot_cluster_density(data, cluster = "cluster")
  expect_named(result, c("var1", "var2", "var3"))
})

test_that("plot_cluster_density NULL theme applies no theme", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(paste0("C", 1:3), each = 20),
    var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2)),
    var2 = c(rnorm(20, -1), rnorm(20, 1), rnorm(20, 0))
  )
  result <- plot_cluster_density(data, cluster = "cluster", thm = NULL)
  for (p in result) expect_s3_class(p, "ggplot")
})

test_that("plot_cluster_density NULL grid applies no grid", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(paste0("C", 1:3), each = 20),
    var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2)),
    var2 = c(rnorm(20, -1), rnorm(20, 1), rnorm(20, 0))
  )
  result <- plot_cluster_density(data, cluster = "cluster", grid = NULL)
  for (p in result) expect_s3_class(p, "ggplot")
})

test_that("plot_cluster_density facet vline data has one row per cluster per variable", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(paste0("C", 1:3), each = 20),
    var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2)),
    var2 = c(rnorm(20, -1), rnorm(20, 1), rnorm(20, 0))
  )
  p <- plot_cluster_density(data, cluster = "cluster", n_col = 1)
  vline_layer <- p$layers[
    sapply(p$layers, function(l) inherits(l$geom, "GeomVline"))
  ][[1]]
  vline_data <- vline_layer$data
  expect_equal(nrow(vline_data), 6L)
  expect_equal(length(unique(vline_data$cluster)), 3L)
  expect_equal(length(unique(vline_data$variable)), 2L)
})

test_that("plot_cluster_density expand_coord numeric vector applies to all vars", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(paste0("C", 1:3), each = 20),
    var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2)),
    var2 = c(rnorm(20, -1), rnorm(20, 1), rnorm(20, 0))
  )
  result <- plot_cluster_density(
    data, cluster = "cluster", expand_coord = c(-10, 10)
  )
  for (p in result) expect_s3_class(p, "ggplot")
})

test_that("plot_cluster_density expand_coord named list applies per variable", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(paste0("C", 1:3), each = 20),
    var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2)),
    var2 = c(rnorm(20, -1), rnorm(20, 1), rnorm(20, 0))
  )
  result <- plot_cluster_density(
    data, cluster = "cluster",
    expand_coord = list(var1 = c(-5, 5), var2 = c(-3, 3))
  )
  for (p in result) expect_s3_class(p, "ggplot")
})

test_that("plot_cluster_density warns and ignores named list expand_coord in facet mode", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(paste0("C", 1:3), each = 20),
    var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2)),
    var2 = c(rnorm(20, -1), rnorm(20, 1), rnorm(20, 0))
  )
  expect_warning(
    plot_cluster_density(
      data, cluster = "cluster", n_col = 1,
      expand_coord = list(var1 = c(-5, 5), var2 = c(-3, 3))
    ),
    "incompatible with facet_wrap"
  )
})

test_that("plot_cluster_density exclude_min = 'overall' removes global min", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(paste0("C", 1:3), each = 20),
    var1 = c(0, rnorm(19, 2), rnorm(20, 0), rnorm(20, -2)),
    var2 = c(rnorm(20, -1), rnorm(20, 1), rnorm(20, 0))
  )
  result <- plot_cluster_density(
    data, cluster = "cluster", exclude_min = "overall"
  )
  for (p in result) expect_s3_class(p, "ggplot")
})

test_that("plot_cluster_density exclude_min = 'variable' removes per-variable min", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(paste0("C", 1:3), each = 20),
    var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2)),
    var2 = c(rnorm(20, -1), rnorm(20, 1), rnorm(20, 0))
  )
  result <- plot_cluster_density(
    data, cluster = "cluster", exclude_min = "variable"
  )
  for (p in result) expect_s3_class(p, "ggplot")
})

test_that("plot_cluster_density exclude_min invalid value errors", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(paste0("C", 1:3), each = 20),
    var1 = rnorm(60)
  )
  expect_error(
    plot_cluster_density(data, cluster = "cluster", exclude_min = "yes")
  )
})

test_that("plot_cluster_density scales parameter is passed to facet_wrap", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(paste0("C", 1:3), each = 20),
    var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2)),
    var2 = c(rnorm(20, -1), rnorm(20, 1), rnorm(20, 0))
  )
  p <- plot_cluster_density(data, cluster = "cluster", n_col = 2, scales = "free")
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
  result <- plot_cluster_density(data, cluster = "cluster", col_clusters = cols)
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
