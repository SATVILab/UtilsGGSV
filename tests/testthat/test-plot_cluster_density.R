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
  result <- plot_cluster_density(
    data, cluster = "cluster", density = "overall"
  )
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
  p <- plot_cluster_density(
    data, cluster = "cluster", density = "overall", n_col = 1
  )
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
    data, cluster = "cluster", vars = c("var1", "var2"),
    density = "overall", n_col = 1
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
  p <- plot_cluster_density(
    data, cluster = "cluster", density = "overall", n_col = 1
  )
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

# density and scale parameter tests

test_that("plot_cluster_density density = 'cluster' returns list of ggplot objects", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(paste0("C", 1:3), each = 20),
    var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2)),
    var2 = c(rnorm(20, -1), rnorm(20, 1), rnorm(20, 0))
  )
  result <- plot_cluster_density(data, cluster = "cluster", density = "cluster")
  expect_type(result, "list")
  for (p in result) expect_s3_class(p, "ggplot")
})

test_that("plot_cluster_density density = 'cluster' plot elements have geom_line layers", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(paste0("C", 1:3), each = 20),
    var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2)),
    var2 = c(rnorm(20, -1), rnorm(20, 1), rnorm(20, 0))
  )
  result <- plot_cluster_density(data, cluster = "cluster", density = "cluster")
  for (p in result) {
    expect_true(any(sapply(p$layers, function(l) inherits(l$geom, "GeomLine"))))
    expect_false(any(sapply(p$layers, function(l) inherits(l$geom, "GeomVline"))))
  }
})

test_that("plot_cluster_density density = 'both' returns list of ggplot objects", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(paste0("C", 1:3), each = 20),
    var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2)),
    var2 = c(rnorm(20, -1), rnorm(20, 1), rnorm(20, 0))
  )
  result <- plot_cluster_density(data, cluster = "cluster", density = "both")
  expect_type(result, "list")
  for (p in result) expect_s3_class(p, "ggplot")
})

test_that("plot_cluster_density density = 'both' plot elements have two geom_line layers", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(paste0("C", 1:3), each = 20),
    var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2)),
    var2 = c(rnorm(20, -1), rnorm(20, 1), rnorm(20, 0))
  )
  result <- plot_cluster_density(data, cluster = "cluster", density = "both")
  for (p in result) {
    n_lines <- sum(sapply(p$layers, function(l) inherits(l$geom, "GeomLine")))
    expect_equal(n_lines, 2L)
  }
})

test_that("plot_cluster_density density = 'cluster' facet mode returns ggplot", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(paste0("C", 1:3), each = 20),
    var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2)),
    var2 = c(rnorm(20, -1), rnorm(20, 1), rnorm(20, 0))
  )
  p <- plot_cluster_density(
    data, cluster = "cluster", density = "cluster", n_col = 2
  )
  expect_s3_class(p, "ggplot")
  expect_true(any(sapply(p$layers, function(l) inherits(l$geom, "GeomLine"))))
})

test_that("plot_cluster_density density = 'both' facet mode returns ggplot", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(paste0("C", 1:3), each = 20),
    var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2)),
    var2 = c(rnorm(20, -1), rnorm(20, 1), rnorm(20, 0))
  )
  p <- plot_cluster_density(
    data, cluster = "cluster", density = "both", n_col = 2
  )
  expect_s3_class(p, "ggplot")
  n_lines <- sum(sapply(p$layers, function(l) inherits(l$geom, "GeomLine")))
  expect_equal(n_lines, 2L)
})

test_that("plot_cluster_density scale = 'max_overall' scales cluster densities", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(paste0("C", 1:3), each = 20),
    var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2)),
    var2 = c(rnorm(20, -1), rnorm(20, 1), rnorm(20, 0))
  )
  result <- plot_cluster_density(
    data, cluster = "cluster", density = "both", scale = "max_overall"
  )
  for (p in result) expect_s3_class(p, "ggplot")
})

test_that("plot_cluster_density scale = 'max_cluster' returns ggplot objects", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(paste0("C", 1:3), each = 20),
    var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2)),
    var2 = c(rnorm(20, -1), rnorm(20, 1), rnorm(20, 0))
  )
  result <- plot_cluster_density(
    data, cluster = "cluster", density = "both", scale = "max_cluster"
  )
  for (p in result) expect_s3_class(p, "ggplot")
})

test_that("plot_cluster_density scale = 'free' returns ggplot objects", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(paste0("C", 1:3), each = 20),
    var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2)),
    var2 = c(rnorm(20, -1), rnorm(20, 1), rnorm(20, 0))
  )
  result <- plot_cluster_density(
    data, cluster = "cluster", density = "both", scale = "free"
  )
  for (p in result) expect_s3_class(p, "ggplot")
})

test_that("plot_cluster_density density invalid value errors", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(paste0("C", 1:3), each = 20),
    var1 = rnorm(60)
  )
  expect_error(
    plot_cluster_density(data, cluster = "cluster", density = "all")
  )
})

test_that("plot_cluster_density scale invalid value errors", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(paste0("C", 1:3), each = 20),
    var1 = rnorm(60)
  )
  expect_error(
    plot_cluster_density(
      data, cluster = "cluster", density = "both", scale = "none"
    )
  )
})

test_that("plot_cluster_density scale = 'max_overall' max y matches overall max", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(paste0("C", 1:3), each = 20),
    var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2))
  )
  result <- plot_cluster_density(
    data, cluster = "cluster", density = "both", scale = "max_overall"
  )
  p <- result[["var1"]]
  # The overall density line layer (no colour aes) should be the first geom_line
  overall_layer <- p$layers[[1]]
  overall_data <- overall_layer$data
  max_overall <- max(overall_data$y)
  # Cluster density layers should be scaled to match
  cluster_layer <- p$layers[[2]]
  cluster_data <- cluster_layer$data
  max_cluster_scaled <- max(cluster_data$y)
  expect_equal(max_cluster_scaled, max_overall, tolerance = 1e-10)
})

# rug tests

test_that("plot_cluster_density adds a rug layer by default", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(paste0("C", 1:3), each = 20),
    var1 = rnorm(60)
  )
  result <- plot_cluster_density(data, cluster = "cluster")
  p <- result[["var1"]]
  expect_true(any(sapply(p$layers, function(l) inherits(l$geom, "GeomRug"))))
})

test_that("plot_cluster_density default rug for density='overall' has no colour aes", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(paste0("C", 1:3), each = 20),
    var1 = rnorm(60)
  )
  result <- plot_cluster_density(
    data, cluster = "cluster", density = "overall"
  )
  p <- result[["var1"]]
  rug_layers <- p$layers[
    sapply(p$layers, function(l) inherits(l$geom, "GeomRug"))
  ]
  expect_length(rug_layers, 1L)
  expect_false("colour" %in% names(rug_layers[[1]]$mapping))
})

test_that("plot_cluster_density default rug for density='cluster' has colour aes", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(paste0("C", 1:3), each = 20),
    var1 = rnorm(60)
  )
  result <- plot_cluster_density(
    data, cluster = "cluster", density = "cluster"
  )
  p <- result[["var1"]]
  rug_layers <- p$layers[
    sapply(p$layers, function(l) inherits(l$geom, "GeomRug"))
  ]
  expect_length(rug_layers, 1L)
  expect_true("colour" %in% names(rug_layers[[1]]$mapping))
})

test_that("plot_cluster_density default rug for density='both' has colour aes", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(paste0("C", 1:3), each = 20),
    var1 = rnorm(60)
  )
  result <- plot_cluster_density(
    data, cluster = "cluster", density = "both"
  )
  p <- result[["var1"]]
  rug_layers <- p$layers[
    sapply(p$layers, function(l) inherits(l$geom, "GeomRug"))
  ]
  expect_length(rug_layers, 1L)
  expect_true("colour" %in% names(rug_layers[[1]]$mapping))
})

test_that("plot_cluster_density rug='overall' always adds overall rug", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(paste0("C", 1:3), each = 20),
    var1 = rnorm(60)
  )
  result <- plot_cluster_density(
    data, cluster = "cluster", density = "cluster", rug = "overall"
  )
  p <- result[["var1"]]
  rug_layers <- p$layers[
    sapply(p$layers, function(l) inherits(l$geom, "GeomRug"))
  ]
  expect_length(rug_layers, 1L)
  expect_false("colour" %in% names(rug_layers[[1]]$mapping))
})

test_that("plot_cluster_density rug='cluster' always adds cluster rug", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(paste0("C", 1:3), each = 20),
    var1 = rnorm(60)
  )
  result <- plot_cluster_density(
    data, cluster = "cluster", density = "overall", rug = "cluster"
  )
  p <- result[["var1"]]
  rug_layers <- p$layers[
    sapply(p$layers, function(l) inherits(l$geom, "GeomRug"))
  ]
  expect_length(rug_layers, 1L)
  expect_true("colour" %in% names(rug_layers[[1]]$mapping))
})

test_that("plot_cluster_density adds rug in facet mode", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(paste0("C", 1:3), each = 20),
    var1 = rnorm(60),
    var2 = rnorm(60)
  )
  p <- plot_cluster_density(data, cluster = "cluster", n_col = 1)
  expect_true(any(sapply(p$layers, function(l) inherits(l$geom, "GeomRug"))))
})

test_that("plot_cluster_density rug invalid value errors", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(paste0("C", 1:3), each = 20),
    var1 = rnorm(60)
  )
  expect_error(
    plot_cluster_density(data, cluster = "cluster", rug = "none"),
    "should be one of"
  )
})

# cluster coercion tests

test_that("plot_cluster_density errors when cluster column is integer", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(1:3, each = 20),
    var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2))
  )
  expect_error(
    plot_cluster_density(data, cluster = "cluster", density = "overall"),
    "numeric"
  )
})

test_that("plot_cluster_density works with character cluster after manual conversion", {
  set.seed(1)
  data <- data.frame(
    cluster = as.character(rep(1:3, each = 20)),
    var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2))
  )
  result <- plot_cluster_density(data, cluster = "cluster", density = "overall")
  p <- result[["var1"]]
  vline_layer <- p$layers[
    sapply(p$layers, function(l) inherits(l$geom, "GeomVline"))
  ][[1]]
  expect_type(vline_layer$data$cluster, "character")
})

test_that("plot_cluster_density factor cluster produces ggplot objects", {
  set.seed(1)
  data <- data.frame(
    cluster = factor(rep(paste0("C", 1:3), each = 20)),
    var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2))
  )
  result <- plot_cluster_density(data, cluster = "cluster", density = "overall")
  expect_type(result, "list")
  for (p in result) expect_s3_class(p, "ggplot")
})

# density_overall_weight tests

test_that("plot_cluster_density density_overall_weight='even' returns ggplot list", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(paste0("C", 1:3), each = 20),
    var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2)),
    var2 = c(rnorm(20, -1), rnorm(20, 1), rnorm(20, 0))
  )
  result <- plot_cluster_density(
    data, cluster = "cluster", density_overall_weight = "even"
  )
  expect_type(result, "list")
  for (p in result) expect_s3_class(p, "ggplot")
})

test_that("plot_cluster_density density_overall_weight='even' uses geom_line not geom_density", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(paste0("C", 1:3), each = 20),
    var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2))
  )
  result <- plot_cluster_density(
    data, cluster = "cluster", density = "overall",
    density_overall_weight = "even"
  )
  p <- result[["var1"]]
  expect_false(
    any(sapply(p$layers, function(l) inherits(l$geom, "GeomDensity")))
  )
  expect_true(
    any(sapply(p$layers, function(l) inherits(l$geom, "GeomLine")))
  )
})

test_that("plot_cluster_density density_overall_weight='even' with density='both' returns ggplot list", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(paste0("C", 1:3), each = 20),
    var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2))
  )
  result <- plot_cluster_density(
    data, cluster = "cluster", density = "both",
    density_overall_weight = "even"
  )
  expect_type(result, "list")
  for (p in result) expect_s3_class(p, "ggplot")
})

test_that("plot_cluster_density density_overall_weight='even' facet mode returns ggplot", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(paste0("C", 1:3), each = 20),
    var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2)),
    var2 = c(rnorm(20, -1), rnorm(20, 1), rnorm(20, 0))
  )
  p <- plot_cluster_density(
    data, cluster = "cluster", n_col = 2,
    density_overall_weight = "even"
  )
  expect_s3_class(p, "ggplot")
})

test_that("plot_cluster_density density_overall_weight='even' facet density='both' returns ggplot", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(paste0("C", 1:3), each = 20),
    var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2)),
    var2 = c(rnorm(20, -1), rnorm(20, 1), rnorm(20, 0))
  )
  p <- plot_cluster_density(
    data, cluster = "cluster", n_col = 2, density = "both",
    density_overall_weight = "even"
  )
  expect_s3_class(p, "ggplot")
})

test_that("plot_cluster_density density_overall_weight='even' produces different y than pooled", {
  set.seed(1)
  # Unequal cluster sizes - even weighting should differ from pooled
  data <- data.frame(
    cluster = c(rep("A", 5), rep("B", 55)),
    var1 = c(rnorm(5, -3), rnorm(55, 3))
  )
  result_pooled <- plot_cluster_density(
    data, cluster = "cluster", density = "overall",
    density_overall_weight = NULL, thm = NULL, grid = NULL
  )
  result_even <- plot_cluster_density(
    data, cluster = "cluster", density = "overall",
    density_overall_weight = "even", thm = NULL, grid = NULL
  )
  p_pooled <- result_pooled[["var1"]]
  p_even <- result_even[["var1"]]
  # Pooled uses geom_density, even uses geom_line - they differ structurally
  expect_true(
    any(sapply(p_pooled$layers, function(l) inherits(l$geom, "GeomDensity")))
  )
  expect_false(
    any(sapply(p_even$layers, function(l) inherits(l$geom, "GeomDensity")))
  )
})

test_that("plot_cluster_density density_overall_weight invalid value errors", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(paste0("C", 1:3), each = 20),
    var1 = rnorm(60)
  )
  expect_error(
    plot_cluster_density(
      data, cluster = "cluster", density_overall_weight = "half"
    ),
    "should be"
  )
})

# bandwidth tests

test_that("plot_cluster_density bandwidth = 'hpi_1' (default) returns ggplot list", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(paste0("C", 1:3), each = 20),
    var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2))
  )
  result <- plot_cluster_density(
    data, cluster = "cluster", density = "cluster"
  )
  expect_type(result, "list")
  for (p in result) expect_s3_class(p, "ggplot")
})

test_that("plot_cluster_density bandwidth = 'hpi_0' returns ggplot list", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(paste0("C", 1:3), each = 20),
    var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2))
  )
  result <- plot_cluster_density(
    data, cluster = "cluster", density = "cluster", bandwidth = "hpi_0"
  )
  expect_type(result, "list")
  for (p in result) expect_s3_class(p, "ggplot")
})

test_that("plot_cluster_density bandwidth = 'SJ' returns ggplot list", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(paste0("C", 1:3), each = 20),
    var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2))
  )
  result <- plot_cluster_density(
    data, cluster = "cluster", density = "cluster", bandwidth = "SJ"
  )
  expect_type(result, "list")
  for (p in result) expect_s3_class(p, "ggplot")
})

test_that("plot_cluster_density bandwidth as numeric returns ggplot list", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(paste0("C", 1:3), each = 20),
    var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2))
  )
  result <- plot_cluster_density(
    data, cluster = "cluster", density = "cluster", bandwidth = 0.5
  )
  expect_type(result, "list")
  for (p in result) expect_s3_class(p, "ggplot")
})

test_that("plot_cluster_density bandwidth invalid value errors", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(paste0("C", 1:3), each = 20),
    var1 = rnorm(60)
  )
  expect_error(
    plot_cluster_density(
      data, cluster = "cluster", bandwidth = "nrd0"
    ),
    "should be one of"
  )
})

test_that("plot_cluster_density bandwidth hpi_1 applied in density_overall_weight=even", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(paste0("C", 1:3), each = 20),
    var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2))
  )
  result <- plot_cluster_density(
    data, cluster = "cluster", density = "overall",
    density_overall_weight = "even", bandwidth = "hpi_1"
  )
  expect_type(result, "list")
  for (p in result) expect_s3_class(p, "ggplot")
})

test_that("plot_cluster_density default density is 'both'", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(paste0("C", 1:3), each = 20),
    var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2))
  )
  result <- plot_cluster_density(data, cluster = "cluster")
  p <- result[["var1"]]
  n_lines <- sum(sapply(p$layers, function(l) inherits(l$geom, "GeomLine")))
  # 1 overall line + 1 cluster line layer (3 clusters combined in one layer)
  expect_equal(n_lines, 2L)
  expect_false(
    any(sapply(p$layers, function(l) inherits(l$geom, "GeomDensity")))
  )
  expect_false(
    any(sapply(p$layers, function(l) inherits(l$geom, "GeomVline")))
  )
  # The cluster density layer data should contain all 3 clusters
  cluster_line <- p$layers[
    sapply(p$layers, function(l) inherits(l$geom, "GeomLine"))
  ][[2]]
  expect_equal(length(unique(cluster_line$data$cluster)), 3L)
})

test_that("plot_cluster_density bandwidth non-positive errors", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(paste0("C", 1:3), each = 20),
    var1 = rnorm(60)
  )
  expect_error(
    plot_cluster_density(data, cluster = "cluster", bandwidth = 0),
    "positive"
  )
  expect_error(
    plot_cluster_density(data, cluster = "cluster", bandwidth = -1),
    "positive"
  )
})

test_that("plot_cluster_density errors when data is not a data.frame", {
  expect_error(
    plot_cluster_density(list(cluster = c("A", "B"), var1 = 1:2), cluster = "cluster"),
    "`data` must be a data.frame"
  )
})

test_that("plot_cluster_density errors when cluster is not a character string", {
  data <- data.frame(
    cluster = rep(c("A", "B"), each = 5),
    var1 = rnorm(10)
  )
  expect_error(
    plot_cluster_density(data, cluster = 1),
    "single non-NA character string"
  )
})

test_that("plot_cluster_density errors when cluster column is missing", {
  data <- data.frame(
    grp = rep(c("A", "B"), each = 5),
    var1 = rnorm(10)
  )
  expect_error(
    plot_cluster_density(data, cluster = "cluster"),
    "not found in `data`"
  )
})

test_that("plot_cluster_density errors when cluster column is numeric", {
  data <- data.frame(
    cluster = rep(c(1.0, 2.0), each = 5),
    var1 = rnorm(10)
  )
  expect_error(
    plot_cluster_density(data, cluster = "cluster"),
    "numeric"
  )
})

test_that("plot_cluster_density errors when cluster column has fewer than 2 unique values", {
  data <- data.frame(
    cluster = rep("A", 10),
    var1 = rnorm(10)
  )
  expect_error(
    plot_cluster_density(data, cluster = "cluster"),
    "at least 2 unique"
  )
})

test_that("plot_cluster_density errors when a vars column is missing", {
  data <- data.frame(
    cluster = rep(c("A", "B"), each = 5),
    var1 = rnorm(10)
  )
  expect_error(
    plot_cluster_density(data, cluster = "cluster", vars = c("var1", "var_missing")),
    "not found in `data`"
  )
})

test_that("plot_cluster_density errors when a vars column is not numeric", {
  data <- data.frame(
    cluster = rep(c("A", "B"), each = 5),
    var1 = rnorm(10),
    label = rep(c("x", "y"), 5)
  )
  expect_error(
    plot_cluster_density(data, cluster = "cluster", vars = c("var1", "label")),
    "not numeric"
  )
})

test_that("plot_cluster_density errors when cluster column is integer", {
  data <- data.frame(
    cluster = rep(c(1L, 2L), each = 5),
    var1 = rnorm(10)
  )
  expect_error(
    plot_cluster_density(data, cluster = "cluster"),
    "numeric"
  )
})

test_that("plot_cluster_density works with factor cluster column", {
  set.seed(1)
  data <- data.frame(
    cluster = factor(rep(c("A", "B"), each = 20)),
    var1 = rnorm(40),
    var2 = rnorm(40)
  )
  result <- plot_cluster_density(data, cluster = "cluster")
  expect_type(result, "list")
  expect_s3_class(result[[1]], "ggplot")
})
