test_that("plot_cluster_mst returns a named list by default", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(paste0("C", 1:3), each = 20),
    var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2)),
    var2 = c(rnorm(20, -1), rnorm(20, 1), rnorm(20, 0))
  )
  result <- plot_cluster_mst(data, cluster = "cluster")
  expect_type(result, "list")
  expect_named(result, c("var1", "var2"))
})

test_that("plot_cluster_mst list elements are ggplot objects", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(paste0("C", 1:3), each = 20),
    var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2)),
    var2 = c(rnorm(20, -1), rnorm(20, 1), rnorm(20, 0))
  )
  result <- plot_cluster_mst(data, cluster = "cluster")
  for (p in result) expect_s3_class(p, "ggplot")
})

test_that("plot_cluster_mst plots have segment and point layers", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(paste0("C", 1:3), each = 20),
    var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2)),
    var2 = c(rnorm(20, -1), rnorm(20, 1), rnorm(20, 0))
  )
  result <- plot_cluster_mst(data, cluster = "cluster")
  for (p in result) {
    has_seg <- any(sapply(p$layers, function(l) inherits(l$geom, "GeomSegment")))
    has_pt <- any(sapply(p$layers, function(l) inherits(l$geom, "GeomPoint")))
    expect_true(has_seg)
    expect_true(has_pt)
  }
})

test_that("plot_cluster_mst MST has n_clusters - 1 edges", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(paste0("C", 1:4), each = 20),
    var1 = c(rnorm(20, 3), rnorm(20, 1), rnorm(20, -1), rnorm(20, -3)),
    var2 = rnorm(80)
  )
  result <- plot_cluster_mst(data, cluster = "cluster")
  p <- result[[1]]
  seg_layer <- p$layers[sapply(p$layers, function(l) inherits(l$geom, "GeomSegment"))][[1]]
  expect_equal(nrow(seg_layer$data), 3L)
})

test_that("plot_cluster_mst errors with fewer than two clusters", {
  data <- data.frame(cluster = rep("C1", 10), var1 = rnorm(10))
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
  result <- plot_cluster_mst(data, cluster = "cluster")
  for (p in result) {
    seg_layer <- p$layers[sapply(p$layers, function(l) inherits(l$geom, "GeomSegment"))][[1]]
    expect_equal(nrow(seg_layer$data), 1L)
  }
})

test_that("plot_cluster_mst respects vars argument", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(paste0("C", 1:3), each = 20),
    var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2)),
    var2 = c(rnorm(20, -1), rnorm(20, 1), rnorm(20, 0)),
    var3 = rnorm(60)
  )
  result <- plot_cluster_mst(data, cluster = "cluster", vars = c("var1", "var2"))
  expect_named(result, c("var1", "var2"))
  expect_length(result, 2L)
})

test_that("plot_cluster_mst uses all non-cluster columns when vars is NULL", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(paste0("C", 1:3), each = 20),
    var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2)),
    var2 = c(rnorm(20, -1), rnorm(20, 1), rnorm(20, 0)),
    var3 = rnorm(60)
  )
  result <- plot_cluster_mst(data, cluster = "cluster")
  expect_named(result, c("var1", "var2", "var3"))
})

test_that("plot_cluster_mst nodes have a fill scale", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(paste0("C", 1:3), each = 20),
    var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2)),
    var2 = c(rnorm(20, -1), rnorm(20, 1), rnorm(20, 0))
  )
  result <- plot_cluster_mst(data, cluster = "cluster")
  for (p in result) {
    fill_scale <- p$scales$get_scales("fill")
    expect_false(is.null(fill_scale))
  }
})

test_that("plot_cluster_mst fill values are between 0 and 1", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(paste0("C", 1:3), each = 20),
    var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2)),
    var2 = c(rnorm(20, -1), rnorm(20, 1), rnorm(20, 0))
  )
  result <- plot_cluster_mst(data, cluster = "cluster")
  for (p in result) {
    pt_layer <- p$layers[sapply(p$layers, function(l) inherits(l$geom, "GeomPoint"))][[1]]
    fill_vals <- pt_layer$data$fill
    expect_true(all(fill_vals >= 0 & fill_vals <= 1))
  }
})

test_that("plot_cluster_mst col_clusters applies colour scale", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(paste0("C", 1:3), each = 20),
    var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2)),
    var2 = c(rnorm(20, -1), rnorm(20, 1), rnorm(20, 0))
  )
  cols <- c(C1 = "#FF0000", C2 = "#00FF00", C3 = "#0000FF")
  result <- plot_cluster_mst(data, cluster = "cluster", col_clusters = cols)
  for (p in result) {
    colour_scale <- p$scales$get_scales("colour")
    expect_false(is.null(colour_scale))
  }
})

test_that("plot_cluster_mst custom heatmap colours apply to fill scale", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(paste0("C", 1:3), each = 20),
    var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2)),
    var2 = c(rnorm(20, -1), rnorm(20, 1), rnorm(20, 0))
  )
  result <- plot_cluster_mst(
    data, cluster = "cluster",
    col_high = "#FF0000", col_mid = "#FFFFFF", col_low = "#0000FF"
  )
  for (p in result) {
    fill_scale <- p$scales$get_scales("fill")
    expect_equal(fill_scale$palette(0), "#0000FF")
    expect_equal(fill_scale$palette(1), "#FF0000")
  }
})

test_that("plot_cluster_mst with n_col returns a cowplot grid", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(paste0("C", 1:3), each = 20),
    var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2)),
    var2 = c(rnorm(20, -1), rnorm(20, 1), rnorm(20, 0))
  )
  result <- plot_cluster_mst(data, cluster = "cluster", n_col = 2)
  expect_s3_class(result, "gg")
})

test_that("plot_cluster_mst with n_row returns a cowplot grid", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(paste0("C", 1:3), each = 20),
    var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2)),
    var2 = c(rnorm(20, -1), rnorm(20, 1), rnorm(20, 0))
  )
  result <- plot_cluster_mst(data, cluster = "cluster", n_row = 1)
  expect_s3_class(result, "gg")
})

test_that("plot_cluster_mst NULL theme applies no theme", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(paste0("C", 1:3), each = 20),
    var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2)),
    var2 = c(rnorm(20, -1), rnorm(20, 1), rnorm(20, 0))
  )
  result <- plot_cluster_mst(data, cluster = "cluster", thm = NULL)
  for (p in result) expect_s3_class(p, "ggplot")
})

test_that("plot_cluster_mst NULL grid applies no grid", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(paste0("C", 1:3), each = 20),
    var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2)),
    var2 = c(rnorm(20, -1), rnorm(20, 1), rnorm(20, 0))
  )
  result <- plot_cluster_mst(data, cluster = "cluster", grid = NULL)
  for (p in result) expect_s3_class(p, "ggplot")
})

test_that("plot_cluster_mst default layout_algorithm is kamada-kawai", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(paste0("C", 1:3), each = 20),
    var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2)),
    var2 = c(rnorm(20, -1), rnorm(20, 1), rnorm(20, 0))
  )
  result <- plot_cluster_mst(data, cluster = "cluster")
  for (p in result) {
    expect_s3_class(p, "ggplot")
    expect_equal(p$labels$x, "KK 1")
    expect_equal(p$labels$y, "KK 2")
  }
})

test_that("plot_cluster_mst layout_algorithm mds uses MDS axis labels", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(paste0("C", 1:3), each = 20),
    var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2)),
    var2 = c(rnorm(20, -1), rnorm(20, 1), rnorm(20, 0))
  )
  result <- plot_cluster_mst(data, cluster = "cluster", layout_algorithm = "mds")
  for (p in result) {
    expect_equal(p$labels$x, "MDS 1")
    expect_equal(p$labels$y, "MDS 2")
  }
})

test_that("plot_cluster_mst coord_equal TRUE applies CoordEqual", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(paste0("C", 1:3), each = 20),
    var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2)),
    var2 = c(rnorm(20, -1), rnorm(20, 1), rnorm(20, 0))
  )
  result <- plot_cluster_mst(data, cluster = "cluster", coord_equal = TRUE)
  for (p in result) expect_equal(p$coordinates$ratio, 1)
})

test_that("plot_cluster_mst coord_equal FALSE does not apply CoordEqual", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(paste0("C", 1:3), each = 20),
    var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2)),
    var2 = c(rnorm(20, -1), rnorm(20, 1), rnorm(20, 0))
  )
  result <- plot_cluster_mst(data, cluster = "cluster", coord_equal = FALSE)
  for (p in result) expect_true(is.null(p$coordinates$ratio))
})

test_that("plot_cluster_mst suppress_axes NULL inherits coord_equal", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(paste0("C", 1:3), each = 20),
    var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2)),
    var2 = c(rnorm(20, -1), rnorm(20, 1), rnorm(20, 0))
  )
  # coord_equal = TRUE => axes suppressed by default
  result_eq <- plot_cluster_mst(
    data, cluster = "cluster", coord_equal = TRUE, suppress_axes = NULL
  )
  for (p in result_eq) {
    theme_elements <- p$theme
    expect_true(inherits(theme_elements$axis.text, "element_blank"))
  }
  # coord_equal = FALSE => axes not suppressed by default
  result_no <- plot_cluster_mst(
    data, cluster = "cluster", coord_equal = FALSE, suppress_axes = NULL
  )
  for (p in result_no) {
    theme_elements <- p$theme
    expect_false(isTRUE(inherits(theme_elements$axis.text, "element_blank")))
  }
})

test_that("plot_cluster_mst suppress_axes TRUE suppresses axes regardless of coord_equal", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(paste0("C", 1:3), each = 20),
    var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2)),
    var2 = c(rnorm(20, -1), rnorm(20, 1), rnorm(20, 0))
  )
  result <- plot_cluster_mst(
    data, cluster = "cluster", coord_equal = FALSE, suppress_axes = TRUE
  )
  for (p in result) {
    expect_true(inherits(p$theme$axis.text, "element_blank"))
    expect_true(inherits(p$theme$axis.ticks, "element_blank"))
    expect_true(inherits(p$theme$axis.line, "element_blank"))
    expect_true(inherits(p$theme$axis.title, "element_blank"))
  }
})

