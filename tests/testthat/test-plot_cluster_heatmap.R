test_that("plot_cluster_heatmap returns a ggplot object", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(paste0("C", 1:3), each = 20),
    var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2)),
    var2 = c(rnorm(20, -1), rnorm(20, 1), rnorm(20, 0))
  )
  p <- plot_cluster_heatmap(data, cluster = "cluster")
  expect_s3_class(p, "ggplot")
})

test_that("plot_cluster_heatmap uses geom_raster layer", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(paste0("C", 1:3), each = 20),
    var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2)),
    var2 = c(rnorm(20, -1), rnorm(20, 1), rnorm(20, 0))
  )
  p <- plot_cluster_heatmap(data, cluster = "cluster")
  has_raster <- any(sapply(p$layers, function(l) inherits(l$geom, "GeomRaster")))
  expect_true(has_raster)
})

test_that("plot_cluster_heatmap respects vars argument", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(paste0("C", 1:3), each = 20),
    var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2)),
    var2 = c(rnorm(20, -1), rnorm(20, 1), rnorm(20, 0)),
    var3 = rnorm(60)
  )
  p <- plot_cluster_heatmap(data, cluster = "cluster", vars = c("var1", "var2"))
  expect_equal(length(unique(p$data$variable)), 2L)
  expect_true(all(unique(as.character(p$data$variable)) %in% c("var1", "var2")))
})

test_that("plot_cluster_heatmap uses all non-cluster columns when vars is NULL", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(paste0("C", 1:3), each = 20),
    var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2)),
    var2 = c(rnorm(20, -1), rnorm(20, 1), rnorm(20, 0)),
    var3 = rnorm(60)
  )
  p <- plot_cluster_heatmap(data, cluster = "cluster")
  expect_equal(
    sort(unique(as.character(p$data$variable))),
    sort(c("var1", "var2", "var3"))
  )
})

test_that("plot_cluster_heatmap NULL theme applies no theme", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(paste0("C", 1:3), each = 20),
    var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2)),
    var2 = c(rnorm(20, -1), rnorm(20, 1), rnorm(20, 0))
  )
  expect_s3_class(
    plot_cluster_heatmap(data, cluster = "cluster", thm = NULL),
    "ggplot"
  )
})

test_that("plot_cluster_heatmap NULL grid applies no grid", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(paste0("C", 1:3), each = 20),
    var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2)),
    var2 = c(rnorm(20, -1), rnorm(20, 1), rnorm(20, 0))
  )
  expect_s3_class(
    plot_cluster_heatmap(data, cluster = "cluster", grid = NULL),
    "ggplot"
  )
})

test_that("plot_cluster_heatmap perc values are between 0 and 1", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(paste0("C", 1:3), each = 20),
    var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2)),
    var2 = c(rnorm(20, -1), rnorm(20, 1), rnorm(20, 0))
  )
  p <- plot_cluster_heatmap(data, cluster = "cluster")
  expect_true(all(p$data$perc >= 0 & p$data$perc <= 1))
})

test_that("plot_cluster_heatmap works with two clusters", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(c("A", "B"), each = 15),
    var1 = c(rnorm(15, 2), rnorm(15, -2)),
    var2 = c(rnorm(15, 0), rnorm(15, 1))
  )
  p <- plot_cluster_heatmap(data, cluster = "cluster")
  expect_s3_class(p, "ggplot")
  expect_equal(length(unique(p$data$cluster)), 2L)
})

test_that("plot_cluster_heatmap custom colours are applied to the scale", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(paste0("C", 1:3), each = 20),
    var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2)),
    var2 = c(rnorm(20, -1), rnorm(20, 1), rnorm(20, 0))
  )
  p <- plot_cluster_heatmap(
    data,
    cluster = "cluster",
    palette = NULL,
    col = c("#0000FF", "#FFFFFF", "#FF0000")
  )
  expect_s3_class(p, "ggplot")
  fill_scale <- p$scales$get_scales("fill")
  expect_equal(fill_scale$palette(0), "#0000FF")
  expect_equal(fill_scale$palette(1), "#FF0000")
  expect_equal(fill_scale$palette(0.5), "#FFFFFF")
})

test_that("plot_cluster_heatmap custom white_range is applied to the scale", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(paste0("C", 1:3), each = 20),
    var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2)),
    var2 = c(rnorm(20, -1), rnorm(20, 1), rnorm(20, 0))
  )
  # With white_range c(0.3, 0.7), the value 0.35 falls inside the white zone
  p_wide <- plot_cluster_heatmap(
    data,
    cluster = "cluster",
    white_range = c(0.3, 0.7)
  )
  # With default white_range c(0.4, 0.6), the value 0.35 falls outside the white zone
  p_default <- plot_cluster_heatmap(data, cluster = "cluster")
  sc_wide <- p_wide$scales$get_scales("fill")
  sc_default <- p_default$scales$get_scales("fill")
  expect_equal(sc_wide$palette(0.35), "#F7F7F7")
  expect_false(sc_default$palette(0.35) == "#F7F7F7")
})

test_that("plot_cluster_heatmap show_values adds a GeomText layer", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(paste0("C", 1:3), each = 20),
    var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2)),
    var2 = c(rnorm(20, -1), rnorm(20, 1), rnorm(20, 0))
  )
  p <- plot_cluster_heatmap(data, cluster = "cluster", show_values = TRUE)
  has_text <- any(sapply(p$layers, function(l) inherits(l$geom, "GeomText")))
  expect_true(has_text)
})

test_that("plot_cluster_heatmap show_values FALSE adds no GeomText layer", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(paste0("C", 1:3), each = 20),
    var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2)),
    var2 = c(rnorm(20, -1), rnorm(20, 1), rnorm(20, 0))
  )
  p <- plot_cluster_heatmap(data, cluster = "cluster", show_values = FALSE)
  has_text <- any(sapply(p$layers, function(l) inherits(l$geom, "GeomText")))
  expect_false(has_text)
})

test_that("plot_cluster_heatmap values_format custom function is applied", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(paste0("C", 1:3), each = 20),
    var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2)),
    var2 = c(rnorm(20, -1), rnorm(20, 1), rnorm(20, 0))
  )
  fmt <- function(x) paste0(round(x, 1), "!")
  p <- plot_cluster_heatmap(
    data,
    cluster = "cluster",
    show_values = TRUE,
    values_format = fmt
  )
  text_layer <- Filter(function(l) inherits(l$geom, "GeomText"), p$layers)[[1]]
  labels <- text_layer$data$label
  expect_true(all(grepl("!$", labels)))
})

test_that("plot_cluster_heatmap values_col and values_size are passed to text layer", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(paste0("C", 1:3), each = 20),
    var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2)),
    var2 = c(rnorm(20, -1), rnorm(20, 1), rnorm(20, 0))
  )
  p <- plot_cluster_heatmap(
    data,
    cluster = "cluster",
    show_values = TRUE,
    values_col = "white",
    values_size = 5
  )
  text_layer <- Filter(function(l) inherits(l$geom, "GeomText"), p$layers)[[1]]
  expect_equal(text_layer$aes_params$colour, "white")
  expect_equal(text_layer$aes_params$size, 5)
})

test_that("plot_cluster_heatmap med column is present in plot data", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(paste0("C", 1:3), each = 20),
    var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2)),
    var2 = c(rnorm(20, -1), rnorm(20, 1), rnorm(20, 0))
  )
  p <- plot_cluster_heatmap(data, cluster = "cluster")
  expect_true("med" %in% names(p$data))
  expected_med <- stats::median(data$var1[data$cluster == "C1"])
  row <- p$data[p$data$cluster == "C1" & p$data$variable == "var1", ]
  expect_equal(row$med, expected_med)
})

test_that("plot_cluster_heatmap scale_method zscore returns ggplot and perc can be negative", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(paste0("C", 1:3), each = 20),
    var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2)),
    var2 = c(rnorm(20, -1), rnorm(20, 1), rnorm(20, 0))
  )
  p <- plot_cluster_heatmap(data, cluster = "cluster", scale_method = "zscore")
  expect_s3_class(p, "ggplot")
  expect_true(any(p$data$perc < 0))
})

test_that("plot_cluster_heatmap scale_method zscore perc equals z-score of cluster median", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(paste0("C", 1:3), each = 20),
    var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2)),
    var2 = c(rnorm(20, -1), rnorm(20, 1), rnorm(20, 0))
  )
  p <- plot_cluster_heatmap(data, cluster = "cluster", scale_method = "zscore")
  med_c1_v1 <- stats::median(data$var1[data$cluster == "C1"])
  expected_z <- (med_c1_v1 - mean(data$var1)) / stats::sd(data$var1)
  row <- p$data[p$data$cluster == "C1" & p$data$variable == "var1", ]
  expect_equal(row$perc, expected_z)
})

test_that("plot_cluster_heatmap scale_method raw returns ggplot with perc equal to med", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(paste0("C", 1:3), each = 20),
    var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2)),
    var2 = c(rnorm(20, -1), rnorm(20, 1), rnorm(20, 0))
  )
  p <- plot_cluster_heatmap(data, cluster = "cluster", scale_method = "raw")
  expect_s3_class(p, "ggplot")
  expect_equal(p$data$perc, p$data$med)
})

test_that("plot_cluster_heatmap scale_method minmax returns ggplot with perc in [0, 1]", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(paste0("C", 1:3), each = 20),
    var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2)),
    var2 = c(rnorm(20, -1), rnorm(20, 1), rnorm(20, 0))
  )
  p <- plot_cluster_heatmap(data, cluster = "cluster", scale_method = "minmax")
  expect_s3_class(p, "ggplot")
  expect_true(all(p$data$perc >= 0 & p$data$perc <= 1))
})

test_that("plot_cluster_heatmap scale_method minmax_var returns ggplot with perc in [0, 1]", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(paste0("C", 1:3), each = 20),
    var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2)),
    var2 = c(rnorm(20, -1), rnorm(20, 1), rnorm(20, 0))
  )
  p <- plot_cluster_heatmap(data, cluster = "cluster", scale_method = "minmax_var")
  expect_s3_class(p, "ggplot")
  expect_true(all(p$data$perc >= 0 & p$data$perc <= 1))
})

test_that("plot_cluster_heatmap invalid scale_method gives an error", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(paste0("C", 1:3), each = 20),
    var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2)),
    var2 = c(rnorm(20, -1), rnorm(20, 1), rnorm(20, 0))
  )
  expect_error(
    plot_cluster_heatmap(data, cluster = "cluster", scale_method = "invalid"),
    "should be one of"
  )
})

test_that("plot_cluster_heatmap scale_method zscore legend name is Z-score", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(paste0("C", 1:3), each = 20),
    var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2)),
    var2 = c(rnorm(20, -1), rnorm(20, 1), rnorm(20, 0))
  )
  p <- plot_cluster_heatmap(data, cluster = "cluster", scale_method = "zscore")
  fill_scale <- p$scales$get_scales("fill")
  expect_equal(fill_scale$name, "Z-score")
})

test_that("plot_cluster_heatmap scale_method raw legend name is Median", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(paste0("C", 1:3), each = 20),
    var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2)),
    var2 = c(rnorm(20, -1), rnorm(20, 1), rnorm(20, 0))
  )
  p <- plot_cluster_heatmap(data, cluster = "cluster", scale_method = "raw")
  fill_scale <- p$scales$get_scales("fill")
  expect_equal(fill_scale$name, "Median")
})

test_that("plot_cluster_heatmap five colours with auto positions returns ggplot", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(paste0("C", 1:3), each = 20),
    var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2)),
    var2 = c(rnorm(20, -1), rnorm(20, 1), rnorm(20, 0))
  )
  five_cols <- c("#2166AC", "#92C5DE", "#F7F7F7", "#F4A582", "#B2182B")
  p <- plot_cluster_heatmap(data, cluster = "cluster", col = five_cols)
  expect_s3_class(p, "ggplot")
  fill_scale <- p$scales$get_scales("fill")
  expect_equal(fill_scale$palette(0), "#2166AC")
  expect_equal(fill_scale$palette(1), "#B2182B")
})

test_that("plot_cluster_heatmap custom col_positions are used", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(paste0("C", 1:3), each = 20),
    var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2)),
    var2 = c(rnorm(20, -1), rnorm(20, 1), rnorm(20, 0))
  )
  p <- plot_cluster_heatmap(
    data,
    cluster = "cluster",
    col = c("#2166AC", "#F7F7F7", "#B2182B"),
    col_positions = c(0, 0.2, 1)
  )
  expect_s3_class(p, "ggplot")
  fill_scale <- p$scales$get_scales("fill")
  expect_equal(fill_scale$palette(0), "#2166AC")
  expect_equal(fill_scale$palette(1), "#B2182B")
})

test_that("plot_cluster_heatmap five colours with auto positions evenly spaces colours", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(paste0("C", 1:3), each = 20),
    var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2)),
    var2 = c(rnorm(20, -1), rnorm(20, 1), rnorm(20, 0))
  )
  five_cols <- c("#2166AC", "#92C5DE", "#F7F7F7", "#F4A582", "#B2182B")
  p <- plot_cluster_heatmap(data, cluster = "cluster", col = five_cols)
  fill_scale <- p$scales$get_scales("fill")
  expect_equal(fill_scale$palette(0.5), "#F7F7F7")
})

test_that("plot_cluster_heatmap errors when data is not a data.frame", {
  expect_error(
    plot_cluster_heatmap(list(cluster = 1:3, var1 = 1:3), cluster = "cluster"),
    "`\\.data` must be a data.frame"
  )
})

test_that("plot_cluster_heatmap errors when cluster is not a character string", {
  data <- data.frame(
    cluster = rep(c("A", "B"), each = 5),
    var1 = rnorm(10)
  )
  expect_error(
    plot_cluster_heatmap(data, cluster = 1),
    "single non-NA character string"
  )
})

test_that("plot_cluster_heatmap errors when cluster column is missing", {
  data <- data.frame(
    grp = rep(c("A", "B"), each = 5),
    var1 = rnorm(10)
  )
  expect_error(
    plot_cluster_heatmap(data, cluster = "cluster"),
    "not found in `\\.data`"
  )
})

test_that("plot_cluster_heatmap errors when cluster column is numeric", {
  data <- data.frame(
    cluster = rep(c(1, 2), each = 5),
    var1 = rnorm(10)
  )
  expect_error(
    plot_cluster_heatmap(data, cluster = "cluster"),
    "numeric"
  )
})

test_that("plot_cluster_heatmap errors when cluster column has fewer than 2 unique values", {
  data <- data.frame(
    cluster = rep("A", 10),
    var1 = rnorm(10)
  )
  expect_error(
    plot_cluster_heatmap(data, cluster = "cluster"),
    "at least 2 unique"
  )
})

test_that("plot_cluster_heatmap errors when a vars column is missing", {
  data <- data.frame(
    cluster = rep(c("A", "B"), each = 5),
    var1 = rnorm(10)
  )
  expect_error(
    plot_cluster_heatmap(data, cluster = "cluster", vars = c("var1", "var_missing")),
    "not found in `\\.data`"
  )
})

test_that("plot_cluster_heatmap errors when a vars column is not numeric", {
  data <- data.frame(
    cluster = rep(c("A", "B"), each = 5),
    var1 = rnorm(10),
    label = rep(c("x", "y"), 5)
  )
  expect_error(
    plot_cluster_heatmap(data, cluster = "cluster", vars = c("var1", "label")),
    "not numeric"
  )
})

test_that("plot_cluster_heatmap errors when col is not a character vector", {
  data <- data.frame(
    cluster = rep(c("A", "B"), each = 5),
    var1 = rnorm(10)
  )
  expect_error(
    plot_cluster_heatmap(data, cluster = "cluster", palette = NULL, col = 1:3),
    "`col` must be a character vector"
  )
})

test_that("plot_cluster_heatmap errors when col has fewer than 2 elements", {
  data <- data.frame(
    cluster = rep(c("A", "B"), each = 5),
    var1 = rnorm(10)
  )
  expect_error(
    plot_cluster_heatmap(data, cluster = "cluster", palette = NULL, col = "#FF0000"),
    "`col` must be a character vector of length >= 2"
  )
})

test_that("plot_cluster_heatmap errors when col_positions does not start at 0 and end at 1", {
  data <- data.frame(
    cluster = rep(c("A", "B"), each = 5),
    var1 = rnorm(10)
  )
  expect_error(
    plot_cluster_heatmap(
      data, cluster = "cluster",
      palette = NULL,
      col = c("#2166AC", "#F7F7F7", "#B2182B"),
      col_positions = c(0.1, 0.5, 0.9)
    ),
    "start at 0 and end at 1"
  )
})

test_that("plot_cluster_heatmap errors when col_positions wrong length", {
  data <- data.frame(
    cluster = rep(c("A", "B"), each = 5),
    var1 = rnorm(10)
  )
  expect_error(
    plot_cluster_heatmap(
      data, cluster = "cluster",
      palette = NULL,
      col = c("#2166AC", "#F7F7F7", "#B2182B"),
      col_positions = c(0, 1)
    ),
    "same length as `col`"
  )
})

test_that("plot_cluster_heatmap errors when show_values is not logical", {
  data <- data.frame(
    cluster = rep(c("A", "B"), each = 5),
    var1 = rnorm(10)
  )
  expect_error(
    plot_cluster_heatmap(data, cluster = "cluster", show_values = "yes"),
    "`show_values` must be TRUE or FALSE"
  )
})

test_that("plot_cluster_heatmap errors when cluster column is integer", {
  data <- data.frame(
    cluster = rep(c(1L, 2L), each = 5),
    var1 = rnorm(10)
  )
  expect_error(
    plot_cluster_heatmap(data, cluster = "cluster"),
    "numeric"
  )
})

test_that("plot_cluster_heatmap works with factor cluster column", {
  set.seed(1)
  data <- data.frame(
    cluster = factor(rep(c("A", "B"), each = 10)),
    var1 = rnorm(20),
    var2 = rnorm(20)
  )
  p <- plot_cluster_heatmap(data, cluster = "cluster")
  expect_s3_class(p, "ggplot")
})
