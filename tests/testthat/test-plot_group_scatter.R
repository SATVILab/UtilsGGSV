test_that("plot_group_scatter returns a ggplot object", {
  set.seed(1)
  data <- data.frame(
    grp = rep(paste0("G", 1:3), each = 20),
    var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2)),
    var2 = c(rnorm(20, -1), rnorm(20, 1), rnorm(20, 0)),
    var3 = c(rnorm(20, 1), rnorm(20, -1), rnorm(20, 0))
  )
  result <- plot_group_scatter(data, group = "grp")
  expect_s3_class(result, "ggplot")
})

test_that("plot_group_scatter fill label is Group", {
  set.seed(1)
  data <- data.frame(
    grp = rep(paste0("G", 1:3), each = 20),
    var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2)),
    var2 = c(rnorm(20, -1), rnorm(20, 1), rnorm(20, 0)),
    var3 = c(rnorm(20, 1), rnorm(20, -1), rnorm(20, 0))
  )
  result <- plot_group_scatter(data, group = "grp")
  expect_equal(result$labels$fill, "Group")
})

test_that("plot_cluster_scatter is an alias for plot_group_scatter", {
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
