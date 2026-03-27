test_that("plot_group_density returns a named list by default", {
  set.seed(1)
  data <- data.frame(
    grp = rep(paste0("G", 1:3), each = 20),
    var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2)),
    var2 = c(rnorm(20, -1), rnorm(20, 1), rnorm(20, 0))
  )
  result <- plot_group_density(data, group = "grp")
  expect_type(result, "list")
  expect_named(result, c("var1", "var2"))
})

test_that("plot_group_density list elements are ggplot objects", {
  set.seed(1)
  data <- data.frame(
    grp = rep(paste0("G", 1:3), each = 20),
    var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2)),
    var2 = c(rnorm(20, -1), rnorm(20, 1), rnorm(20, 0))
  )
  result <- plot_group_density(data, group = "grp")
  for (p in result) expect_s3_class(p, "ggplot")
})

test_that("plot_group_density colour label is Group", {
  set.seed(1)
  data <- data.frame(
    grp = rep(paste0("G", 1:3), each = 20),
    var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2)),
    var2 = c(rnorm(20, -1), rnorm(20, 1), rnorm(20, 0))
  )
  result <- plot_group_density(data, group = "grp", density = "overall")
  expect_equal(result[["var1"]]$labels$colour, "Group")
})

test_that("plot_cluster_density is an alias for plot_group_density", {
  set.seed(1)
  data <- data.frame(
    cluster = rep(paste0("C", 1:3), each = 20),
    var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2)),
    var2 = c(rnorm(20, -1), rnorm(20, 1), rnorm(20, 0))
  )
  result <- plot_cluster_density(data, cluster = "cluster")
  expect_type(result, "list")
  for (p in result) expect_s3_class(p, "ggplot")
})
