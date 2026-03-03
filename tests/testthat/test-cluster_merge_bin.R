test_that("cluster_merge_bin returns a list with assign and label", {
  set.seed(1)
  mat <- matrix(
    c(rnorm(20, 2), rnorm(20, -2)),
    ncol = 2,
    dimnames = list(NULL, c("var1", "var2"))
  )
  cl <- rep(c("A", "B", "C", "D"), each = 5)
  result <- cluster_merge_bin(mat, cl, list(var1 = 0, var2 = 0))
  expect_type(result, "list")
  expect_named(result, c("assign", "label"))
})

test_that("cluster_merge_bin assign has correct structure", {
  set.seed(1)
  mat <- matrix(
    c(rnorm(20, 2), rnorm(20, -2)),
    ncol = 2,
    dimnames = list(NULL, c("var1", "var2"))
  )
  cl <- rep(c("A", "B", "C", "D"), each = 5)
  result <- cluster_merge_bin(mat, cl, list(var1 = 0, var2 = 0))
  expect_s3_class(result$assign, "tbl_df")
  expect_named(result$assign, c("orig", "merged"))
  expect_equal(nrow(result$assign), length(cl))
})

test_that("cluster_merge_bin label has correct structure", {
  set.seed(1)
  mat <- matrix(
    c(rnorm(20, 2), rnorm(20, -2)),
    ncol = 2,
    dimnames = list(NULL, c("var1", "var2"))
  )
  cl <- rep(c("A", "B", "C", "D"), each = 5)
  result <- cluster_merge_bin(mat, cl, list(var1 = 0, var2 = 0))
  expect_s3_class(result$label, "tbl_df")
  expect_named(result$label, c("orig", "level", "descriptive"))
  expect_equal(nrow(result$label), length(unique(cl)))
})

test_that("cluster_merge_bin merges clusters with same bin combination", {
  mat <- matrix(
    c(1, 1, -1, -1, 1, 1, -1, -1,
      1, 1, -1, -1, 1, 1, -1, -1),
    nrow = 8,
    ncol = 2,
    dimnames = list(NULL, c("var1", "var2"))
  )
  cl <- c("A", "A", "B", "B", "C", "C", "D", "D")
  result <- cluster_merge_bin(mat, cl, list(var1 = 0, var2 = 0))
  merged_A <- unique(result$assign$merged[result$assign$orig == "A"])
  merged_C <- unique(result$assign$merged[result$assign$orig == "C"])
  expect_equal(merged_A, merged_C)
})

test_that("cluster_merge_bin keeps clusters separate when bins differ", {
  mat <- matrix(
    c(1, 1, -1, -1, 1, 1, -1, -1,
      1, 1, -1, -1, 1, 1, -1, -1),
    nrow = 8,
    ncol = 2,
    dimnames = list(NULL, c("var1", "var2"))
  )
  cl <- c("A", "A", "B", "B", "C", "C", "D", "D")
  result <- cluster_merge_bin(mat, cl, list(var1 = 0, var2 = 0))
  merged_A <- unique(result$assign$merged[result$assign$orig == "A"])
  merged_B <- unique(result$assign$merged[result$assign$orig == "B"])
  expect_false(identical(merged_A, merged_B))
})

test_that("cluster_merge_bin level uses bin indices joined by underscore", {
  mat <- matrix(
    c(1, -1, 5),
    nrow = 3,
    ncol = 1,
    dimnames = list(NULL, "var1")
  )
  cl <- c("A", "B", "C")
  result <- cluster_merge_bin(mat, cl, list(var1 = c(0, 3)))
  levels <- result$label$level
  expect_equal(levels[result$label$orig == "A"], "2")
  expect_equal(levels[result$label$orig == "B"], "1")
  expect_equal(levels[result$label$orig == "C"], "3")
})

test_that("cluster_merge_bin level is underscore-joined for multiple vars", {
  mat <- matrix(
    c(1, 1),
    nrow = 1,
    ncol = 2,
    dimnames = list(NULL, c("var1", "var2"))
  )
  cl <- "A"
  result <- cluster_merge_bin(mat, cl, list(var1 = 0, var2 = 0))
  expect_equal(result$label$level, "2_2")
})

test_that("cluster_merge_bin descriptive describes bin boundaries", {
  mat <- matrix(
    c(-1, 1, 5),
    nrow = 3,
    ncol = 1,
    dimnames = list(NULL, "var1")
  )
  cl <- c("low", "mid", "high")
  result <- cluster_merge_bin(mat, cl, list(var1 = c(0, 3)))
  desc <- result$label$descriptive
  expect_equal(desc[result$label$orig == "low"], "var1 <= 0")
  expect_equal(desc[result$label$orig == "mid"], "0 < var1 <= 3")
  expect_equal(desc[result$label$orig == "high"], "var1 > 3")
})

test_that("cluster_merge_bin works with a data.frame input", {
  df <- data.frame(var1 = c(1, -1), var2 = c(1, -1))
  cl <- c("A", "B")
  result <- cluster_merge_bin(df, cl, list(var1 = 0, var2 = 0))
  expect_s3_class(result$assign, "tbl_df")
  expect_equal(nrow(result$assign), 2L)
})

test_that("cluster_merge_bin orig in assign matches input cluster", {
  mat <- matrix(
    c(1, -1),
    nrow = 2,
    ncol = 1,
    dimnames = list(NULL, "var1")
  )
  cl <- c("X", "Y")
  result <- cluster_merge_bin(mat, cl, list(var1 = 0))
  expect_equal(result$assign$orig, cl)
})
