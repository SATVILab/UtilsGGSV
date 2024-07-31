test_that("ggcorr works", {
  set.seed(2106)
  response_vec_a <- rnorm(5)
  response_tbl <- data.frame(
    group = rep(letters[1:3], each = 5),
    response = c(
      response_vec_a,
      response_vec_a * 1.2 + rnorm(5, sd = 0.2),
      response_vec_a * 2 + rnorm(5, sd = 2)
    ),
    pid = rep(paste0("id_", 1:5), 3)
  )

  path_fig_save <- "/workspaces/UtilsGGSV/_tmp/test-ggcorr.png"
  ggsavetest <- function(p = p_test, path_fig_save_par = path_fig_save) { # nolint
    try(ggsave(path_fig_save_par, p_test, units = "cm", width = 10, height = 10), silent = TRUE)
  }

  p_test <- ggcorr(
    data = response_tbl,
    grp = "group",
    y = "response",
    id = "pid"
  )
  
  p_test <- ggcorr(
    data = response_tbl,
    grp = "group",
    corr_method = "pearson",
    y = "response",
    id = "pid"
  )
  
  p_test <- ggcorr(
    data = response_tbl,
    grp = "group",
    corr_method = "kendall",
    y = "response",
    id = "pid"
  )
  
  p_test <- ggcorr(
    data = response_tbl,
    grp = "group",
    corr_method = c("concordance"),
    y = "response",
    id = "pid"
  )
  p_test <- ggcorr(
    data = response_tbl,
    grp = "group",
    corr_method = c("concordance", "pearson"),
    y = "response",
    id = "pid"
  )
  

  expect_s3_class(p_test, "gg")

  # only two groups
  p_test <- ggcorr(
    data = response_tbl |>
      dplyr::filter(group != "c"),
    grp = "group",
    y = "response",
    id = "pid"
  )

  expect_identical(
    as.character(p_test$labels$y),
    "b"
  )

  # labelling ids
  p_test <- ggcorr(
    data = response_tbl |>
      dplyr::filter(group != "c"),
    grp = "group",
    y = "response",
    id = "pid",
    label_id = TRUE
  )

  expect_identical(
    length(p_test$layers),
    5L
  )

  # test adding the abline
  p_test <- ggcorr(
    data = response_tbl |>
      dplyr::filter(group != "c"),
    grp = "group",
    y = "response",
    id = "pid",
    abline = FALSE
  )

  expect_identical(
    length(p_test$layers),
    4L
  )

  p_test <- ggcorr(
    data = response_tbl |>
      dplyr::filter(group != "c"),
    grp = "group",
    y = "response",
    id = "pid",
    smooth = FALSE
  )
  
  expect_identical(
    length(p_test$layers),
    3L
  )

  # loess smooth
  p_test <- ggcorr(
    data = response_tbl |>
      dplyr::filter(group != "c"),
    grp = "group",
    y = "response",
    id = "pid",
    smooth = TRUE,
    smooth_method = "loess"
  )
  expect_identical(
    length(p_test$layers),
    4L
  )

  p_test <- ggcorr(
    data = response_tbl |>
      dplyr::filter(group != "c"),
    grp = "group",
    y = "response",
    id = "pid",
    limits_equal = TRUE
  )
  expect_identical(
    p_test$layers[[2]]$data$x,
    p_test$layers[[2]]$data$y
  )
})
