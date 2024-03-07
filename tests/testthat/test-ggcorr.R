test_that("ggcorr works", {
  browser()
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

  p_std <- ggcorr(
    data = response_tbl,
    grp = "group",
    y = "response",
    id = "pid"
  )
  p_std <- ggcorr(
    data = response_tbl,
    grp = "group",
    corr_method = "pearson",
    y = "response",
    id = "pid"
  )
  p_std <- ggcorr(
    data = response_tbl,
    grp = "group",
    corr_method = c("ccc", "pearson"),
    y = "response",
    id = "pid"
  )

  expect_s3_class(
    p_std,
    "gg"
  )

  p_group_2 <- ggcorr(
    data = response_tbl %>%
      dplyr::filter(group != "c"),
    grp = "group",
    y = "response",
    id = "pid"
  )

  expect_identical(
    as.character(p_group_2$labels$y),
    "b"
  )

  p_lab_id <- ggcorr(
    data = response_tbl %>%
      dplyr::filter(group != "c"),
    grp = "group",
    y = "response",
    id = "pid",
    label_id = TRUE
  )

  expect_identical(
    length(p_lab_id$layers),
    6L
  )

  p_abline <- ggcorr(
    data = response_tbl %>%
      dplyr::filter(group != "c"),
    grp = "group",
    y = "response",
    id = "pid",
    abline = FALSE
  )

  expect_identical(
    length(p_abline$layers),
    4L
  )

  p_smooth <- ggcorr(
    data = response_tbl %>%
      dplyr::filter(group != "c"),
    grp = "group",
    y = "response",
    id = "pid",
    smooth = FALSE
  )
  expect_identical(
    length(p_smooth$layers),
    4L
  )
  p_smooth_loess <- ggcorr(
    data = response_tbl %>%
      dplyr::filter(group != "c"),
    grp = "group",
    y = "response",
    id = "pid",
    smooth = TRUE,
    smooth_method = "loess"
  )
  expect_identical(
    length(p_smooth_loess$layers),
    5L
  )

  p_equal <- ggcorr(
    data = response_tbl %>%
      dplyr::filter(group != "c"),
    grp = "group",
    y = "response",
    id = "pid",
    limits_equal = TRUE
  )
  expect_identical(
    p_equal$layers[[2]]$data$x,
    p_equal$layers[[2]]$data$y
  )
})
