test_that("add_text_column works", {
  plot_tbl <- data.frame(
    y = c(seq(-3, 0)^2, 1:3),
    x = seq(-0.5, 1.25, length.out = 7)
  )
  p <- ggplot(
    plot_tbl,
    aes(x, y)
  ) +
    geom_point()

  p_identity <- add_text_column(
    p = p,
    x = plot_tbl$x,
    y = plot_tbl$y,
    text = c("p: 1", "q: 2")
  )
  expect_s3_class(p_identity, c("gg", "ggplot"))

  p_asinh <- add_text_column(
    p = p +
      scale_y_continuous(
        trans = get_trans("asinh")
      ),
    x = plot_tbl$x,
    y = plot_tbl$y,
    text = c("p: 1", "q: 2"),
    trans = "asinh"
  )
  expect_s3_class(p_asinh, c("gg", "ggplot"))
})
