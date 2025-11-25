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

test_that("add_text_column with custom parameters works", {
  plot_tbl <- data.frame(
    y = 1:10,
    x = 1:10
  )
  p <- ggplot(
    plot_tbl,
    aes(x, y)
  ) +
    geom_point()

  # Test with custom coord
  p_custom_coord <- add_text_column(
    p = p,
    x = plot_tbl$x,
    y = plot_tbl$y,
    text = c("Line 1"),
    coord = c(0.5, 0.5)
  )
  expect_s3_class(p_custom_coord, c("gg", "ggplot"))

  # Test with custom skip
  p_custom_skip <- add_text_column(
    p = p,
    x = plot_tbl$x,
    y = plot_tbl$y,
    text = c("Line 1", "Line 2", "Line 3"),
    skip = 0.1
  )
  expect_s3_class(p_custom_skip, c("gg", "ggplot"))

  # Test with custom font_size
  p_custom_font <- add_text_column(
    p = p,
    x = plot_tbl$x,
    y = plot_tbl$y,
    text = c("Big text"),
    font_size = 6
  )
  expect_s3_class(p_custom_font, c("gg", "ggplot"))

  # Test with custom hjust and vjust
  p_custom_just <- add_text_column(
    p = p,
    x = plot_tbl$x,
    y = plot_tbl$y,
    text = c("Justified text"),
    hjust = 0.5,
    vjust = 1
  )
  expect_s3_class(p_custom_just, c("gg", "ggplot"))
})

test_that("add_text_column works with different transformations", {
  plot_tbl <- data.frame(
    y = 1:10,
    x = 1:10
  )
  p <- ggplot(
    plot_tbl,
    aes(x, y)
  ) +
    geom_point()

  # Test with log10 transformation
  p_log10 <- add_text_column(
    p = p +
      scale_y_continuous(trans = get_trans("log10")),
    x = plot_tbl$x,
    y = plot_tbl$y,
    text = c("Log scale"),
    trans = "log10"
  )
  expect_s3_class(p_log10, c("gg", "ggplot"))

  # Test with sqrt transformation
  p_sqrt <- add_text_column(
    p = p +
      scale_y_continuous(trans = get_trans("sqrt")),
    x = plot_tbl$x,
    y = plot_tbl$y,
    text = c("Sqrt scale"),
    trans = "sqrt"
  )
  expect_s3_class(p_sqrt, c("gg", "ggplot"))

  # Test with trans object passed directly
  trans_obj <- scales::identity_trans()
  p_trans_obj <- add_text_column(
    p = p,
    x = plot_tbl$x,
    y = plot_tbl$y,
    text = c("Trans object"),
    trans = trans_obj
  )
  expect_s3_class(p_trans_obj, c("gg", "ggplot"))
})
