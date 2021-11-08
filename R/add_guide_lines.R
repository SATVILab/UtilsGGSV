#' @title Add labeled guide-lines to assist cowplot item position
#'
#' @description When using `cowplot::ggdraw` and associated functions, occasionally
#' one needs to be very precise about where to place items. However, it can be difficult
#' to eyeball the exact coordinates, and re-running the entire plot multiple times can be
#' slow. This function draws lines that have their x- or y-dimensions labeled,
#' allowing one to gradually zoom in in on the desired coordinates very accurately and quickly.
#'
#' @param p ggplot object. Object to add plot lines to.
#' @param "x", "y" or "xy". Axi(e)s lines should be perpendicular to.
#' @param range numeric vector. Values should lie between 0 and 1.
#' Specifies minimum and maximum values for lines,
#' with 0 being the left-hand side/bottom
#' and 1 being the right-hand side/bottom.
#' Can be of length one, in which case a single line is drawn at that
#' coordinate (regardless of value of \code{n}).
#' Default is \code{c(0,1)}.
#' @param n positive integer. Number of lines to draw. Default is 5.
#'
#' @return Object of class "gg". However, it cannot be treated as a standard
#' "gg" object, and so any manipulations (adding geoms, changing axis labels, etc.)
#' will not work on this plot.
#'
#' @examples
#'
#' library(ggplot2)
#' p <- ggplot(data.frame(x = 1, y = 1)) +
#'   geom_point()
#' ggutils::add_cp_grid_guide(p = p, signif = 2)
#' @export
add_guide_lines <- function(p, axis = "x", range = c(0, 1),
                            n = 5, signif = 3) {
  min <- min(range)
  max <- max(range)
  if (n == 1) {
    min <- mean(min, max)
    max <- mean(min, max)
  }
  n <- ifelse(min == max, 1, n)
  seq_vec <- seq(min, max, length.out = n)
  seq_vec_01 <- seq(0.05, 0.95, length.out = n)
  axis_vec <- purrr::map_chr(seq_len(stringr::str_length(axis)), function(i) {
    stringr::str_sub(axis, start = i, end = i)
  })
  p <- cowplot::ggdraw() +
    cowplot::draw_plot(p)
  if ("x" %in% axis_vec) {
    y <- c(0, 1)
    for (i in seq_len(n)) {
      p <- p +
        cowplot::draw_line(
          x = rep(seq_vec[i], 2),
          y = y
        ) +
        cowplot::draw_text(
          text = signif(seq_vec[i], signif),
          x = rep(seq_vec[i], 2),
          y = rep(seq_vec_01[i], 2),
        )
    }
  }
  if ("y" %in% axis_vec) {
    x <- c(0, 1)
    for (i in seq_len(n)) {
      p <- p +
        cowplot::draw_line(
          x = x,
          y = rep(seq_vec[i], 2)
        ) +
        cowplot::draw_text(
          text = signif(seq_vec[i], signif),
          x = rep(seq_vec_01[i], 2),
          y = rep(seq_vec[i], 2)
        )
    }
  }
  p
}
