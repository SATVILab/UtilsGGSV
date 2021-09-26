#' @title Add p-values to a plot
#'
#' @param p ggplot object. Object to add p-values to.
#' @param p_txt,q_txt character. P-value and q-value text,
#' e.g. "p = 0.01" and "(q = 0.05)".
#' @param line_x c([0,1], [0,1]). Specify left and right endpoints of line along
#' the x-axis.
#' @param p_nudge,q_nudge c([0,1]). By default, the p and q-values texts are
#' centred at the middle of \code{line_x}. They may be shifted horizontally using
#' these parameters, with positive values shifting them further to the right.
#' @param y c([0,1], [0,1], [0,1]). Specifies vertical coordinates of the
#' p-value, q-value and the line. Note that all three are required, even
#' if the q-value is not used.
#' @param tick_nudge_in [0,1]. Amount by which to push the line's ticks inwards.
#' Default is 0.01. Will often need to be adjusted slightly.
#' @param tick_height [0,1]. Height of ticks. Default is 0.01.
#' @param text_size,line_size positive numeric. Size of text and line, respectively.
#' Defaults ar 14 and 1, respctively.
#'
#' @return Object of class "gg". However, it cannot be treated as a standard
#' "gg" object, and so any manipulations (adding geoms, changing axis labels, etc.)
#' will not work on this object. They should therefore be done before, with the
#' plot afterward consisted as fixed apart from further additions from `cowplot::draw_`
#' functions.
add_p_value <- function(p,
                        p_txt,
                        q_txt = NULL,
                        line_x,
                        p_nudge,
                        q_nudge,
                        y,
                        tick_nudge_in = 0.01,
                        tick_height = 0.01,
                        text_size = 14,
                        line_size = 1) {
  p_x <- mean(line_x) + p_nudge
  q_x <- mean(line_x) + q_nudge
  tick_x <- line_x + c(tick_nudge_in, -tick_nudge_in)
  p <- p +
    cowplot::draw_text(
      text = p_txt,
      x = p_x,
      y = y[1],
      size = text_size
    )
  if (!is.null(q_txt)) {
    p <- p +
      cowplot::draw_text(
        text = q_txt,
        x = p_x,
        y = y[2],
        size = text_size
      )
  }
  p +
    cowplot::draw_line(
      x = line_x,
      y = y[3],
      size = line_size
    ) +
    cowplot::draw_line(
      x = tick_x[1],
      y = c(y[3], y[3] - tick_height),
      size = line_size
    ) +
    cowplot::draw_line(
      x = tick_x[2],
      y = c(y[3], y[3] - tick_height),
      size = line_size
    )
}
