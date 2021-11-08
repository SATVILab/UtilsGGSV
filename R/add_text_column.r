#' @title Add a column of text
#'
#' @param p \code{ggplot} object.
#' Plot to have text overlaid onto.
#' @param x,y numeric vector.
#' The min and max of these vectors specify the range of the plot.
#' @param trans character or trans object.
#' Specifies the (visual) transformation applied to the y-axis.
#' Internally passed to \code{ggutils::get_trans}, so see
#' \code{?ggutils::get_trans} for further details regarding
#' possible values. Default is "identity".
#' Note that this transformation is NOT applied to the
#' y-axis (the assumption is that it already has been, or will be).
#' @param text character vector.
#' Each element specifies a line of text.
#' Subsequent elements are plotted underneath (assuming
#' skip >= 0).
#' @param coord numeric vector of length two.
#' The elements specify the x- and y-coordinates, respectively,
#' for the first line of text (i.e. the first element) of \code{text}.
#' \code(c(0, 0)} means the bottom-left corner and
#' \code{c(0, 0)} the top-right.
#' There is no problem with specifying values outside \code{[0, 1]},
#' however.
#' Default is \code{c(0.1, 0.95)} (i.e. towards top-left corner).
#' @param skip numeric.
#' Proportion of y-axis length to skip between rows.
#' Default is 0.05.
#' @param font_size numeric. Font size.
#' Default is 4.2 (roughly size 12).
#' @param align_symbol character.
#' Symbol around which to align the text.
#' Not implemented at present.
#' @param expand_limits_y numeric.
#' Proportion of y-axis limits to expand y-axis by.
#' Default is 0.
#' @param limits_equal logical.
#' If \code{TRUE}, then x- and y-limits are forced
#' to be equal.
#' Default is \code{FALSE}.
#' @param hjust,vjust numeric.
#' Passed onto \code{hjust} and \code{vjust} parameters, respectively,
#' of \code{geom_text}. Defaults are \code{0} and \code{0.5}, respectively.
#'
#' @examples
#' data_mod <- data.frame(x = rnorm(10))
#' data_mod$y <- data_mod$x * 3 + rnorm(10, sd = 0.5)
#' fit <- lm(y ~ x, data = data_mod)
#' coef_tbl <- coefficients(summary(fit))
#' results_vec <- c(
#'    paste0(
#'      "Intercept: ",
#'      signif(coef_tbl[1, "Estimate"][[1]], 2),
#'      " (",
#'      signif(coef_tbl[1, 1][[1]] - 2 * coef_tbl[1, 2][[1]], 3),
#'      ", ",
#'      signif(coef_tbl[1, 1][[1]] + 2 * coef_tbl[1, 2][[1]], 3),
#'      "; p = ",
#'      signif(coef_tbl[1, 4][[1]], 3),
#'      ")"
#'      ),
#'    paste0(
#'      "Slope: ",
#'      signif(coef_tbl[2, "Estimate"][[1]], 2),
#'      " (",
#'      signif(coef_tbl[2, 1][[1]] - 2 * coef_tbl[2, 2][[1]], 3),
#'      ", ",
#'      signif(coef_tbl[2, 1][[1]] + 2 * coef_tbl[2, 2][[1]], 3),
#'      "; p = ",
#'      signif(coef_tbl[2, 4][[1]], 3),
#'     ")"
#'     )
#' )
#' p <- ggplot(
#'   data = data_mod, 
#'   aes(x = x, y = y)
#' ) +
#'   geom_point()
#' add_text_column(
#'   p = p,
#'   x = data_mod$x,
#'   y = data_mod$y,
#'   text = results_vec)
#' 
#' # works even if y-axis is transformed
#' p <- p + 
#'   scale_y_continuous(
#'     trans = ggutils::get_trans("asinh")
#'  )
#' add_text_column(
#'   p = p,
#'   x = data_mod$x,
#'   y = data_mod$y,
#'   text = results_vec, 
#'   trans =  ggutils::get_trans("asinh")
#' )
#' @export
add_text_column <- function(p, x, y, trans = "identity",
                            text, coord = c(0.1, 0.9),
                            skip = 0.05, font_size = 4.2,
                            align_symbol = NULL,
                            hjust = 0,
                            vjust = 0.5,
                            expand_limits_y = 0,
                            limits_equal = FALSE) {

  # prepare transformation
  trans <- get_trans(trans = trans) # nolint

  # calculate limits
  lim_vec_y_orig <- range(y)
  lim_vec_y_trans <- trans$transform(lim_vec_y_orig)
  length_axis_y_trans <- diff(lim_vec_y_trans)
  lim_vec_x_orig <- range(x)
  lim_vec_x_trans <- trans$transform(lim_vec_x_orig)
  length_axis_x_trans <- diff(lim_vec_x_trans)

  # calculate text tbl
  if (is.null(align_symbol)) {
    plot_tbl_text <- purrr::map_df(seq_along(text), function(i) {
      txt <- text[[i]]
      y_trans <- lim_vec_y_trans[1] +
        coord[2] * length_axis_y_trans -
        skip * (i - 1) * length_axis_y_trans
      y <- trans$inverse(y_trans)
      x_trans <- lim_vec_x_trans[1] +
        coord[1] * length_axis_x_trans
      x <- trans$inverse(x_trans)
      tibble::tibble(
        x = x,
        y = y,
        txt = txt
      )
    })
  } else {
    stop("align_symbol cannot be non-NULL yet")
  }

  if (expand_limits_y) {
    stop("expand_limits_y cannot be non-zero yet")
  }

  if (limits_equal) {
    stop("limits_equal cannot be TRUE yet")
  }

  p +
    geom_text(
      data = plot_tbl_text,
      mapping = aes(x = x, y = y, label = txt),
      hjust = hjust, vjust = vjust,
      size = font_size, 
      inherit.aes = FALSE
    )
}

#' @title Get trans object from character
#'
#' @param trans character or trans object.
#' If class is character, then it is converted to a trans object.
#' Adds "root_cube", "root_fourth", "root_fifth" and
#' "asinh" transformations, as well as "sqrt" transformation
#' that allows plotting of lines at zero.
#' If class is a trans object, then it is returned as is.
#'
#' @export
#' @examples
#'  x_vec <- seq(1, 5, length.out = 1e3)
#'  y_vec <- ggutils::get_trans("root_fifth")$transform(seq_vec)
#'  plot_tbl <- data.frame(x = x_vec, y = y_vec)
#'  ggplot(
#'    plot_tbl,
#'    aes(x, y)
#'   ) +
#'   geom_line() +
#'   geom_point() +
#'   coord_equal() +
#'   expand_limits(x = 5, y = 5)
get_trans <- function(trans) {
  switch(
    class(trans)[1],
    "character" = switch(
      trans,
      "root_2" = , # nolint
      "root_quadratic" = , # nolint
      "root_quad" = , # nolint
      "root_second" = , # nolint
      "root_two" = , # nolint
      "sqrt" = scales::trans_new(
        "root_2",
        transform = function(x) sqrt(x),
        inverse = function(x) ifelse(x <= 0, 0, x^2)
      ),
      "root_3" = , # nolint
      "root_three" = , # nolint
      "root_third" = , # nolint
      "root_cube" = scales::trans_new(
        "root_3",
        transform = function(x) x^(1 / 3),
        inverse = function(x) ifelse(x <= 0, 0, x^3)
      ),
      "root_4" = , # nolint
      "root_four" = , # nolint
      "root_fourth" = scales::trans_new(
        "root_4",
        transform = function(x) x^0.25,
        inverse = function(x) ifelse(x <= 0, 0, x^4)
      ),
      "root_5" = , # nolint
      "root_five" = , # nolint
      "root_fifth" = scales::trans_new(
        "root_5",
        transform = function(x) x^0.2,
        inverse = function(x) ifelse(x <= 0, 0, x^5)
      ),
      "identity" = scales::identity_trans(),
      "log10" = scales::log10_trans(),
      "log2" = scales::log2_trans(),
      "log1p" = scales::log1p(),
      "log" = scales::log(),
      "psueudo_log" = scales::pseudo_log_trans(),
      "asinh" = scales::trans_new(
        "asinh",
        transform = asinh,
        inverse = sinh
      ),
      "asn" = scales::asn_trans(),
      "boxcox" = scales::boxcox_trans(),
      "exp" = scales::exp_trans(),
      "reciprocal" = scales::reciprocal_trans(),
      "probit" = scales::probit_trans(),
      "probability" = scales::probability_trans(),
      "modulus" = scales::modulus_trans()
    ),
    "trans" = trans,
    stop(paste0("class ", class(trans)[1], " not recognised"))
  )
}