#' @md
#' @title Add a column of text to a ggplot
#'
#' @description
#' Adds a column of text to a ggplot object, overlaying the text at specified 
#' coordinates with customizable transformation, font size, and alignment.
#'
#' @param p ggplot object. Plot to have text overlaid onto.
#' @param x,y numeric vector. The min and max of these vectors specify the range 
#'   of the plot.
#' @param trans character or trans object. Specifies the (visual) transformation 
#'   applied to the y-axis. Internally passed to `get_trans`. Default is "identity".
#'   Note that this transformation is NOT applied to the y-axis (the assumption is
#'   that it already has been or will be).
#' @param text character vector. Each element specifies a line of text. 
#'   Subsequent elements are plotted underneath (assuming skip >= 0).
#' @param coord numeric vector of length two. The elements specify the x- and 
#'   y-coordinates, respectively, for the first line of text (i.e. the first element) 
#'   of `text`. `c(0, 0)` means the bottom-left corner and `c(1, 1)` the top-right.
#'   There is no problem with specifying values outside `[0, 1]`, however. 
#'   Default is `c(0.05, 0.95)` (i.e. towards top-left corner).
#' @param skip numeric. Proportion of y-axis length to skip between rows. Default is 0.06.
#' @param font_size numeric. Font size. Default is 4.2 (roughly size 12).
#' @param hjust,vjust numeric. Passed to `hjust` and `vjust` parameters of 
#'   `geom_text`. Defaults are `0` and `0.5`, respectively.
#' @param ... Additional parameters passed to `geom_text`.
#'
#' @return A ggplot object with the added text.
#'
#' @examples
#' data_mod <- data.frame(x = rnorm(10))
#' data_mod$y <- data_mod$x * 3 + rnorm(10, sd = 0.5)
#' fit <- lm(y ~ x, data = data_mod)
#' coef_tbl <- coefficients(summary(fit))
#' results_vec <- c(
#'   paste0(
#'     "Intercept: ", signif(coef_tbl[1, "Estimate"][[1]], 2), " (",
#'     signif(coef_tbl[1, 1][[1]] - 2 * coef_tbl[1, 2][[1]], 3), ", ",
#'     signif(coef_tbl[1, 1][[1]] + 2 * coef_tbl[1, 2][[1]], 3), "; p = ",
#'     signif(coef_tbl[1, 4][[1]], 3), ")"
#'   ),
#'   paste0(
#'     "Slope: ", signif(coef_tbl[2, "Estimate"][[1]], 2), " (",
#'     signif(coef_tbl[2, 1][[1]] - 2 * coef_tbl[2, 2][[1]], 3), ", ",
#'     signif(coef_tbl[2, 1][[1]] + 2 * coef_tbl[2, 2][[1]], 3), "; p = ",
#'     signif(coef_tbl[2, 4][[1]], 3), ")"
#'   )
#' )
#' p <- ggplot(data_mod, aes(x = x, y = y)) + geom_point()
#' add_text_column(
#'   p = p,
#'   x = data_mod$x,
#'   y = data_mod$y,
#'   text = results_vec
#' )
#'
#' # Works even if y-axis is transformed
#' p <- p + scale_y_continuous(trans = get_trans("asinh"))
#' add_text_column(
#'   p = p,
#'   x = data_mod$x,
#'   y = data_mod$y,
#'   text = results_vec,
#'   trans = "asinh"
#' )
#' @export
add_text_column <- function(p, x, y, trans = "identity", text,
                            coord = c(0.05, 0.95), skip = 0.06,
                            font_size = 4.2, hjust = 0, vjust = 0.5, ...) {
  # Prepare transformation
  trans <- get_trans(trans)
  
  # Calculate limits
  lim_vec_y_orig <- range(y)
  lim_vec_y_trans <- trans$transform(lim_vec_y_orig)
  length_axis_y_trans <- diff(lim_vec_y_trans)
  lim_vec_x_orig <- range(x)
  lim_vec_x_trans <- trans$transform(lim_vec_x_orig)
  length_axis_x_trans <- diff(lim_vec_x_trans)
  
  # Calculate text table
  plot_tbl_text <- purrr::map_df(seq_along(text), function(i) {
    y_trans <- lim_vec_y_trans[1] + coord[2] * length_axis_y_trans -
      skip * (i - 1) * length_axis_y_trans
    y <- trans$inverse(y_trans)
    x_trans <- lim_vec_x_trans[1] + coord[1] * length_axis_x_trans
    x <- trans$inverse(x_trans)
    tibble::tibble(x = x, y = y, txt = text[[i]])
  })
  
  p + geom_text(
    data = plot_tbl_text,
    mapping = aes(x = x, y = y, label = txt),
    hjust = hjust, vjust = vjust,
    size = font_size,
    inherit.aes = FALSE,
    ...
  )
}


#' @title Get transformation object
#'
#' @description
#' Converts a character string specifying a transformation into a `trans` object,
#' or returns the input if it is already a `trans` object. Supports various 
#' root, log, and other transformations.
#'
#' @param trans character or trans object. If class is character, it is converted 
#'   to a trans object. Adds "root_cube", "root_fourth", "root_fifth" and "asinh"
#'   transformations, as well as "sqrt" transformation that allows plotting of
#'   lines at zero. If class is a trans object, it is returned as is.
#'
#' @return A `trans` object corresponding to the specified transformation.
#'
#' @examples
#' x_vec <- seq(1, 5, length.out = 1000)
#' y_vec <- get_trans("root_fifth")$transform(x_vec)
#' plot_tbl <- data.frame(x = x_vec, y = y_vec)
#' ggplot(plot_tbl, aes(x, y)) +
#'   geom_line() +
#'   geom_point() +
#'   coord_equal() +
#'   expand_limits(x = 5, y = 5)
#' @export
get_trans <- function(trans) {
  switch(class(trans)[1],
    "character" = switch(trans,
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
    "transform" = trans,
    stop(paste0("class ", class(trans)[1], " not recognised"))
  )
}
