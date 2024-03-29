% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add_text_column.r
\name{add_text_column}
\alias{add_text_column}
\title{Add a column of text}
\usage{
add_text_column(
  p,
  x,
  y,
  trans = "identity",
  text,
  coord = c(0.05, 0.95),
  skip = 0.06,
  font_size = 4.2,
  align_symbol = NULL,
  hjust = 0,
  vjust = 0.5,
  expand_limits_y = 0,
  limits_equal = FALSE,
  ...
)
}
\arguments{
\item{p}{\code{ggplot} object.
Plot to have text overlaid onto.}

\item{x, y}{numeric vector.
The min and max of these vectors specify the range of the plot.}

\item{trans}{character or trans object.
Specifies the (visual) transformation applied to the y-axis.
Internally passed to \code{UtilsGGSV::get_trans}, so see
\code{?UtilsGGSV::get_trans} for further details regarding
possible values. Default is "identity".
Note that this transformation is NOT applied to the
y-axis (the assumption is that it already has been, or will be).}

\item{text}{character vector.
Each element specifies a line of text.
Subsequent elements are plotted underneath (assuming
skip >= 0).}

\item{skip}{numeric.
Proportion of y-axis length to skip between rows.
Default is 0.6.}

\item{font_size}{numeric. Font size.
Default is 4.2 (roughly size 12).}

\item{align_symbol}{character.
Symbol around which to align the text.
Not implemented at present.}

\item{hjust, vjust}{numeric.
Passed onto \code{hjust} and \code{vjust} parameters, respectively,
of \code{geom_text}. Defaults are \code{0} and \code{0.5}, respectively.}

\item{...}{Passed to \code{geom_text}.}
}
\description{
Add a column of text
}
\examples{
data_mod <- data.frame(x = rnorm(10))
data_mod$y <- data_mod$x * 3 + rnorm(10, sd = 0.5)
fit <- lm(y ~ x, data = data_mod)
coef_tbl <- coefficients(summary(fit))
results_vec <- c(
  paste0(
    "Intercept: ",
    signif(coef_tbl[1, "Estimate"][[1]], 2),
    " (",
    signif(coef_tbl[1, 1][[1]] - 2 * coef_tbl[1, 2][[1]], 3),
    ", ",
    signif(coef_tbl[1, 1][[1]] + 2 * coef_tbl[1, 2][[1]], 3),
    "; p = ",
    signif(coef_tbl[1, 4][[1]], 3),
    ")"
  ),
  paste0(
    "Slope: ",
    signif(coef_tbl[2, "Estimate"][[1]], 2),
    " (",
    signif(coef_tbl[2, 1][[1]] - 2 * coef_tbl[2, 2][[1]], 3),
    ", ",
    signif(coef_tbl[2, 1][[1]] + 2 * coef_tbl[2, 2][[1]], 3),
    "; p = ",
    signif(coef_tbl[2, 4][[1]], 3),
    ")"
  )
)
p <- ggplot(
  data = data_mod,
  aes(x = x, y = y)
) +
  geom_point()
add_text_column(
  p = p,
  x = data_mod$x,
  y = data_mod$y,
  text = results_vec
)

# works even if y-axis is transformed
p <- p +
  scale_y_continuous(
    trans = UtilsGGSV::get_trans("asinh")
  )
add_text_column(
  p = p,
  x = data_mod$x,
  y = data_mod$y,
  text = results_vec,
  trans = UtilsGGSV::get_trans("asinh")
)
}
