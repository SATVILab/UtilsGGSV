# Get transformation object

Converts a character string specifying a transformation into a `trans`
object, or returns the input if it is already a `trans` object. Supports
various root, log, and other transformations.

## Usage

``` r
get_trans(trans)
```

## Arguments

- trans:

  character or trans object. If class is character, it is converted to a
  trans object. Adds "root_cube", "root_fourth", "root_fifth" and
  "asinh" transformations, as well as "sqrt" transformation that allows
  plotting of lines at zero. If class is a trans object, it is returned
  as is.

## Value

A `trans` object corresponding to the specified transformation.

## Examples

``` r
x_vec <- seq(1, 5, length.out = 1000)
y_vec <- get_trans("root_fifth")$transform(x_vec)
plot_tbl <- data.frame(x = x_vec, y = y_vec)
library(ggplot2)
ggplot(plot_tbl, aes(x, y)) +
  geom_line() +
  geom_point() +
  coord_equal() +
  expand_limits(x = 5, y = 5)
```
