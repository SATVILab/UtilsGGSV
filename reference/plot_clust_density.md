# Plot density of variable values with per-cluster median lines

For each variable, plots the overall density of values across all
observations and overlays a vertical line for each cluster at that
cluster's median value for the variable. Each cluster is given a
distinct colour. When multiple variables are provided the plots are
arranged in a faceted grid with free scales.

## Usage

``` r
plot_clust_density(
  data,
  cluster,
  vars = NULL,
  font_size = 14,
  thm = cowplot::theme_cowplot(font_size = font_size) + ggplot2::theme(plot.background =
    ggplot2::element_rect(fill = "white", colour = NA), panel.background =
    ggplot2::element_rect(fill = "white", colour = NA)),
  grid = cowplot::background_grid(major = "xy")
)
```

## Arguments

- data:

  data.frame. Rows are observations. Must contain a column identifying
  cluster membership and columns for variable values.

- cluster:

  character. Name of the column in `data` that identifies cluster
  membership.

- vars:

  character vector or `NULL`. Names of columns in `data` to use as
  variables. If `NULL`, all columns except `cluster` are used. Default
  is `NULL`.

- font_size:

  numeric. Font size passed to
  [`cowplot::theme_cowplot`](https://wilkelab.org/cowplot/reference/theme_cowplot.html).
  Default is `14`.

- thm:

  ggplot2 theme object or `NULL`. Default is
  `cowplot::theme_cowplot(font_size = font_size)` with a white plot
  background. Set to `NULL` to apply no theme adjustment.

- grid:

  ggplot2 panel grid or `NULL`. Default is
  `cowplot::background_grid(major = "xy")`. Set to `NULL` for no grid.

## Value

A ggplot object.

## Examples

``` r
set.seed(1)
data <- data.frame(
  cluster = rep(paste0("C", 1:3), each = 20),
  var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2)),
  var2 = c(rnorm(20, -1), rnorm(20, 1), rnorm(20, 0))
)
plot_clust_density(data, cluster = "cluster")
#> Warning: Removed 6 rows containing missing values or values outside the scale range
#> (`geom_vline()`).
```
