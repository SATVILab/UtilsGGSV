# Plot heat map of ECDF-standardised variable values per cluster

Creates a heat map where each tile shows the percentile of the median
value of a variable for a cluster, compared against the empirical
cumulative distribution function (ECDF) of that variable across all
observations not belonging to the cluster. Clusters and variables are
ordered along the axes via hierarchical clustering.

## Usage

``` r
plot_cluster_heatmap(
  data,
  cluster,
  vars = NULL,
  col_high = "#B2182B",
  col_mid = "#F7F7F7",
  col_low = "#2166AC",
  white_range = c(0.4, 0.6),
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

- col_high:

  character. Colour for high values (100th percentile). Default is
  `"#B2182B"`.

- col_mid:

  character. Colour for the middle of the value range. Default is
  `"#F7F7F7"`.

- col_low:

  character. Colour for low values (0th percentile). Default is
  `"#2166AC"`.

- white_range:

  numeric vector of length 2. The range of percentile values (on a 0-1
  scale) that map to `col_mid`. Default is `c(0.4, 0.6)`.

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
plot_cluster_heatmap(data, cluster = "cluster")
```
