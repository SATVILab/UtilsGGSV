# Plot heat map of scaled variable values per group

Creates a heat map where each tile shows a scaled summary of a variable
for a group. The scaling method is controlled by the `scale_method`
parameter. By default (`scale_method = "ecdf"`), each tile shows the
percentile of the group's median value compared against the empirical
cumulative distribution function (ECDF) of that variable across all
observations not belonging to the group. Groups and variables are
ordered along the axes via hierarchical clustering.

## Usage

``` r
plot_group_heatmap(
  .data,
  group,
  vars = NULL,
  scale_method = "ecdf",
  palette = "bipolar",
  col = c("#2166AC", "#F7F7F7", "#B2182B"),
  col_positions = "auto",
  white_range = c(0.4, 0.6),
  na_rm = TRUE,
  font_size = 14,
  thm = cowplot::theme_cowplot(font_size = font_size) + ggplot2::theme(plot.background =
    ggplot2::element_rect(fill = "white", colour = NA), panel.background =
    ggplot2::element_rect(fill = "white", colour = NA)),
  grid = cowplot::background_grid(major = "xy"),
  show_values = FALSE,
  values_format = NULL,
  values_col = "black",
  values_size = 3
)

plot_cluster_heatmap(.data, cluster, ...)
```

## Arguments

- .data:

  data.frame. Rows are observations. Must contain a column identifying
  group membership and columns for variable values.

- group:

  character. Name of the column in `.data` that identifies group
  membership.

- vars:

  character vector or `NULL`. Names of columns in `.data` to use as
  variables. If `NULL`, all columns except `group` are used. Default is
  `NULL`.

- scale_method:

  character. Method used to scale variable values for colouring cells.
  One of `"ecdf"` (default), `"zscore"`, `"raw"`, `"minmax"`, or
  `"minmax_var"`.

  - `"ecdf"`: Each cell shows the percentile of the cluster's median
    value compared to all observations outside the cluster (empirical
    CDF). Fill values are in \[0, 1\] and the legend uses percent
    labels.

  - `"zscore"`: Each cell shows the z-score of the cluster's median
    relative to all observations of that variable
    (`(median - mean) / sd`). Fill values are unbounded.

  - `"raw"`: Each cell shows the raw median value. Fill values are
    unbounded.

  - `"minmax"`: Each cell shows the cluster median scaled to \[0, 1\]
    using the global minimum and maximum across all observations of all
    variables. Fill values are in \[0, 1\] and the legend uses percent
    labels.

  - `"minmax_var"`: Each cell shows the cluster median scaled to \[0,
    1\] using the minimum and maximum of all observations within each
    variable separately. Fill values are in \[0, 1\] and the legend uses
    percent labels.

- palette:

  character or `NULL`. Named colour palette for the continuous fill
  scale. When not `NULL`, overrides `col` and `col_positions`. Available
  palettes: `"bipolar"` (default, blue-white-red), `"alarm"`
  (green-white-red, good-to-bad), `"accessible"` (blue-white-orange,
  colour-blind-safe diverging), `"heat"` (light-yellow to dark-red,
  sequential), `"sky"` (white to navy, sequential). Set to `NULL` to use
  `col` and `col_positions` directly.

- col:

  character vector. Colours used to fill tiles, ordered from low to high
  values. Default is `c("#2166AC", "#F7F7F7", "#B2182B")` (blue, white,
  red). Any number of colours (\>= 2) is accepted. Ignored when
  `palette` is not `NULL`.

- col_positions:

  numeric vector or `"auto"`. Positions (in \[0, 1\]) at which each
  colour in `col` is placed on the fill scale. Must be the same length
  as `col`, sorted in ascending order, with the first value `0` and the
  last value `1`. When `"auto"` (default) and `col` has exactly three
  colours and `scale_method = "ecdf"`, the middle colour is stretched
  over `white_range` (the current default behaviour). In all other
  `"auto"` cases the colours are evenly spaced from 0 to 1. Ignored when
  `palette` is not `NULL`.

- white_range:

  numeric vector of length 2. The range of positions (on a 0-1 scale)
  over which the middle colour is stretched. Only used when `col` has
  exactly three colours, `scale_method = "ecdf"`, and
  `col_positions = "auto"`. Also applied to diverging `palette` presets
  (those with `col_positions = "auto"`). Default is `c(0.4, 0.6)`.

- na_rm:

  logical. Whether to remove `NA` values before computing per-cluster
  statistics. When `TRUE` (default), `NA` values are removed and a
  message is issued showing how many were removed per variable. When
  `FALSE`, `NA` values are passed through: tile fill values will be `NA`
  (rendered as grey by default) where a variable has no non-missing
  observations in a cluster.

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

- show_values:

  logical. Whether to overlay the median value for each cluster-variable
  combination as a text label on each tile. Default is `FALSE`.

- values_format:

  function or `NULL`. A function that takes a numeric vector and returns
  a character vector of formatted labels. Applied to the per-cluster
  median values when `show_values = TRUE`. When `NULL`, values are
  formatted to three significant figures using
  `formatC(x, digits = 3, format = "g")`. Default is `NULL`.

- values_col:

  character. Colour for the overlaid text labels. Default is `"black"`.

- values_size:

  numeric. Font size (in `ggplot2` units) for the overlaid text labels.
  Default is `3`.

- cluster:

  character. Name of the column in `.data` that identifies group
  membership. Alias for the `group` parameter.

- ...:

  Additional arguments passed to `plot_group_heatmap()`.

## Value

A ggplot object.

## Examples

``` r
set.seed(1)
.data <- data.frame(
  group = rep(paste0("C", 1:3), each = 20),
  var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2)),
  var2 = c(rnorm(20, -1), rnorm(20, 1), rnorm(20, 0))
)
plot_group_heatmap(.data, group = "group")

plot_group_heatmap(.data, group = "group", show_values = TRUE)

plot_group_heatmap(.data, group = "group", palette = "alarm")
```
