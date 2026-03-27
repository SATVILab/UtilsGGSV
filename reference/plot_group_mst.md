# Plot minimum-spanning tree of groups with per-variable node colouring

Computes the minimum-spanning tree (MST) over groups, where the distance
between two groups is the Euclidean distance between their median
variable profiles. The MST is built from the full pairwise Euclidean
distance matrix (i.e. a fully connected undirected weighted graph),
matching the approach used by FlowSOM (`BuildMST`). The node layout is
determined once and shared across all per-variable plots.

Two layout algorithms are supported via `layout_algorithm`:

- `"kamada-kawai"` (default): uses the Kamada-Kawai force-directed
  algorithm
  ([`igraph::layout_with_kk`](https://r.igraph.org/reference/layout_with_kk.html))
  on the MST graph, matching the FlowSOM visualisation style.

- `"mds"`: uses classical multidimensional scaling
  ([`stats::cmdscale`](https://rdrr.io/r/stats/cmdscale.html)) of the
  full Euclidean distance matrix.

For each variable, a separate plot is produced in which each group node
is **filled** according to the ECDF-standardised percentile of that
group's median value for the variable — the same scaling used by
[`plot_group_heatmap()`](https://satvilab.github.io/UtilsGGSV/reference/plot_group_heatmap.md).
The node border and label colour encode group identity and can be
overridden via `col_clusters`.

By default the function returns a **named list of ggplot2 objects**, one
per variable. If `n_col` or `n_row` is supplied the plots are combined
into a single figure using
[`cowplot::plot_grid`](https://wilkelab.org/cowplot/reference/plot_grid.html),
with variable names as labels.

## Usage

``` r
plot_group_mst(
  .data,
  group,
  vars = NULL,
  layout_algorithm = c("kamada-kawai", "mds"),
  coord_equal = TRUE,
  suppress_axes = NULL,
  col_clusters = NULL,
  palette = "bipolar",
  col = c("#2166AC", "#F7F7F7", "#B2182B"),
  col_positions = "auto",
  white_range = c(0.4, 0.6),
  na_rm = TRUE,
  n_col = NULL,
  n_row = NULL,
  label_x = 0,
  label_y = 1,
  hjust = -0.5,
  vjust = 1.5,
  font_size = 14,
  thm = cowplot::theme_cowplot(font_size = font_size) + ggplot2::theme(plot.background =
    ggplot2::element_rect(fill = "white", colour = NA), panel.background =
    ggplot2::element_rect(fill = "white", colour = NA)),
  grid = cowplot::background_grid(major = "xy")
)

plot_cluster_mst(.data, cluster, ...)
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

- layout_algorithm:

  character. Layout algorithm for positioning nodes. One of
  `"kamada-kawai"` (default) or `"mds"`. `"kamada-kawai"` uses the
  Kamada-Kawai force-directed algorithm on the MST graph via
  [`igraph::layout_with_kk`](https://r.igraph.org/reference/layout_with_kk.html),
  matching the FlowSOM visualisation style. `"mds"` uses classical
  multidimensional scaling of the full distance matrix via
  [`stats::cmdscale`](https://rdrr.io/r/stats/cmdscale.html).

- coord_equal:

  logical. Whether to enforce equal visual scaling on both axes (one
  unit on the x-axis equals one unit on the y-axis) via
  [`ggplot2::coord_equal()`](https://ggplot2.tidyverse.org/reference/coord_fixed.html).
  Default is `TRUE`.

- suppress_axes:

  logical or `NULL`. Whether to suppress axis text, ticks, lines, and
  titles. When `NULL` (default), the value is inherited from
  `coord_equal` — axes are suppressed when equal scaling is active.

- col_clusters:

  named character vector or `NULL`. Per-cluster colours applied to node
  borders and text labels. Names should match cluster labels. When
  `NULL` (default), a colour-blind-friendly palette (`"Paired"`) is
  used.

- palette:

  character or `NULL`. Named colour palette for the continuous node fill
  scale. When not `NULL`, overrides `col` and `col_positions`. Available
  palettes: `"bipolar"` (default, blue-white-red), `"alarm"`
  (green-white-red, good-to-bad), `"accessible"` (blue-white-orange,
  colour-blind-safe diverging), `"heat"` (light-yellow to dark-red,
  sequential), `"sky"` (white to navy, sequential). Set to `NULL` to use
  `col` and `col_positions` directly.

- col:

  character vector. Colours used to fill nodes, ordered from low to high
  values. Default is `c("#2166AC", "#F7F7F7", "#B2182B")` (blue, white,
  red). Any number of colours (\>= 2) is accepted. Ignored when
  `palette` is not `NULL`.

- col_positions:

  numeric vector or `"auto"`. Positions (in \[0, 1\]) at which each
  colour in `col` is placed on the fill scale. Must be the same length
  as `col`, sorted in ascending order, with the first value `0` and the
  last value `1`. When `"auto"` (default) and `col` has exactly three
  colours, the middle colour is stretched over `white_range`. In all
  other `"auto"` cases the colours are evenly spaced from 0 to 1.
  Ignored when `palette` is not `NULL`.

- white_range:

  numeric vector of length 2. The range of positions (on a 0-1 scale)
  over which the middle colour is stretched. Only used when `col` has
  exactly three colours and `col_positions = "auto"`. Also applied to
  diverging `palette` presets. Default is `c(0.4, 0.6)`.

- na_rm:

  logical. Whether to remove `NA` values before computing per-cluster
  medians and ECDF percentiles. When `TRUE` (default), `NA` values are
  removed and a message is issued showing how many were removed per
  variable. When `FALSE`, `NA` values are passed through: node fill
  values will be `NA` (rendered as grey by default) where a variable has
  no non-missing observations in a cluster.

- n_col:

  integer or `NULL`. Number of columns passed to
  [`cowplot::plot_grid`](https://wilkelab.org/cowplot/reference/plot_grid.html).
  If supplied (or if `n_row` is supplied) a single combined figure is
  returned instead of a list. Default is `NULL`.

- n_row:

  integer or `NULL`. Number of rows passed to
  [`cowplot::plot_grid`](https://wilkelab.org/cowplot/reference/plot_grid.html).
  If supplied (or if `n_col` is supplied) a single combined figure is
  returned instead of a list. Default is `NULL`.

- label_x:

  numeric. x position of the plot labels within each panel in grid mode.
  Passed to
  [`cowplot::plot_grid`](https://wilkelab.org/cowplot/reference/plot_grid.html).
  Default is `0`.

- label_y:

  numeric. y position of the plot labels within each panel in grid mode.
  Passed to
  [`cowplot::plot_grid`](https://wilkelab.org/cowplot/reference/plot_grid.html).
  Default is `1`.

- hjust:

  numeric. Horizontal justification of the plot labels in grid mode.
  Passed to
  [`cowplot::plot_grid`](https://wilkelab.org/cowplot/reference/plot_grid.html).
  Default is `-0.5`.

- vjust:

  numeric. Vertical justification of the plot labels in grid mode.
  Passed to
  [`cowplot::plot_grid`](https://wilkelab.org/cowplot/reference/plot_grid.html).
  Default is `1.5`.

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

- cluster:

  character. Name of the column in `.data` that identifies group
  membership. Alias for the `group` parameter.

## Value

A named list of ggplot2 objects (one per variable) when neither `n_col`
nor `n_row` is specified. A
[`cowplot::plot_grid`](https://wilkelab.org/cowplot/reference/plot_grid.html)
figure when `n_col` or `n_row` is specified.

## Examples

``` r
set.seed(1)
.data <- data.frame(
  group = rep(paste0("C", 1:3), each = 20),
  var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2)),
  var2 = c(rnorm(20, -1), rnorm(20, 1), rnorm(20, 0))
)
# Default: Kamada-Kawai layout, returns a named list of plots
plot_list <- plot_group_mst(.data, group = "group")

# MDS layout
plot_group_mst(.data, group = "group", layout_algorithm = "mds")
#> $var1

#> 
#> $var2

#> 

# Combined grid with 2 columns
plot_group_mst(.data, group = "group", n_col = 2)
```
