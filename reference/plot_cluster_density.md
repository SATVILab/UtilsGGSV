# Plot density of variable values with per-cluster median lines

For each variable, plots the density of values and optionally overlays
per-cluster density curves or cluster median lines. The `density`
argument controls what is shown:

- `"overall"` (default): the overall density of all observations with a
  vertical line per cluster at that cluster's median value.

- `"cluster"`: one density curve per cluster, coloured by cluster.

- `"both"`: the overall density curve and one density curve per cluster,
  with the cluster curves scaled according to the `scale` argument.

By default the function returns a **named list of ggplot2 objects**, one
per variable. If `n_col` or `n_row` is supplied the plots are instead
combined into a **single faceted ggplot2 object** via `facet_wrap`.

## Usage

``` r
plot_cluster_density(
  data,
  cluster,
  vars = NULL,
  col_clusters = NULL,
  n_col = NULL,
  n_row = NULL,
  density = "overall",
  scale = "max_overall",
  scales = "free_y",
  expand_coord = NULL,
  exclude_min = "no",
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

- col_clusters:

  named character vector or `NULL`. Per-cluster colours. Names should
  match cluster labels. When `NULL` (default), the default ggplot2
  colour scale is used.

- n_col:

  integer or `NULL`. Number of columns passed to
  [`ggplot2::facet_wrap`](https://ggplot2.tidyverse.org/reference/facet_wrap.html).
  If supplied (or if `n_row` is supplied) a single faceted plot is
  returned instead of a list. Default is `NULL`.

- n_row:

  integer or `NULL`. Number of rows passed to
  [`ggplot2::facet_wrap`](https://ggplot2.tidyverse.org/reference/facet_wrap.html).
  If supplied (or if `n_col` is supplied) a single faceted plot is
  returned instead of a list. Default is `NULL`.

- density:

  character. What density to display. One of `"overall"` (default:
  overall density curve plus cluster median lines), `"cluster"` (one
  density curve per cluster, coloured by cluster), or `"both"` (overall
  density curve plus per-cluster density curves). When `"cluster"` or
  `"both"`, the `scale` argument controls how cluster densities are
  scaled.

- scale:

  character. How to scale per-cluster density curves. Only relevant when
  `density` is `"cluster"` or `"both"`. One of `"max_overall"` (default:
  each cluster density is rescaled so that its maximum equals the
  maximum of the overall density, keeping y-axis values comparable to
  the overall density), `"max_cluster"` (no rescaling; y-axis is
  determined by the tallest curve), or `"free"` (no rescaling;
  equivalent to `"max_cluster"`).

- scales:

  character. The `scales` argument passed to
  [`ggplot2::facet_wrap`](https://ggplot2.tidyverse.org/reference/facet_wrap.html)
  when a faceted plot is requested. Default is `"free_y"` so that the
  x-axis is shared across panels.

- expand_coord:

  numeric vector or named list or `NULL`. Expands the x-axis limits to
  include the given values. A plain numeric vector is applied to every
  variable. A named list (names = variable names, values = numeric
  vectors) applies expansion per variable. When a faceted plot is
  requested via `n_col`/`n_row` and a named list is provided, a warning
  is issued and `expand_coord` is ignored (incompatible with faceting).
  Default is `NULL`.

- exclude_min:

  character. Whether to exclude observations whose value equals the
  minimum from the density and median calculations. Options are: `"no"`
  (default, no exclusion), `"overall"` (exclude observations whose value
  equals the global minimum across all variables), or `"variable"` (for
  each variable, exclude observations whose value equals that variable's
  minimum).

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

A named list of ggplot2 objects (one per variable) when neither `n_col`
nor `n_row` is specified. A single ggplot2 object with `facet_wrap`
panels when `n_col` or `n_row` is specified.

## Examples

``` r
set.seed(1)
data <- data.frame(
  cluster = rep(paste0("C", 1:3), each = 20),
  var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2)),
  var2 = c(rnorm(20, -1), rnorm(20, 1), rnorm(20, 0))
)
# Default: overall density with cluster median lines
plot_list <- plot_cluster_density(data, cluster = "cluster")

# Per-cluster density curves
plot_cluster_density(data, cluster = "cluster", density = "cluster")
#> $var1

#> 
#> $var2

#> 

# Both overall and per-cluster densities (default scaling: max_overall)
plot_cluster_density(data, cluster = "cluster", density = "both")
#> $var1

#> 
#> $var2

#> 

# Faceted plot with 2 columns
plot_cluster_density(data, cluster = "cluster", n_col = 2)
```
