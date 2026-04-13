# Biaxial scatter plot with group medians overlaid

Creates a biaxial scatter plot with observations colored by group
assignment. Median group centroids are overlaid as large points and
labeled with group names. The plot can use either raw variables
(specified by the user) or dimensionality reduction components as axes.

### Colour palette

When `point_col` is `NULL`, group colours are assigned automatically
based on `palette_group`. The `"auto"` strategy selects a palette by the
number of groups:

- **1–8 groups**: Okabe-Ito — colorblind-safe 8-colour palette.

- **9–12 groups**: ColorBrewer Paired — 12 colours pairing light and
  dark versions of 6 hues.

- **13–21 groups**: Kelly's palette (optional `Polychrome` package) — 21
  colours of maximum perceptual contrast (white excluded). Falls back to
  `hue_pal()` with a warning if `Polychrome` is not installed.

- **22–31 groups**: Glasbey's palette (optional `Polychrome` package) —
  31 algorithmically spaced colours (white excluded). Falls back to
  `hue_pal()` with a warning if `Polychrome` is not installed.

- **\> 31 groups**: `hue_pal()` — evenly spaced hues (a warning is
  issued).

Set `palette_group` explicitly to override the automatic selection
(provided the chosen palette supports at least as many colours as there
are groups).

## Usage

``` r
plot_group_scatter(
  .data,
  group,
  dim_red = NULL,
  vars = NULL,
  dim_red_args = list(),
  point_col_var = NULL,
  point_col = NULL,
  palette_group = "auto",
  palette = "bipolar",
  col = c("#2166AC", "#F7F7F7", "#B2182B"),
  col_positions = "auto",
  white_range = c(0.4, 0.6),
  na_rm = TRUE,
  point_size = 2,
  point_alpha = 0.65,
  centroid_size = 3,
  label_size = 4,
  label_offset = 0.3,
  ggrepel = TRUE,
  font_size = 14,
  x_lab = NULL,
  y_lab = NULL,
  show_legend = TRUE,
  thm = cowplot::theme_cowplot(font_size = font_size) + ggplot2::theme(plot.background =
    ggplot2::element_rect(fill = "white", colour = NA), panel.background =
    ggplot2::element_rect(fill = "white", colour = NA)),
  grid = cowplot::background_grid(major = "xy")
)

plot_cluster_scatter(
  .data,
  cluster,
  palette_cluster = "auto",
  palette = "bipolar",
  ...
)
```

## Arguments

- .data:

  data.frame. Rows are observations. Must contain a column identifying
  group membership and numeric variables.

- group:

  character. Name of the column in `.data` that identifies group
  membership.

- dim_red:

  character or `NULL`. Dimensionality reduction method: one of `"none"`,
  `"pca"`, `"tsne"`, `"umap"`. If `NULL`, auto-selects `"none"` when
  exactly 2 numeric vars are available, otherwise `"pca"`.

- vars:

  character vector or `NULL`. Names of numeric columns in `.data` to use
  for the plot or reduction. If `NULL`, uses all numeric columns except
  `group` and `point_col_var`.

- dim_red_args:

  named list. Additional arguments passed to the dimensionality
  reduction function, overriding any defaults set by
  `plot_cluster_scatter`. For `dim_red = "pca"` these are passed to
  [`stats::prcomp()`](https://rdrr.io/r/stats/prcomp.html) (default:
  `scale. = TRUE`); for `dim_red = "tsne"` to
  [`Rtsne::Rtsne()`](https://rdrr.io/pkg/Rtsne/man/Rtsne.html)
  (defaults: `dims = 2`, `perplexity` auto-computed,
  `check_duplicates = FALSE`, `pca = FALSE`); for `dim_red = "umap"` to
  [`umap::umap()`](https://rdrr.io/pkg/umap/man/umap.html). The data
  argument is always set internally and cannot be overridden. Ignored
  when `dim_red = "none"`. Default is
  [`list()`](https://rdrr.io/r/base/list.html).

- point_col_var:

  character or `NULL`. Column to use for point colour mapping. Default
  is same as `cluster`.

- point_col:

  named vector or `NULL`. Custom colours for discrete `point_col_var`
  (named by level). When `NULL` (default), colours are chosen
  automatically by number of groups: Okabe-Ito for up to 8, ColorBrewer
  Paired for up to 12, Kelly's palette (requires `Polychrome`) for up to
  21, Glasbey's palette (requires `Polychrome`) for up to 31, and
  `hue_pal()` for larger numbers. Ignored for continuous `point_col_var`
  (use `col` instead).

- palette_group:

  character. Palette used for automatic colour assignment when
  `point_col` is `NULL` for a discrete `point_col_var`. One of `"auto"`
  (default), `"okabe_ito"`, `"paired"`, `"kelly"`, `"glasbey"`, or
  `"hue_pal"`. See the **Colour palette** section of Details.

- palette:

  character or `NULL`. Named colour palette for the continuous point
  colour scale. Forwarded to `plot_group_scatter()`. Default is
  `"bipolar"`.

- col:

  character vector. Colours used for the continuous `point_col_var`
  colour scale, ordered from low to high values. Default is
  `c("#2166AC", "#F7F7F7", "#B2182B")` (blue, white, red). Any number of
  colours (\>= 2) is accepted. Ignored when `point_col_var` is discrete
  or when `palette` is not `NULL`.

- col_positions:

  numeric vector or `"auto"`. Positions (in \[0, 1\]) at which each
  colour in `col` is placed on the colour scale. Must be the same length
  as `col`, sorted ascending, with first value `0` and last value `1`.
  When `"auto"` (default) and `col` has exactly three colours, the
  middle colour is stretched over `white_range`. In all other `"auto"`
  cases the colours are evenly spaced from 0 to 1. Ignored when
  `point_col_var` is discrete or when `palette` is not `NULL`.

- white_range:

  numeric vector of length 2. The range of positions (on a 0-1 scale)
  over which the middle colour is stretched. Only used when `col` has
  exactly three colours and `col_positions = "auto"`. Also applied to
  diverging `palette` presets. Default is `c(0.4, 0.6)`. Ignored when
  `point_col_var` is discrete.

- na_rm:

  logical. Whether to remove observations with missing values in the
  variables used for the plot axes (and dimensionality reduction, when
  applicable). When `TRUE` (default), missing observations are removed
  and a message is issued showing how many. When `FALSE` and `dim_red`
  is not `"none"`, an error is raised if any missing values are found
  (dimensionality reduction algorithms cannot handle them). When `FALSE`
  and `dim_red = "none"`, observations with missing axis values are
  silently dropped by ggplot2.

- point_size:

  numeric. Size of observation points. Default is `2`.

- point_alpha:

  numeric. Alpha transparency for observation points and legend guide.
  Default is `0.65`.

- centroid_size:

  numeric. Size of centroid points. Default is `3`.

- label_size:

  numeric. Font size for centroid labels. Default is `4`.

- label_offset:

  numeric. Label repulsion padding for centroid labels in cm. Default is
  `0.3`.

- ggrepel:

  logical. Use
  [`ggrepel::geom_text_repel`](https://ggrepel.slowkow.com/reference/geom_text_repel.html)
  for centroid labels. Default is `TRUE`.

- font_size:

  numeric. Font size passed to
  [`cowplot::theme_cowplot`](https://wilkelab.org/cowplot/reference/theme_cowplot.html).
  Default is `14`.

- x_lab:

  character or `NULL`. Label for x axis; default uses reduction variable
  names.

- y_lab:

  character or `NULL`. Label for y axis; default uses reduction variable
  names.

- show_legend:

  logical. Whether to show the legend. Default is `TRUE`. Set to `FALSE`
  to hide the legend, as centroid labels may suffice.

- thm:

  ggplot2 theme object or `NULL`. Default is
  `cowplot::theme_cowplot(font_size = font_size)` with white background.

- grid:

  ggplot2 layer or `NULL`. Background grid added to the plot. Default is
  `cowplot::background_grid(major = "xy")`. Set to `NULL` to suppress
  the grid.

- cluster:

  character. Name of the column in `.data` that identifies group
  membership. Alias for the `group` parameter.

- palette_cluster:

  character. Alias for `palette_group` in `plot_group_scatter()`. See
  the **Colour palette** section of Details.

- ...:

  Additional arguments passed to `plot_group_scatter()`.

## Examples

``` r
set.seed(1)
.data <- data.frame(
  group = rep(paste0("C", 1:3), each = 20),
  var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2)),
  var2 = c(rnorm(20, -1), rnorm(20, 1), rnorm(20, 0)),
  var3 = c(rnorm(20, 1), rnorm(20, -1), rnorm(20, 0))
)
plot_group_scatter(.data, group = "group")
#> dim_red automatically set to 'pca' because more than two numeric variables are available.

plot_group_scatter(.data, group = "group", dim_red = "none", vars = c("var1", "var2"))

plot_group_scatter(.data, group = "group", show_legend = FALSE)
#> dim_red automatically set to 'pca' because more than two numeric variables are available.

# Pass extra arguments to the dim-red function, e.g. disable scaling in PCA:
plot_group_scatter(.data, group = "group", dim_red = "pca",
                     dim_red_args = list(scale. = FALSE))
```
