# UtilsGGSV

The goal of UtilsGGSV is to provide utility functions for plotting in R
using `ggplot2`.

## Installation

You can install `UtilsGGSV` from [GitHub](https://github.com/) with:

``` r
if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
remotes::install_github("SATVILab/UtilsGGSV")
```

## Examples

``` r
library(UtilsGGSV)
library(ggplot2)
theme_set(cowplot::theme_cowplot())
```

### Scatter Plot With Clusters with `plot_cluster_scatter`

``` r
set.seed(123)
example_data <- data.frame(
  cluster = rep(c("A", "B", "C"), each = 20),
  var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2)),
  var2 = c(rnorm(20, -1), rnorm(20, 1), rnorm(20, 0)),
  var3 = c(rnorm(20, 1), rnorm(20, -1), rnorm(20, 0))
)

# Default: PCA projection (> 2 numeric variables)
plot_cluster_scatter(example_data, cluster = "cluster")
#> dim_red automatically set to 'pca' because more than two numeric variables are available.
```

![](reference/figures/README-unnamed-chunk-3-1.png)

Raw variables can also be used directly:

``` r
plot_cluster_scatter(
  example_data,
  cluster = "cluster",
  dim_red = "none",
  vars = c("var1", "var2")
)
```

![](reference/figures/README-unnamed-chunk-4-1.png)

### Correlation Plots with `ggcorr`

The function `ggcorr` plots correlation coefficients:

``` r
set.seed(3)
response_vec_a <- rnorm(5)
response_tbl <- data.frame(
  group = rep(letters[1:3], each = 5),
  response = c(
    response_vec_a,
    response_vec_a * 1.2 + rnorm(5, sd = 0.2),
    response_vec_a * 2 + rnorm(5, sd = 2)
  ),
  pid = rep(paste0("id_", 1:5), 3)
)

ggcorr(
  data = response_tbl %>% dplyr::filter(group %in% c("a", "b")),
  grp = "group",
  y = "response",
  id = "pid"
)
```

![](reference/figures/README-unnamed-chunk-5-1.png)

We can display multiple correlation coefficients:

``` r
ggcorr(
  data = response_tbl %>% dplyr::filter(group %in% c("a", "b")),
  grp = "group",
  y = "response",
  id = "pid",
  corr_method = c("spearman", "pearson")
)
```

![](reference/figures/README-unnamed-chunk-6-1.png)

We can compare more than two groups:

``` r
ggcorr(
  data = response_tbl,
  grp = "group",
  y = "response",
  id = "pid",
  corr_method = "kendall"
)
```

![](reference/figures/README-unnamed-chunk-7-1.png)

We can compare more than two groups and multiple correlation
coefficients:

``` r
ggcorr(
  data = response_tbl,
  grp = "group",
  y = "response",
  id = "pid",
  corr_method = c("spearman", "pearson")
)
```

![](reference/figures/README-unnamed-chunk-8-1.png)

Specific functionality to make appropriate plots for the concordance
correlation coefficient is available:

``` r
ggcorr(
  data = response_tbl %>% dplyr::filter(group %in% c("a", "b")),
  grp = "group",
  y = "response",
  id = "pid",
  corr_method = "concordance",
  abline = TRUE,
  limits_equal = TRUE
)
```

![](reference/figures/README-unnamed-chunk-9-1.png)

Text in table can be moved around and resized:

``` r
ggcorr(
  data = response_tbl %>% dplyr::filter(group %in% c("a", "b")),
  grp = "group",
  y = "response",
  id = "pid",
  corr_method = c("spearman", "pearson", "concordance"),
  abline = TRUE,
  limits_equal = TRUE,
  coord = c(0.4, 0.17),
  font_size = 3,
  skip = 0.04,
  pval_signif = 2,
  est_signif = 2,
  ci_signif = 2
)
```

![](reference/figures/README-unnamed-chunk-10-1.png)

Finally, the text placement is kept consistent when the axes are
visually transformed:

``` r
ggcorr(
  data = response_tbl %>% dplyr::mutate(response = abs(response + 1)^4),
  grp = "group",
  y = "response",
  id = "pid",
  corr_method = "spearman",
  abline = TRUE,
  limits_equal = TRUE,
  trans = "log10",
  skip = 0.06
)
```

![](reference/figures/README-unnamed-chunk-11-1.png)

### Axis Limits with `axis_limits`

Fix axis limits to be equal between x- and y-axes, and/or expand axis
coordinates. The primary use of `axis_limits` is forcing the x- and
y-axes to have the same limits “automatically” (i.e. by inspecting the
`ggplot` object, thus not requiring the user to manually calculate
limits to pass to
[`ggplot2::expand_limits`](https://ggplot2.tidyverse.org/reference/expand_limits.html)).

``` r
data("cars", package = "datasets")

p0 <- ggplot(cars, aes(speed, dist)) +
  cowplot::background_grid(major = "xy") +
  geom_point() +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "Axes unadjusted") +
  labs(x = "Speed", y = "Distance")
p1 <- axis_limits(
  p = p0,
  limits_equal = TRUE
) +
  labs(title = "Axes limits equal")
p2 <- axis_limits(
  p = p0,
  limits_expand = list(
    x = c(0, 50),
    y = c(-10, 200)
  )
) +
  labs(title = "Axes limits expanded")
cowplot::plot_grid(p0, p1, p2)
```

![](reference/figures/README-axis_limits-1.png)

### Text Annotations with `add_text_column`

Add a column of text easily to a plot, regardless of underlying
transformation, using `add_text_column`.

``` r
data_mod <- data.frame(x = rnorm(mean = 1, 10)^2)
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
  geom_point() +
  cowplot::background_grid(major = "xy") 
add_text_column(
  p = p,
  x = data_mod$x,
  y = data_mod$y,
  text = results_vec,
  coord = c(0.05, 0.95),
  skip = 0.07
)
```

![](reference/figures/README-add_text_column-1.png)

Note that `add_text_column` places text in the same position, regardless
of underlying transformation.

``` r
p <- p +
  scale_y_continuous(
    trans = UtilsGGSV::get_trans("asinh")
  )
add_text_column(
  p = p,
  x = data_mod$x,
  y = data_mod$y,
  text = results_vec,
  trans = UtilsGGSV::get_trans("asinh"),
  coord = c(0.05, 0.95),
  skip = 0.07
)
```

![](reference/figures/README-add_text_column_trans-1.png)

### Cluster-Specific Plots

#### Heat Maps with `plot_cluster_heatmap`

The function `plot_cluster_heatmap` creates a heat map where each tile
shows the percentile of the median value of a variable for a cluster.
This percentile is compared against the ECDF of that variable across all
observations not in the cluster. Clusters and variables are ordered by
hierarchical clustering.

``` r
set.seed(1)
cluster_data <- data.frame(
  cluster = rep(paste0("C", 1:3), each = 20),
  var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2)),
  var2 = c(rnorm(20, -1), rnorm(20, 1), rnorm(20, 0))
)
plot_cluster_heatmap(cluster_data, cluster = "cluster")
```

![](reference/figures/README-plot_cluster_heatmap-1.png)

#### Density Plots with `plot_cluster_density`

The function `plot_cluster_density` plots, for each variable, the
overall density of values across all observations and overlays a
vertical line for each cluster at that cluster’s median value. Each
cluster is given a distinct colour, making it easy to see how each
cluster relates to the overall distribution.

``` r
set.seed(1)
cluster_data <- data.frame(
  cluster = rep(paste0("C", 1:3), each = 20),
  var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2)),
  var2 = c(rnorm(20, -1), rnorm(20, 1), rnorm(20, 0))
)
plot_cluster_density(cluster_data, cluster = "cluster")
#> $var1
```

![](reference/figures/README-plot_cluster_density-1.png)

``` R
#> 
#> $var2
```

![](reference/figures/README-plot_cluster_density-2.png)

#### Minimum-Spanning Tree with `plot_cluster_mst`

The function `plot_cluster_mst` computes the minimum-spanning tree (MST)
over clusters, using Euclidean distance between cluster median profiles.
Clusters are laid out in two dimensions via classical multidimensional
scaling (MDS). For each variable, a separate plot is produced in which
each node is filled according to the ECDF-standardised percentile of
that cluster’s median — the same colour scale used by
`plot_cluster_heatmap`. By default a named list of plots is returned;
supplying `n_col` or `n_row` returns a combined
[`cowplot::plot_grid`](https://wilkelab.org/cowplot/reference/plot_grid.html)
figure.

``` r
set.seed(1)
cluster_data <- data.frame(
  cluster = rep(paste0("C", 1:3), each = 20),
  var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2)),
  var2 = c(rnorm(20, -1), rnorm(20, 1), rnorm(20, 0))
)
# Default: returns a named list of plots, one per variable
plot_list <- plot_cluster_mst(cluster_data, cluster = "cluster")
plot_list[["var1"]]
```

![](reference/figures/README-plot_cluster_mst-1.png)

Combine into a grid with variable-name labels:

``` r
plot_cluster_mst(cluster_data, cluster = "cluster", n_col = 2)
```

![](reference/figures/README-plot_cluster_mst_grid-1.png)

### Transformations with `get_trans`

The utility function `get_trans` returns `trans` objects (as implemented
by the `scales` package) when given characters. It also adds various
higher roots (such as cubic and quartic) and adds the `asinh`
transformation.

``` r
get_trans("log10")
#> Transformer: log-10 [1e-100, Inf]
```
