# SOM clustering with bin and unimodal merging

End-to-end wrapper that applies SOM clustering
([`cluster_som()`](https://satvilab.github.io/UtilsGGSV/reference/cluster_som.md)),
then merges clusters by variable bin thresholds
([`cluster_merge_bin()`](https://satvilab.github.io/UtilsGGSV/reference/cluster_merge_bin.md)),
and finally merges unimodal neighbours
([`cluster_merge_unimodal()`](https://satvilab.github.io/UtilsGGSV/reference/cluster_merge_unimodal.md)).

## Usage

``` r
cluster_som_merge(
  data,
  vars = NULL,
  thresholds,
  x_dim = 10L,
  y_dim = 10L,
  rlen = 10L,
  topo = "rectangular",
  max_label_diff = 2L,
  ignore_labels = NULL,
  dip_threshold = 0.15,
  min_mode_dist = NULL,
  max_iterations = 50L
)
```

## Arguments

- data:

  matrix or data.frame. Rows are observations, columns are variables.

- vars:

  character vector or `NULL`. Column names to use for clustering. If
  `NULL` (default), all columns are used.

- thresholds:

  named list. Passed to
  [`cluster_merge_bin()`](https://satvilab.github.io/UtilsGGSV/reference/cluster_merge_bin.md).
  Each element is a numeric vector of threshold values for the variable
  given by the element name.

- x_dim:

  integer(1). Number of columns in the SOM grid. Default `10L` (matching
  FlowSOM).

- y_dim:

  integer(1). Number of rows in the SOM grid. Default `10L` (matching
  FlowSOM).

- rlen:

  integer(1). Number of training iterations. Default `10L` (matching
  FlowSOM).

- topo:

  character(1). Grid topology passed to
  [`kohonen::somgrid()`](https://rdrr.io/pkg/kohonen/man/unit.distances.html).
  One of `"rectangular"` (default) or `"hexagonal"`.

- max_label_diff:

  integer(1). Passed to
  [`cluster_merge_unimodal()`](https://satvilab.github.io/UtilsGGSV/reference/cluster_merge_unimodal.md).
  Default `2L`.

- ignore_labels:

  character vector or `NULL`. Passed to
  [`cluster_merge_unimodal()`](https://satvilab.github.io/UtilsGGSV/reference/cluster_merge_unimodal.md).
  Default `NULL`.

- dip_threshold:

  numeric(1). Passed to
  [`cluster_merge_unimodal()`](https://satvilab.github.io/UtilsGGSV/reference/cluster_merge_unimodal.md).
  Default `0.15`.

- min_mode_dist:

  numeric or named numeric vector or `NULL`. Passed to
  [`cluster_merge_unimodal()`](https://satvilab.github.io/UtilsGGSV/reference/cluster_merge_unimodal.md).
  Default `NULL`.

- max_iterations:

  integer(1). Passed to
  [`cluster_merge_unimodal()`](https://satvilab.github.io/UtilsGGSV/reference/cluster_merge_unimodal.md).
  Default `50L`.

## Value

The list returned by
[`cluster_merge_unimodal()`](https://satvilab.github.io/UtilsGGSV/reference/cluster_merge_unimodal.md),
with elements `assign` and `label` and a `"thresholds"` attribute.

## Examples

``` r
set.seed(1)
mat <- matrix(
  c(rnorm(50, -3), rnorm(50, 3), rnorm(100, 0)),
  ncol = 2,
  dimnames = list(NULL, c("v1", "v2"))
)
result <- cluster_som_merge(
  mat,
  vars = c("v1", "v2"),
  thresholds = list(v1 = 0, v2 = 0),
  x_dim = 3, y_dim = 3
)
table(result$assign$merged)
#> 
#> 1_1 2_1 
#>  50  50 
```
