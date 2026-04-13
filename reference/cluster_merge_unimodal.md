# Merge clusters that remain unimodal when combined

Takes the output of
[`cluster_merge_bin()`](https://satvilab.github.io/UtilsGGSV/reference/cluster_merge_bin.md)
and iteratively merges similarly-labelled clusters as long as the
combined population remains unimodal along every variable where the two
clusters have different bin indices (assessed via Hartigan's Dip Test).

## Usage

``` r
cluster_merge_unimodal(
  data,
  merge_result,
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
  Must contain the same variable columns used in the preceding call to
  [`cluster_merge_bin()`](https://satvilab.github.io/UtilsGGSV/reference/cluster_merge_bin.md).

- merge_result:

  list. The object returned by
  [`cluster_merge_bin()`](https://satvilab.github.io/UtilsGGSV/reference/cluster_merge_bin.md).
  Must carry a `"thresholds"` attribute (added automatically by
  `cluster_merge_bin`).

- max_label_diff:

  integer(1). Maximum allowed difference between bin indices (Chebyshev
  distance) for two clusters to be considered for merging. Default `2L`.

- ignore_labels:

  character vector or `NULL`. Merged-cluster labels to exclude from
  merging consideration. Default `NULL` (no labels ignored).

- dip_threshold:

  numeric(1). Minimum dip-test p-value required for every mismatched
  variable (i.e. variables where the two candidate clusters fall in
  different bins) before a merge is accepted. A higher value is more
  conservative (demands stronger evidence of unimodality). Default
  `0.15`.

- min_mode_dist:

  numeric or named numeric vector or `NULL`. If supplied, a candidate
  merge is rejected when the absolute difference between the two
  clusters' per-variable modes is below this distance **for every
  variable**. A named vector specifies per-variable distances; a scalar
  applies to all variables. Default `NULL` (no mode-distance filter).

- max_iterations:

  integer(1). Safety cap on the number of merge-relabel-repeat cycles.
  Default `50L`.

## Value

A named list with two elements and a `"thresholds"` attribute (carried
over from `merge_result`):

- `assign`: a
  [`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
  with one row per observation and columns `orig` (original cluster
  label from the raw data) and `merged` (final merged label after
  unimodal merging).

- `label`: a
  [`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
  with one row per unique final merged cluster and columns `merged`
  (final merged label), `level` (bin-combination label) and
  `descriptive` (human-readable bin-combination description).

## Examples

``` r
set.seed(42)
n <- 40
mat <- matrix(
  c(rnorm(n, 0, 1), rnorm(n, 0.3, 1)),
  ncol = 2,
  dimnames = list(NULL, c("v1", "v2"))
)
cl <- rep(c("A", "B", "C", "D"), each = n / 4)
bin_res <- cluster_merge_bin(mat, cl, list(v1 = 0, v2 = 0))
cluster_merge_unimodal(mat, bin_res)
#> $assign
#> # A tibble: 40 × 2
#>    orig  merged
#>    <chr> <chr> 
#>  1 A     1_2   
#>  2 A     1_2   
#>  3 A     1_2   
#>  4 A     1_2   
#>  5 A     1_2   
#>  6 A     1_2   
#>  7 A     1_2   
#>  8 A     1_2   
#>  9 A     1_2   
#> 10 A     1_2   
#> # ℹ 30 more rows
#> 
#> $label
#> # A tibble: 1 × 3
#>   merged level descriptive    
#>   <chr>  <chr> <chr>          
#> 1 1_2    1_2   v1 <= 0; v2 > 0
#> 
#> attr(,"thresholds")
#> attr(,"thresholds")$v1
#> [1] 0
#> 
#> attr(,"thresholds")$v2
#> [1] 0
#> 
```
