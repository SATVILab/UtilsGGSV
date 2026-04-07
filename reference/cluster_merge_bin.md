# Merge clusters based on variable bin thresholds

Assigns each cluster to a bin for each variable based on where the
cluster's median falls relative to the provided thresholds. Clusters
that fall in the same combination of bins across all variables are
merged.

## Usage

``` r
cluster_merge_bin(data, cluster, thresholds)
```

## Arguments

- data:

  matrix or data.frame. Rows are observations, columns are variables.
  Must contain columns whose names match those in `thresholds`.

- cluster:

  character vector. Cluster assignment for each observation. Length must
  equal `nrow(data)`.

- thresholds:

  named list. Each element is a numeric vector of threshold values for
  the variable given by the element name. The thresholds define bin
  boundaries: a single threshold `t` creates two bins (`<= t` and
  `> t`), two thresholds create three bins, and so on.

## Value

A named list with two elements and a `"thresholds"` attribute:

- `assign`: a
  [`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
  with one row per observation and columns `orig` (original cluster
  label) and `merged` (merged cluster label).

- `label`: a
  [`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
  with one row per unique original cluster and columns `orig` (original
  cluster label), `level` (concise bin-combination label formed by
  joining per-variable bin indices with `"_"`) and `descriptive`
  (human-readable bin-combination description formed by joining
  per-variable range descriptions with `"; "`).

The `"thresholds"` attribute stores the input `thresholds` list so that
downstream functions such as
[`cluster_merge_unimodal()`](https://satvilab.github.io/UtilsGGSV/reference/cluster_merge_unimodal.md)
can re-use the bin boundaries without requiring the caller to pass them
again.

## Examples

``` r
set.seed(1)
mat <- matrix(
  c(rnorm(20, 2), rnorm(20, -2)),
  ncol = 2,
  dimnames = list(NULL, c("var1", "var2"))
)
cl <- rep(c("A", "B", "C", "D"), each = 5)
thresholds <- list(var1 = 0, var2 = 0)
cluster_merge_bin(mat, cl, thresholds)
#> $assign
#> # A tibble: 20 × 2
#>    orig  merged
#>    <chr> <chr> 
#>  1 A     2_1   
#>  2 A     2_1   
#>  3 A     2_1   
#>  4 A     2_1   
#>  5 A     2_1   
#>  6 B     2_1   
#>  7 B     2_1   
#>  8 B     2_1   
#>  9 B     2_1   
#> 10 B     2_1   
#> 11 C     2_1   
#> 12 C     2_1   
#> 13 C     2_1   
#> 14 C     2_1   
#> 15 C     2_1   
#> 16 D     2_1   
#> 17 D     2_1   
#> 18 D     2_1   
#> 19 D     2_1   
#> 20 D     2_1   
#> 
#> $label
#> # A tibble: 4 × 3
#>   orig  level descriptive        
#>   <chr> <chr> <chr>              
#> 1 A     2_1   var1 > 0; var2 <= 0
#> 2 B     2_1   var1 > 0; var2 <= 0
#> 3 C     2_1   var1 > 0; var2 <= 0
#> 4 D     2_1   var1 > 0; var2 <= 0
#> 
#> attr(,"thresholds")
#> attr(,"thresholds")$var1
#> [1] 0
#> 
#> attr(,"thresholds")$var2
#> [1] 0
#> 
```
