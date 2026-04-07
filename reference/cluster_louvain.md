# Louvain community-detection clustering

Builds a shared nearest-neighbour graph from the input data and applies
the Louvain algorithm
([`igraph::cluster_louvain()`](https://r.igraph.org/reference/cluster_louvain.html))
to detect communities.

## Usage

``` r
cluster_louvain(data, vars = NULL, k = 15L, resolution = 1)
```

## Arguments

- data:

  matrix or data.frame. Rows are observations, columns are variables.

- vars:

  character vector or `NULL`. Column names to use for clustering. If
  `NULL` (default), all columns are used.

- k:

  integer(1). Number of nearest neighbours used to build the kNN graph.
  Default `15L`.

- resolution:

  numeric(1). Resolution parameter passed to
  [`igraph::cluster_louvain()`](https://r.igraph.org/reference/cluster_louvain.html).
  Higher values yield more (smaller) clusters. Default `1`.

## Value

A character vector of cluster labels, one per row of `data`. Labels are
of the form `"1"`, `"2"`, etc.

## Details

The function:

1.  Scales the selected columns to zero mean and unit variance.

2.  Finds the `k` nearest neighbours for each observation using
    Euclidean distance (via
    [`FNN::get.knn()`](https://rdrr.io/pkg/FNN/man/get.knn.html)).

3.  Constructs a shared-nearest-neighbour (SNN) graph where the edge
    weight between two observations equals the number of shared
    neighbours.

4.  Applies Louvain community detection on the SNN graph.

## Examples

``` r
set.seed(1)
mat <- matrix(
  c(rnorm(50, -3), rnorm(50, 3), rnorm(100, 0)),
  ncol = 2,
  dimnames = list(NULL, c("v1", "v2"))
)
cl <- cluster_louvain(mat, k = 10)
table(cl)
#> cl
#>  1  2  3  4  5  6  7 
#> 18 12 13  7 22 15 13 
```
