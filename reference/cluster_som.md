# Self-Organizing Map (SOM) clustering

Trains a Self-Organizing Map on the input data using
[`kohonen::som()`](https://rdrr.io/pkg/kohonen/man/supersom.html) and
returns the mapping-unit (node) assignment for every observation as a
cluster label.

## Usage

``` r
cluster_som(
  data,
  vars = NULL,
  x_dim = 10L,
  y_dim = 10L,
  rlen = 10L,
  topo = "rectangular"
)
```

## Arguments

- data:

  matrix or data.frame. Rows are observations, columns are variables.

- vars:

  character vector or `NULL`. Column names to use for clustering. If
  `NULL` (default), all columns are used.

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

## Value

A character vector of cluster labels, one per row of `data`. Labels are
of the form `"1"`, `"2"`, etc., corresponding to SOM grid nodes.

## Details

The function:

1.  Scales the selected columns to zero mean and unit variance.

2.  Creates a SOM grid of size `x_dim` × `y_dim`.

3.  Trains a SOM on the scaled data.

4.  Returns the unit assignment for each observation.

The default grid size (10 × 10) and training length (`rlen = 10`) match
the defaults used by FlowSOM's `BuildSOM`.

The `kohonen` package must be installed.

## Examples

``` r
set.seed(1)
mat <- matrix(
  c(rnorm(50, -3), rnorm(50, 3), rnorm(100, 0)),
  ncol = 2,
  dimnames = list(NULL, c("v1", "v2"))
)
cl <- cluster_som(mat, x_dim = 3, y_dim = 3)
table(cl)
#> cl
#>  1  2  3  4  5  6  7  8  9 
#> 14  3 15 18  3 15 12  1 19 
```
