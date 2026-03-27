# Validate inputs shared by plot_cluster functions

Internal helper that validates the `.data`, `cluster`, and `vars`
arguments shared by `plot_cluster_heatmap` and `plot_cluster_density`.
Stops with an informative error message if any check fails.

## Usage

``` r
.plot_cluster_validate(.data, cluster, vars)
```

## Arguments

- .data:

  data.frame. The input data.

- cluster:

  character(1). Name of the cluster column.

- vars:

  character vector or `NULL`. Variable column names.

## Value

Invisibly returns `NULL`. Called for its side-effect (errors).
