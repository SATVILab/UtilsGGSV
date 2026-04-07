# Simulate High-Dimensional Single-Cell Clusters (FAUST Method)

Generates synthetic high-dimensional single-cell data replicating the
simulation studies from the FAUST paper (Greene et al., 2021,
*Patterns*). Supports configurable numbers of dimensions, samples, and
clusters, along with differential abundance spike-ins for benchmarking.

## Usage

``` r
cluster_sim(
  n_samples = 20L,
  n_clusters = 125L,
  n_dims = 10L,
  n_cells_per_sample = 25000L,
  base_cluster_weights = NULL,
  expr_modes = c(0, 8),
  var_bounds = c(1, 2),
  mean_perturb_sd = 1/sqrt(2),
  transform = c("none", "faust_gamma"),
  spike_clusters = NULL,
  spike_samples = NULL,
  spike_fold_change = 2,
  noise_dims = NULL,
  distribution = c("normal", "t"),
  df = 3,
  batch_effect_shift = NULL,
  batch_samples = NULL,
  knockout_clusters = NULL,
  knockout_samples = NULL
)
```

## Arguments

- n_samples:

  Integer. Number of total samples to simulate.

- n_clusters:

  Integer. Number of mixture components.

- n_dims:

  Integer. Number of markers/dimensions.

- n_cells_per_sample:

  Integer. Total observations per sample.

- base_cluster_weights:

  Numeric vector. Custom base weights. If `NULL`, uses the FAUST
  reference vector with uniform residual distribution.

- expr_modes:

  Numeric vector. The values used to construct base mean vectors.

- var_bounds:

  Numeric vector of length 2. Min and max constraints for variance in
  covariance matrices.

- mean_perturb_sd:

  Numeric. Standard deviation for the Gaussian used to perturb means per
  sample.

- transform:

  Character. Options: `"none"` (MVN) or `"faust_gamma"` for
  `Gamma(1 + |x/4|)`.

- spike_clusters:

  Integer vector. Indices of the clusters to spike in abundance.

- spike_samples:

  Integer vector. Indices of the samples to apply the spike to (e.g.,
  "responders").

- spike_fold_change:

  Numeric. The multiplier for the spiked cluster weights.

- noise_dims:

  Integer. Number of nuisance noise columns to append, sampled from a
  standard normal distribution. If `NULL` (default), no noise columns
  are added.

- distribution:

  Character. Distribution used to generate cell data. `"normal"`
  (default) uses
  [`MASS::mvrnorm()`](https://rdrr.io/pkg/MASS/man/mvrnorm.html); `"t"`
  uses [`mvtnorm::rmvt()`](https://rdrr.io/pkg/mvtnorm/man/Mvt.html) for
  heavier tails. Requires the mvtnorm package when set to `"t"`.

- df:

  Numeric. Degrees of freedom for the multivariate t-distribution. Only
  used when `distribution = "t"`. Default is `3`.

- batch_effect_shift:

  Numeric scalar or vector of length `n_dims`. Mean shift applied to
  samples indicated by `batch_samples` to simulate a batch effect. If
  `NULL` (default), no batch effect is applied.

- batch_samples:

  Integer vector. Indices of samples that receive the batch effect
  shift. Must be provided together with `batch_effect_shift`.

- knockout_clusters:

  Integer vector. Indices of clusters whose probability is set to zero
  in the samples given by `knockout_samples`. Must be provided together
  with `knockout_samples`.

- knockout_samples:

  Integer vector. Indices of samples where `knockout_clusters` are
  removed. Must be provided together with `knockout_clusters`.

## Value

A list containing:

- data:

  A tibble of the simulated data with `sample_id`, `cell_id`,
  `cluster_id`, and one column per dimension (`dim_1`, `dim_2`, ...).
  When `noise_dims` is set, additional columns `noise_1`, `noise_2`, ...
  are appended.

- metadata:

  A list containing true parameters: base means, perturbed means per
  sample, covariance matrices, and exact weight vectors.

## Examples

``` r
set.seed(42)
sim <- cluster_sim(
  n_samples = 2, n_clusters = 3, n_dims = 4,
  n_cells_per_sample = 100
)
head(sim$data)
#> # A tibble: 6 × 7
#>   sample_id cell_id cluster_id   dim_1 dim_2 dim_3 dim_4
#>       <int>   <int>      <int>   <dbl> <dbl> <dbl> <dbl>
#> 1         1       1          1 -0.247  -1.67  7.66  5.72
#> 2         1       2          1  0.683  -1.38  7.36  6.79
#> 3         1       3          1  2.71   -1.24  7.64  7.04
#> 4         1       4          1 -0.0160 -1.27  7.63  8.44
#> 5         1       5          1  0.693  -2.43  5.96  5.79
#> 6         1       6          1  2.50   -1.65 10.3   6.78
```
