# Changelog

## UtilsGGSV (development version)

- Add `node_fill_by` parameter to
  [`plot_group_mst()`](https://satvilab.github.io/UtilsGGSV/reference/plot_group_mst.md)
  /
  [`plot_cluster_mst()`](https://satvilab.github.io/UtilsGGSV/reference/plot_group_mst.md),
  allowing nodes to be filled by cluster identity (discrete palette)
  instead of per-variable ECDF percentile (continuous gradient).
- Add tiered automatic colour palette for all `plot_cluster_*` and
  `plot_group_*` functions: Okabe-Ito for up to 8 groups
  (colorblind-safe), ColorBrewer Paired for up to 12, Kelly’s palette
  (from the optional `Polychrome` package) for up to 21, Glasbey’s
  palette for up to 31, and `hue_pal()` fallback beyond that. The
  `Polychrome` package is optional; a warning is issued and `hue_pal()`
  is used if it is not installed.
- Fix `devtools::check()` WARNING: documented `...` argument in
  `plot_cluster_*` alias functions.
- Fix `devtools::check()` NOTE: add `^issues$` to `.Rbuildignore`.
- Fix `devtools::check()` NOTE: import
  [`utils::modifyList`](https://rdrr.io/r/utils/modifyList.html) in
  [`plot_group_scatter()`](https://satvilab.github.io/UtilsGGSV/reference/plot_group_scatter.md).
- Add `Depends: R (>= 3.5.0)` to suppress serialised-object build
  warning.

## UtilsGGSV 0.9.0

- Add `plot_group_*` family as the primary API
  ([`plot_group_density()`](https://satvilab.github.io/UtilsGGSV/reference/plot_group_density.md),
  [`plot_group_heatmap()`](https://satvilab.github.io/UtilsGGSV/reference/plot_group_heatmap.md),
  [`plot_group_mst()`](https://satvilab.github.io/UtilsGGSV/reference/plot_group_mst.md),
  [`plot_group_scatter()`](https://satvilab.github.io/UtilsGGSV/reference/plot_group_scatter.md)),
  with `plot_cluster_*` functions kept as backwards-compatible aliases.
- Rename the core `.data` parameter uniformly across all `plot_group_*`
  and `plot_cluster_*` functions (was `data` in some functions).
- Update `@param vars` documentation to refer to groups rather than
  clusters throughout.

## UtilsGGSV 0.8.0

- Add `palette` parameter to
  [`plot_cluster_heatmap()`](https://satvilab.github.io/UtilsGGSV/reference/plot_group_heatmap.md),
  [`plot_cluster_density()`](https://satvilab.github.io/UtilsGGSV/reference/plot_group_density.md),
  [`plot_cluster_mst()`](https://satvilab.github.io/UtilsGGSV/reference/plot_group_mst.md),
  and
  [`plot_cluster_scatter()`](https://satvilab.github.io/UtilsGGSV/reference/plot_group_scatter.md)
  with five built-in colour-blind-friendly presets: `"bipolar"`
  (default), `"alarm"`, `"accessible"`, `"heat"`, and `"sky"`.
- Add `na_rm` parameter to all `plot_cluster_*` functions; `NA` values
  are now removed by default with an informative message.
- Rename the `data` parameter to `.data` in all `plot_cluster_*`
  functions to avoid masking the
  [`rlang::.data`](https://rlang.r-lib.org/reference/dot-data.html)
  pronoun.
- Add multiple scale methods to
  [`plot_cluster_heatmap()`](https://satvilab.github.io/UtilsGGSV/reference/plot_group_heatmap.md)
  via the new `scale_method` parameter: `"ecdf"` (default), `"zscore"`,
  `"raw"`, `"minmax"`, and `"minmax_var"`.
- Add `show_values`, `values_format`, `values_col`, and `values_size`
  parameters to
  [`plot_cluster_heatmap()`](https://satvilab.github.io/UtilsGGSV/reference/plot_group_heatmap.md)
  for overlaying median tile labels.

## UtilsGGSV 0.7.6

- Patch release: internal consistency improvements and minor
  documentation fixes following the 0.7.5 input-validation work.

## UtilsGGSV 0.7.5

- Add input validation to all `plot_cluster_*` functions; informative
  error messages are now raised for invalid `.data`, `cluster`, and
  `vars` arguments.
- Make parameter names consistent across
  [`plot_cluster_density()`](https://satvilab.github.io/UtilsGGSV/reference/plot_group_density.md),
  [`plot_cluster_heatmap()`](https://satvilab.github.io/UtilsGGSV/reference/plot_group_heatmap.md),
  [`plot_cluster_mst()`](https://satvilab.github.io/UtilsGGSV/reference/plot_group_mst.md),
  and
  [`plot_cluster_scatter()`](https://satvilab.github.io/UtilsGGSV/reference/plot_group_scatter.md).

## UtilsGGSV 0.7.4

- Add `bandwidth` parameter to
  [`plot_cluster_density()`](https://satvilab.github.io/UtilsGGSV/reference/plot_group_density.md)
  with `"hpi_1"` as the new default (plug-in bandwidth via
  [`ks::hpi()`](https://rdrr.io/pkg/ks/man/Hpi.html)).
- Change the default value of the `density` parameter in
  [`plot_cluster_density()`](https://satvilab.github.io/UtilsGGSV/reference/plot_group_density.md)
  from `"overall"` to `"both"`.
- Restructure
  [`plot_cluster_density()`](https://satvilab.github.io/UtilsGGSV/reference/plot_group_density.md)
  documentation for clarity.

## UtilsGGSV 0.7.3

- Add rug layer to
  [`plot_cluster_density()`](https://satvilab.github.io/UtilsGGSV/reference/plot_group_density.md),
  controlled by the new `rug` parameter.
- Add automatic coercion of the cluster column to a factor in
  [`plot_cluster_density()`](https://satvilab.github.io/UtilsGGSV/reference/plot_group_density.md).
- Add even-weighting option for the overall density estimate in
  [`plot_cluster_density()`](https://satvilab.github.io/UtilsGGSV/reference/plot_group_density.md)
  via the `density_overall_weight` parameter.
