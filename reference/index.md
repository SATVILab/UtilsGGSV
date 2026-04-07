# Package index

## Correlation Plotting

Functions for plotting correlation coefficients

- [`ggcorr()`](https://satvilab.github.io/UtilsGGSV/reference/ggcorr.md)
  : Plot scatterplot with correlation coefficients and ab-line overlaid

## Axis Management

Functions for managing plot axes

- [`axis_limits()`](https://satvilab.github.io/UtilsGGSV/reference/axis_limits.md)
  : Manage axis limits

## Text Annotations

Functions for adding text to plots

- [`add_text_column()`](https://satvilab.github.io/UtilsGGSV/reference/add_text_column.md)
  : Add a column of text to a ggplot

## Transformations

Scale transformation utilities

- [`get_trans()`](https://satvilab.github.io/UtilsGGSV/reference/get_trans.md)
  : Get transformation object

## Plot Saving

Utilities for saving plots

- [`save_plot()`](https://satvilab.github.io/UtilsGGSV/reference/save_plot.md)
  : Save plot in multiple formats using cowplot::save_plot
- [`ggsave2()`](https://satvilab.github.io/UtilsGGSV/reference/ggsave2.md)
  : Save plot in multiple formats using cowplot::ggsave2

## Group Plots

Functions for visualising group characteristics

- [`plot_group_heatmap()`](https://satvilab.github.io/UtilsGGSV/reference/plot_group_heatmap.md)
  [`plot_cluster_heatmap()`](https://satvilab.github.io/UtilsGGSV/reference/plot_group_heatmap.md)
  : Plot heat map of scaled variable values per group
- [`plot_group_density()`](https://satvilab.github.io/UtilsGGSV/reference/plot_group_density.md)
  [`plot_cluster_density()`](https://satvilab.github.io/UtilsGGSV/reference/plot_group_density.md)
  : Plot density of variable values with per-group overlays
- [`plot_group_mst()`](https://satvilab.github.io/UtilsGGSV/reference/plot_group_mst.md)
  [`plot_cluster_mst()`](https://satvilab.github.io/UtilsGGSV/reference/plot_group_mst.md)
  : Plot minimum-spanning tree of groups with per-variable node
  colouring
- [`plot_group_scatter()`](https://satvilab.github.io/UtilsGGSV/reference/plot_group_scatter.md)
  [`plot_cluster_scatter()`](https://satvilab.github.io/UtilsGGSV/reference/plot_group_scatter.md)
  : Biaxial scatter plot with group medians overlaid

## Cluster Plots (Aliases)

Aliases of group plot functions for backward compatibility

- [`plot_group_heatmap()`](https://satvilab.github.io/UtilsGGSV/reference/plot_group_heatmap.md)
  [`plot_cluster_heatmap()`](https://satvilab.github.io/UtilsGGSV/reference/plot_group_heatmap.md)
  : Plot heat map of scaled variable values per group
- [`plot_group_density()`](https://satvilab.github.io/UtilsGGSV/reference/plot_group_density.md)
  [`plot_cluster_density()`](https://satvilab.github.io/UtilsGGSV/reference/plot_group_density.md)
  : Plot density of variable values with per-group overlays
- [`plot_group_mst()`](https://satvilab.github.io/UtilsGGSV/reference/plot_group_mst.md)
  [`plot_cluster_mst()`](https://satvilab.github.io/UtilsGGSV/reference/plot_group_mst.md)
  : Plot minimum-spanning tree of groups with per-variable node
  colouring
- [`plot_group_scatter()`](https://satvilab.github.io/UtilsGGSV/reference/plot_group_scatter.md)
  [`plot_cluster_scatter()`](https://satvilab.github.io/UtilsGGSV/reference/plot_group_scatter.md)
  : Biaxial scatter plot with group medians overlaid

## Cluster Analysis

Functions for analysing and merging clusters

- [`cluster_merge_bin()`](https://satvilab.github.io/UtilsGGSV/reference/cluster_merge_bin.md)
  : Merge clusters based on variable bin thresholds
- [`cluster_merge_unimodal()`](https://satvilab.github.io/UtilsGGSV/reference/cluster_merge_unimodal.md)
  : Merge clusters that remain unimodal when combined

## Cluster Simulation

Functions for simulating high-dimensional single-cell data

- [`cluster_sim()`](https://satvilab.github.io/UtilsGGSV/reference/cluster_sim.md)
  : Simulate High-Dimensional Single-Cell Clusters (FAUST Method)
