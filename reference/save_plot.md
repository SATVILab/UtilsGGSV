# Save plot in multiple formats using cowplot::save_plot

Wrapper around
[`cowplot::save_plot`](https://wilkelab.org/cowplot/reference/save_plot.html)
to save plots in multiple formats.

## Usage

``` r
save_plot(filename, plot, device = c("png", "pdf"), ...)
```

## Arguments

- filename:

  character. Name of plot (including directory, if not to save in
  working directory).

- plot:

  gg object. Output from either ggplot2::ggplot or cowplot::plot_grid.

- device:

  character vector. Devices to save in (i.e. plot format). Default is
  `c("png", "pdf")`.

- ...:

  passed on to
  [`cowplot::save_plot`](https://wilkelab.org/cowplot/reference/save_plot.html).

## Value

`invisible(filename)`.
