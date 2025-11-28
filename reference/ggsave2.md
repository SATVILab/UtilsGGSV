# Save plot in multiple formats using cowplot::ggsave2

Wrapper around
[`cowplot::ggsave2`](https://wilkelab.org/cowplot/reference/ggsave2.html)
to save plots in multiple formats.

## Usage

``` r
ggsave2(filename, plot, device = c("png", "pdf"), ...)
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
  [`cowplot::ggsave2`](https://wilkelab.org/cowplot/reference/ggsave2.html).

## Value

`invisible(filename)`.
