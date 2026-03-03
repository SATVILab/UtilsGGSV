#' @md
#' @title Plot heat map of scaled variable values per cluster
#'
#' @description
#' Creates a heat map where each tile shows a scaled summary of a variable for
#' a cluster. The scaling method is controlled by the `scale_method` parameter.
#' By default (`scale_method = "ecdf"`), each tile shows the percentile of the
#' cluster's median value compared against the empirical cumulative distribution
#' function (ECDF) of that variable across all observations not belonging to the
#' cluster. Clusters and variables are ordered along the axes via hierarchical
#' clustering.
#'
#' @param data data.frame. Rows are observations. Must contain a column
#'   identifying cluster membership and columns for variable values.
#' @param cluster character. Name of the column in `data` that identifies
#'   cluster membership.
#' @param vars character vector or `NULL`. Names of columns in `data` to
#'   use as variables. If `NULL`, all columns except `cluster` are used.
#'   Default is `NULL`.
#' @param scale_method character. Method used to scale variable values for
#'   colouring cells. One of `"ecdf"` (default), `"zscore"`, `"raw"`,
#'   `"minmax"`, or `"minmax_var"`.
#'
#'   - `"ecdf"`: Each cell shows the percentile of the cluster's median value
#'     compared to all observations outside the cluster (empirical CDF).
#'     Fill values are in \[0, 1\] and the legend uses percent labels.
#'   - `"zscore"`: Each cell shows the z-score of the cluster's median
#'     relative to all observations of that variable
#'     (`(median - mean) / sd`). Fill values are unbounded.
#'   - `"raw"`: Each cell shows the raw median value. Fill values are
#'     unbounded.
#'   - `"minmax"`: Each cell shows the cluster median scaled to \[0, 1\]
#'     using the global minimum and maximum across all observations of all
#'     variables. Fill values are in \[0, 1\] and the legend uses percent
#'     labels.
#'   - `"minmax_var"`: Each cell shows the cluster median scaled to \[0, 1\]
#'     using the minimum and maximum of all observations within each variable
#'     separately. Fill values are in \[0, 1\] and the legend uses percent
#'     labels.
#' @param col character vector. Colours used to fill tiles, ordered from low
#'   to high values. Default is `c("#2166AC", "#F7F7F7", "#B2182B")` (blue,
#'   white, red). Any number of colours (>= 2) is accepted.
#' @param col_positions numeric vector or `"auto"`. Positions (in \[0, 1\]) at
#'   which each colour in `col` is placed on the fill scale. Must be the same
#'   length as `col`, sorted in ascending order, with the first value `0` and
#'   the last value `1`. When `"auto"` (default) and `col` has exactly three
#'   colours and `scale_method = "ecdf"`, the middle colour is stretched over
#'   `white_range` (the current default behaviour). In all other `"auto"` cases
#'   the colours are evenly spaced from 0 to 1.
#' @param white_range numeric vector of length 2. The range of positions (on a
#'   0-1 scale) over which the middle colour is stretched. Only used when `col`
#'   has exactly three colours, `scale_method = "ecdf"`, and `col_positions =
#'   "auto"`. Default is `c(0.4, 0.6)`.
#' @param font_size numeric. Font size passed to `cowplot::theme_cowplot`.
#'   Default is `14`.
#' @param thm ggplot2 theme object or `NULL`. Default is
#'   `cowplot::theme_cowplot(font_size = font_size)` with a white plot
#'   background. Set to `NULL` to apply no theme adjustment.
#' @param grid ggplot2 panel grid or `NULL`. Default is
#'   `cowplot::background_grid(major = "xy")`. Set to `NULL` for no grid.
#' @param show_values logical. Whether to overlay the median value for each
#'   cluster-variable combination as a text label on each tile. Default is
#'   `FALSE`.
#' @param values_format function or `NULL`. A function that takes a numeric
#'   vector and returns a character vector of formatted labels. Applied to
#'   the per-cluster median values when `show_values = TRUE`. When `NULL`,
#'   values are formatted to three significant figures using
#'   `formatC(x, digits = 3, format = "g")`. Default is `NULL`.
#' @param values_col character. Colour for the overlaid text labels.
#'   Default is `"black"`.
#' @param values_size numeric. Font size (in `ggplot2` units) for the
#'   overlaid text labels. Default is `3`.
#'
#' @return A ggplot object.
#' @export
#'
#' @examples
#' set.seed(1)
#' data <- data.frame(
#'   cluster = rep(paste0("C", 1:3), each = 20),
#'   var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2)),
#'   var2 = c(rnorm(20, -1), rnorm(20, 1), rnorm(20, 0))
#' )
#' plot_cluster_heatmap(data, cluster = "cluster")
#' plot_cluster_heatmap(data, cluster = "cluster", show_values = TRUE)
plot_cluster_heatmap <- function(data,
                                 cluster,
                                 vars = NULL,
                                 scale_method = "ecdf",
                                 col = c("#2166AC", "#F7F7F7", "#B2182B"),
                                 col_positions = "auto",
                                 white_range = c(0.4, 0.6),
                                 font_size = 14,
                                 thm = cowplot::theme_cowplot(
                                   font_size = font_size
                                 ) +
                                   ggplot2::theme(
                                     plot.background = ggplot2::element_rect(
                                       fill = "white", colour = NA
                                     ),
                                     panel.background = ggplot2::element_rect(
                                      fill = "white", colour = NA
                                     )
                                   ),
                                 grid = cowplot::background_grid(
                                   major = "xy"
                                 ),
                                 show_values = FALSE,
                                 values_format = NULL,
                                 values_col = "black",
                                 values_size = 3) {
  if (is.null(vars)) {
    vars <- setdiff(colnames(data), cluster)
  }

  scale_method <- match.arg(
    scale_method, c("ecdf", "zscore", "raw", "minmax", "minmax_var")
  )

  cluster_vec <- unique(data[[cluster]])

  plot_tbl <- .plot_cluster_heatmap_calc(data, cluster, vars, cluster_vec, scale_method)
  order_list <- .plot_cluster_heatmap_order(plot_tbl)

  .plot_cluster_heatmap_plot(
    plot_tbl = plot_tbl,
    order_list = order_list,
    col = col,
    col_positions = col_positions,
    white_range = white_range,
    thm = thm,
    grid = grid,
    show_values = show_values,
    values_format = values_format,
    values_col = values_col,
    values_size = values_size,
    scale_method = scale_method
  )
}

.plot_cluster_heatmap_calc <- function(data, cluster, vars, cluster_vec, scale_method) {
  if (scale_method == "ecdf") {
    return(purrr::map_df(cluster_vec, function(clust) {
      obs_in <- data[[cluster]] == clust
      data_out <- data[!obs_in, ]
      purrr::map_df(vars, function(var) {
        med <- stats::median(data[[var]][obs_in])
        ecdf_fn <- stats::ecdf(data_out[[var]])
        tibble::tibble(
          cluster = clust,
          variable = var,
          perc = ecdf_fn(med),
          med = med
        )
      })
    }))
  }

  med_tbl <- purrr::map_df(cluster_vec, function(clust) {
    obs_in <- data[[cluster]] == clust
    purrr::map_df(vars, function(var) {
      tibble::tibble(
        cluster = clust,
        variable = var,
        med = stats::median(data[[var]][obs_in])
      )
    })
  })

  if (scale_method == "zscore") {
    perc_vec <- numeric(nrow(med_tbl))
    for (var in vars) {
      all_vals <- data[[var]]
      mu <- mean(all_vals, na.rm = TRUE)
      sigma <- stats::sd(all_vals, na.rm = TRUE)
      idx <- med_tbl$variable == var
      perc_vec[idx] <- if (is.na(sigma) || sigma == 0) 0 else
        (med_tbl$med[idx] - mu) / sigma
    }
    med_tbl$perc <- perc_vec
  } else if (scale_method == "raw") {
    med_tbl$perc <- med_tbl$med
  } else if (scale_method == "minmax") {
    all_vals <- unlist(lapply(vars, function(v) data[[v]]))
    global_min <- min(all_vals, na.rm = TRUE)
    global_max <- max(all_vals, na.rm = TRUE)
    med_tbl$perc <- if (global_max == global_min) 0.5 else
      (med_tbl$med - global_min) / (global_max - global_min)
  } else if (scale_method == "minmax_var") {
    perc_vec <- numeric(nrow(med_tbl))
    for (var in vars) {
      all_vals <- data[[var]]
      var_min <- min(all_vals, na.rm = TRUE)
      var_max <- max(all_vals, na.rm = TRUE)
      idx <- med_tbl$variable == var
      perc_vec[idx] <- if (var_max == var_min) 0.5 else
        (med_tbl$med[idx] - var_min) / (var_max - var_min)
    }
    med_tbl$perc <- perc_vec
  }

  med_tbl
}

.plot_cluster_heatmap_order <- function(plot_tbl) {
  cluster_vec <- unique(plot_tbl$cluster)
  var_vec <- unique(plot_tbl$variable)

  if (length(cluster_vec) <= 1 || length(var_vec) <= 1) {
    return(list(cluster = cluster_vec, variable = var_vec))
  }

  expr_mat <- .plot_cluster_heatmap_order_mat(plot_tbl, cluster_vec, var_vec)

  list(
    cluster = cluster_vec[stats::hclust(stats::dist(expr_mat))$order],
    variable = var_vec[stats::hclust(stats::dist(t(expr_mat)))$order]
  )
}

.plot_cluster_heatmap_order_mat <- function(plot_tbl, cluster_vec, var_vec) {
  expr_mat <- matrix(
    NA_real_,
    nrow = length(cluster_vec),
    ncol = length(var_vec),
    dimnames = list(cluster_vec, var_vec)
  )
  for (clust in cluster_vec) {
    for (var in var_vec) {
      expr_mat[clust, var] <- plot_tbl$perc[
        plot_tbl$cluster == clust & plot_tbl$variable == var
      ]
    }
  }
  expr_mat
}

.plot_cluster_heatmap_plot <- function(plot_tbl,
                                       order_list,
                                       col,
                                       col_positions,
                                       white_range,
                                       thm,
                                       grid,
                                       show_values,
                                       values_format,
                                       values_col,
                                       values_size,
                                       scale_method) {
  plot_tbl <- plot_tbl |>
    dplyr::mutate(
      cluster = factor(.data$cluster, levels = order_list$cluster),
      variable = factor(.data$variable, levels = order_list$variable)
    )

  n_col <- length(col)
  if (identical(col_positions, "auto")) {
    if (n_col == 3 && scale_method == "ecdf") {
      plot_colours <- c(col[1], col[2], col[2], col[3])
      plot_positions <- c(0, white_range[1], white_range[2], 1)
    } else {
      plot_colours <- col
      plot_positions <- seq(0, 1, length.out = n_col)
    }
  } else {
    plot_colours <- col
    plot_positions <- col_positions
  }

  bounded <- scale_method %in% c("ecdf", "minmax", "minmax_var")
  fill_scale <- if (bounded) {
    ggplot2::scale_fill_gradientn(
      colours = plot_colours,
      values = plot_positions,
      limits = c(0, 1),
      name = "Relative\nvalue",
      labels = scales::percent
    )
  } else {
    legend_name <- if (scale_method == "zscore") "Z-score" else "Median"
    ggplot2::scale_fill_gradientn(
      colours = plot_colours,
      values = plot_positions,
      name = legend_name
    )
  }

  p <- ggplot2::ggplot(
    plot_tbl,
    ggplot2::aes(x = .data$cluster, y = .data$variable, fill = .data$perc)
  ) +
    ggplot2::geom_raster() +
    fill_scale +
    ggplot2::labs(x = "Cluster", y = "Variable")

  if (!is.null(thm)) {
    p <- p + thm
  }
  if (!is.null(grid)) {
    p <- p + grid
  }
  if (show_values) {
    if (is.null(values_format)) {
      values_format <- function(x) formatC(x, digits = 3, format = "g")
    }
    plot_tbl <- plot_tbl |>
      dplyr::mutate(label = values_format(.data$med))
    p <- p + ggplot2::geom_text(
      data = plot_tbl,
      mapping = ggplot2::aes(
        x = .data$cluster,
        y = .data$variable,
        label = .data$label
      ),
      colour = values_col,
      size = values_size,
      inherit.aes = FALSE
    )
  }

  p
}
