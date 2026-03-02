#' @md
#' @title Plot heat map of ECDF-standardised variable values per cluster
#'
#' @description
#' Creates a heat map where each tile shows the percentile of the median
#' value of a variable for a cluster, compared against the empirical cumulative
#' distribution function (ECDF) of that variable across all observations not
#' belonging to the cluster. Clusters and variables are ordered along the axes
#' via hierarchical clustering.
#'
#' @param data data.frame. Rows are observations. Must contain a column
#'   identifying cluster membership and columns for variable values.
#' @param cluster character. Name of the column in `data` that identifies
#'   cluster membership.
#' @param vars character vector or `NULL`. Names of columns in `data` to
#'   use as variables. If `NULL`, all columns except `cluster` are used.
#'   Default is `NULL`.
#' @param col_high character. Colour for high values (100th percentile).
#'   Default is `"#B2182B"`.
#' @param col_mid character. Colour for the middle of the value range.
#'   Default is `"#F7F7F7"`.
#' @param col_low character. Colour for low values (0th percentile).
#'   Default is `"#2166AC"`.
#' @param white_range numeric vector of length 2. The range of percentile
#'   values (on a 0-1 scale) that map to `col_mid`. Default is `c(0.4, 0.6)`.
#' @param font_size numeric. Font size passed to `cowplot::theme_cowplot`.
#'   Default is `14`.
#' @param thm ggplot2 theme object or `NULL`. Default is
#'   `cowplot::theme_cowplot(font_size = font_size)` with a white plot
#'   background. Set to `NULL` to apply no theme adjustment.
#' @param grid ggplot2 panel grid or `NULL`. Default is
#'   `cowplot::background_grid(major = "xy")`. Set to `NULL` for no grid.
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
#' plot_heatmap_cluster(data, cluster = "cluster")
plot_heatmap_cluster <- function(data,
                                 cluster,
                                 vars = NULL,
                                 col_high = "#B2182B",
                                 col_mid = "#F7F7F7",
                                 col_low = "#2166AC",
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
                                 )) {
  if (is.null(vars)) {
    vars <- setdiff(colnames(data), cluster)
  }

  cluster_vec <- unique(data[[cluster]])

  plot_tbl <- .plot_heatmap_cluster_calc(data, cluster, vars, cluster_vec)
  order_list <- .plot_heatmap_cluster_order(plot_tbl)
  colour_values <- c(0, white_range[1], white_range[2], 1)

  .plot_heatmap_cluster_plot(
    plot_tbl = plot_tbl,
    order_list = order_list,
    col_high = col_high,
    col_mid = col_mid,
    col_low = col_low,
    colour_values = colour_values,
    thm = thm,
    grid = grid
  )
}

.plot_heatmap_cluster_calc <- function(data, cluster, vars, cluster_vec) {
  purrr::map_df(cluster_vec, function(clust) {
    obs_in <- data[[cluster]] == clust
    data_out <- data[!obs_in, ]
    purrr::map_df(vars, function(var) {
      med <- stats::median(data[[var]][obs_in])
      ecdf_fn <- stats::ecdf(data_out[[var]])
      tibble::tibble(
        cluster = clust,
        variable = var,
        perc = ecdf_fn(med)
      )
    })
  })
}

.plot_heatmap_cluster_order <- function(plot_tbl) {
  cluster_vec <- unique(plot_tbl$cluster)
  var_vec <- unique(plot_tbl$variable)

  if (length(cluster_vec) <= 1 || length(var_vec) <= 1) {
    return(list(cluster = cluster_vec, variable = var_vec))
  }

  expr_mat <- .plot_heatmap_cluster_order_mat(plot_tbl, cluster_vec, var_vec)

  list(
    cluster = cluster_vec[stats::hclust(stats::dist(expr_mat))$order],
    variable = var_vec[stats::hclust(stats::dist(t(expr_mat)))$order]
  )
}

.plot_heatmap_cluster_order_mat <- function(plot_tbl, cluster_vec, var_vec) {
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

.plot_heatmap_cluster_plot <- function(plot_tbl,
                                       order_list,
                                       col_high,
                                       col_mid,
                                       col_low,
                                       colour_values,
                                       thm,
                                       grid) {
  plot_tbl <- plot_tbl |>
    dplyr::mutate(
      cluster = factor(.data$cluster, levels = order_list$cluster),
      variable = factor(.data$variable, levels = order_list$variable)
    )

  p <- ggplot2::ggplot(
    plot_tbl,
    ggplot2::aes(x = .data$cluster, y = .data$variable, fill = .data$perc)
  ) +
    ggplot2::geom_raster() +
    ggplot2::scale_fill_gradientn(
      colours = c(col_low, col_mid, col_mid, col_high),
      values = colour_values,
      limits = c(0, 1),
      name = "Relative\nvalue",
      labels = scales::percent
    ) +
    ggplot2::labs(x = "Cluster", y = "Variable")

  if (!is.null(thm)) {
    p <- p + thm
  }
  if (!is.null(grid)) {
    p <- p + grid
  }

  p
}
