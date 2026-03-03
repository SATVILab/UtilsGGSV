#' @md
#' @title Plot density of variable values with per-cluster median lines
#'
#' @description
#' For each variable, plots the overall density of values across all
#' observations and overlays a vertical line for each cluster at that
#' cluster's median value for the variable. Each cluster is given a
#' distinct colour. When multiple variables are provided the plots are
#' arranged in a faceted grid with free scales.
#'
#' @param data data.frame. Rows are observations. Must contain a column
#'   identifying cluster membership and columns for variable values.
#' @param cluster character. Name of the column in `data` that identifies
#'   cluster membership.
#' @param vars character vector or `NULL`. Names of columns in `data` to
#'   use as variables. If `NULL`, all columns except `cluster` are used.
#'   Default is `NULL`.
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
#' plot_clust_density(data, cluster = "cluster")
plot_clust_density <- function(data,
                               cluster,
                               vars = NULL,
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

  long_tbl <- purrr::map_df(vars, function(v) {
    tibble::tibble(
      variable = v,
      value = data[[v]]
    )
  })

  median_tbl <- purrr::map_df(vars, function(v) {
    purrr::map_df(cluster_vec, function(cl) {
      tibble::tibble(
        variable = v,
        cluster = cl,
        median = stats::median(data[[v]][data[[cluster]] == cl])
      )
    })
  })

  p <- ggplot2::ggplot(long_tbl, ggplot2::aes(x = .data$value)) +
    ggplot2::geom_density() +
    ggplot2::geom_vline(
      data = median_tbl,
      ggplot2::aes(xintercept = .data$median, colour = .data$cluster)
    ) +
    ggplot2::facet_wrap(~ .data$variable, scales = "free") +
    ggplot2::labs(x = "Value", y = "Density", colour = "Cluster")

  if (!is.null(thm)) {
    p <- p + thm
  }
  if (!is.null(grid)) {
    p <- p + grid
  }

  p
}
