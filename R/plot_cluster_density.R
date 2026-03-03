#' @md
#' @title Plot density of variable values with per-cluster median lines
#'
#' @description
#' For each variable, plots the overall density of values across all
#' observations and overlays a vertical line for each cluster at that
#' cluster's median value for the variable. Each cluster is given a
#' distinct colour.
#'
#' By default the function returns a **named list of ggplot2 objects**, one
#' per variable. If `n_col` or `n_row` is supplied the plots are instead
#' combined into a **single faceted ggplot2 object** via `facet_wrap`.
#'
#' @param data data.frame. Rows are observations. Must contain a column
#'   identifying cluster membership and columns for variable values.
#' @param cluster character. Name of the column in `data` that identifies
#'   cluster membership.
#' @param vars character vector or `NULL`. Names of columns in `data` to
#'   use as variables. If `NULL`, all columns except `cluster` are used.
#'   Default is `NULL`.
#' @param col_clusters named character vector or `NULL`. Per-cluster colours.
#'   Names should match cluster labels. When `NULL` (default), the default
#'   ggplot2 colour scale is used.
#' @param n_col integer or `NULL`. Number of columns passed to
#'   `ggplot2::facet_wrap`. If supplied (or if `n_row` is supplied) a single
#'   faceted plot is returned instead of a list. Default is `NULL`.
#' @param n_row integer or `NULL`. Number of rows passed to
#'   `ggplot2::facet_wrap`. If supplied (or if `n_col` is supplied) a single
#'   faceted plot is returned instead of a list. Default is `NULL`.
#' @param scales character. The `scales` argument passed to
#'   `ggplot2::facet_wrap` when a faceted plot is requested. Default is
#'   `"free_y"` so that the x-axis is shared across panels.
#' @param expand_coord numeric vector or named list or `NULL`. Expands the
#'   x-axis limits to include the given values. A plain numeric vector is
#'   applied to every variable. A named list (names = variable names, values =
#'   numeric vectors) applies expansion per variable. When a faceted plot is
#'   requested via `n_col`/`n_row` and a named list is provided, a warning is
#'   issued and `expand_coord` is ignored (incompatible with faceting).
#'   Default is `NULL`.
#' @param exclude_min character. Whether to exclude observations whose value
#'   equals the minimum from the density and median calculations. Options are:
#'   `"no"` (default, no exclusion), `"overall"` (exclude observations whose
#'   value equals the global minimum across all variables), or `"variable"`
#'   (for each variable, exclude observations whose value equals that
#'   variable's minimum).
#' @param font_size numeric. Font size passed to `cowplot::theme_cowplot`.
#'   Default is `14`.
#' @param thm ggplot2 theme object or `NULL`. Default is
#'   `cowplot::theme_cowplot(font_size = font_size)` with a white plot
#'   background. Set to `NULL` to apply no theme adjustment.
#' @param col_clusters named character vector or `NULL`. Per-cluster colours.
#'   Names should match cluster labels. When `NULL` (default), the default
#'   ggplot2 colour scale is used.
#' @param grid ggplot2 panel grid or `NULL`. Default is
#'   `cowplot::background_grid(major = "xy")`. Set to `NULL` for no grid.
#'
#' @return A named list of ggplot2 objects (one per variable) when neither
#'   `n_col` nor `n_row` is specified. A single ggplot2 object with
#'   `facet_wrap` panels when `n_col` or `n_row` is specified.
#' @export
#'
#' @examples
#' set.seed(1)
#' data <- data.frame(
#'   cluster = rep(paste0("C", 1:3), each = 20),
#'   var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2)),
#'   var2 = c(rnorm(20, -1), rnorm(20, 1), rnorm(20, 0))
#' )
#' # Default: returns a list of plots
#' plot_list <- plot_cluster_density(data, cluster = "cluster")
#'
#' # Faceted plot with 2 columns
#' plot_cluster_density(data, cluster = "cluster", n_col = 2)
plot_cluster_density <- function(data,
                                 cluster,
                                 vars = NULL,
                                 col_clusters = NULL,
                                 n_col = NULL,
                                 n_row = NULL,
                                 scales = "free_y",
                                 expand_coord = NULL,
                                 exclude_min = "no",
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
  exclude_min <- match.arg(exclude_min, c("no", "overall", "variable"))

  if (is.null(vars)) {
    vars <- setdiff(colnames(data), cluster)
  }

  use_facet <- !is.null(n_col) || !is.null(n_row)

  if (use_facet && is.list(expand_coord) && !is.null(names(expand_coord))) {
    warning(
      "`expand_coord` is a named list, which is incompatible with facet_wrap ",
      "mode (n_col/n_row specified). Ignoring `expand_coord`."
    )
    expand_coord <- NULL
  }

  global_min <- if (exclude_min == "overall") {
    min(unlist(lapply(vars, function(v) data[[v]])), na.rm = TRUE)
  } else {
    NULL
  }

  var_mins <- if (exclude_min == "variable") {
    stats::setNames(
      lapply(vars, function(v) min(data[[v]], na.rm = TRUE)),
      vars
    )
  } else {
    NULL
  }

  .filter_vals <- function(vals, v) {
    if (exclude_min == "no") return(vals)
    min_val <- if (exclude_min == "overall") global_min else var_mins[[v]]
    vals[vals != min_val]
  }

  cluster_vec <- unique(data[[cluster]])

  if (!use_facet) {
    plot_list <- stats::setNames(
      lapply(vars, function(v) {
        all_vals <- data[[v]]
        dens_vals <- .filter_vals(all_vals, v)
        dens_tbl <- tibble::tibble(value = dens_vals)

        med_tbl <- purrr::map_df(cluster_vec, function(cl) {
          cl_vals <- .filter_vals(all_vals[data[[cluster]] == cl], v)
          tibble::tibble(
            cluster = cl,
            median = stats::median(cl_vals, na.rm = TRUE)
          )
        })

        p <- ggplot2::ggplot(dens_tbl, ggplot2::aes(x = .data$value)) +
          ggplot2::geom_density() +
          ggplot2::geom_vline(
            data = med_tbl,
            ggplot2::aes(xintercept = .data$median, colour = .data$cluster)
          ) +
          ggplot2::labs(x = v, y = "Density", colour = "Cluster")

        if (!is.null(expand_coord)) {
          ec <- if (is.list(expand_coord)) expand_coord[[v]] else expand_coord
          if (!is.null(ec)) p <- p + ggplot2::expand_limits(x = ec)
        }

        if (!is.null(col_clusters)) {
          p <- p + ggplot2::scale_colour_manual(values = col_clusters)
        }
        if (!is.null(thm)) p <- p + thm
        if (!is.null(grid)) p <- p + grid

        p
      }),
      vars
    )
    return(plot_list)
  }

  long_tbl <- purrr::map_df(vars, function(v) {
    tibble::tibble(variable = v, value = .filter_vals(data[[v]], v))
  })

  median_tbl <- purrr::map_df(vars, function(v) {
    purrr::map_df(cluster_vec, function(cl) {
      vals <- .filter_vals(data[[v]][data[[cluster]] == cl], v)
      tibble::tibble(
        variable = v,
        cluster = cl,
        median = stats::median(vals, na.rm = TRUE)
      )
    })
  })

  p <- ggplot2::ggplot(long_tbl, ggplot2::aes(x = .data$value)) +
    ggplot2::geom_density() +
    ggplot2::geom_vline(
      data = median_tbl,
      ggplot2::aes(xintercept = .data$median, colour = .data$cluster)
    ) +
    ggplot2::facet_wrap(
      ~ .data$variable,
      scales = scales,
      ncol = n_col,
      nrow = n_row
    ) +
    ggplot2::labs(x = "Value", y = "Density", colour = "Cluster")

  if (!is.null(expand_coord) && is.numeric(expand_coord)) {
    p <- p + ggplot2::expand_limits(x = expand_coord)
  }

  if (!is.null(col_clusters)) {
    p <- p + ggplot2::scale_colour_manual(values = col_clusters)
  }
  if (!is.null(thm)) p <- p + thm
  if (!is.null(grid)) p <- p + grid

  p
}
