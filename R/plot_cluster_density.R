#' @md
#' @title Plot density of variable values with per-cluster median lines
#'
#' @description
#' For each variable, plots the density of values and optionally overlays
#' per-cluster density curves or cluster median lines. The `density` argument
#' controls what is shown:
#'
#' - `"overall"` (default): the overall density of all observations with a
#'   vertical line per cluster at that cluster's median value.
#' - `"cluster"`: one density curve per cluster, coloured by cluster.
#' - `"both"`: the overall density curve and one density curve per cluster,
#'   with the cluster curves scaled according to the `scale` argument.
#'
#' A rug is added by default. The `rug` argument controls which data the rug
#' represents:
#'
#' - `NULL` (default): per-cluster rug when `density` is `"cluster"` or
#'   `"both"`, overall rug when `density` is `"overall"`.
#' - `"cluster"`: per-cluster rug, coloured by cluster.
#' - `"overall"`: overall rug (no cluster colouring).
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
#' @param density character. What density to display. One of `"overall"`
#'   (default: overall density curve plus cluster median lines), `"cluster"`
#'   (one density curve per cluster, coloured by cluster), or `"both"` (overall
#'   density curve plus per-cluster density curves). When `"cluster"` or
#'   `"both"`, the `scale` argument controls how cluster densities are scaled.
#' @param scale character. How to scale per-cluster density curves. Only
#'   relevant when `density` is `"cluster"` or `"both"`. One of
#'   `"max_overall"` (default: each cluster density is rescaled so that its
#'   maximum equals the maximum of the overall density, keeping y-axis values
#'   comparable to the overall density), `"max_cluster"` (no rescaling;
#'   y-axis is determined by the tallest curve), or `"free"` (no rescaling;
#'   equivalent to `"max_cluster"`).
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
#' @param rug character or `NULL`. Controls the rug added below the density.
#'   `NULL` (default): per-cluster rug when `density` is `"cluster"` or
#'   `"both"`, overall rug when `density` is `"overall"`. `"cluster"`:
#'   per-cluster rug, coloured by cluster. `"overall"`: overall rug with no
#'   cluster colouring.
#' @param density_overall_weight character or `NULL`. Controls weighting of the
#'   overall density when `density` is `"overall"` or `"both"`. `NULL`
#'   (default): the overall density is estimated from all observations pooled
#'   together. `"even"`: the overall density is computed as an equal-weight
#'   average of per-cluster kernel densities (using the Sheather-Jones
#'   bandwidth for each cluster), preventing larger clusters from dominating
#'   the density estimate. Ignored when `density` is `"cluster"`.
#' @param font_size numeric. Font size passed to `cowplot::theme_cowplot`.
#'   Default is `14`.
#' @param thm ggplot2 theme object or `NULL`. Default is
#'   `cowplot::theme_cowplot(font_size = font_size)` with a white plot
#'   background. Set to `NULL` to apply no theme adjustment.
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
#' # Default: overall density with cluster median lines
#' plot_list <- plot_cluster_density(data, cluster = "cluster")
#'
#' # Per-cluster density curves
#' plot_cluster_density(data, cluster = "cluster", density = "cluster")
#'
#' # Both overall and per-cluster densities (default scaling: max_overall)
#' plot_cluster_density(data, cluster = "cluster", density = "both")
#'
#' # Even-weighted overall density
#' plot_cluster_density(
#'   data, cluster = "cluster", density_overall_weight = "even"
#' )
#'
#' # Faceted plot with 2 columns
#' plot_cluster_density(data, cluster = "cluster", n_col = 2)
plot_cluster_density <- function(data,
                                 cluster,
                                 vars = NULL,
                                 col_clusters = NULL,
                                 n_col = NULL,
                                 n_row = NULL,
                                 density = "overall",
                                 scale = "max_overall",
                                 scales = "free_y",
                                 expand_coord = NULL,
                                 exclude_min = "no",
                                 rug = NULL,
                                 density_overall_weight = NULL,
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
  density <- match.arg(density, c("overall", "cluster", "both"))
  scale <- match.arg(scale, c("max_overall", "max_cluster", "free"))
  exclude_min <- match.arg(exclude_min, c("no", "overall", "variable"))
  if (!is.null(rug)) rug <- match.arg(rug, c("overall", "cluster"))
  if (!is.null(density_overall_weight)) {
    density_overall_weight <- match.arg(
      density_overall_weight, c("even")
    )
  }

  # Coerce cluster column to character unless it is already a factor,
  # ensuring ggplot2 always treats cluster as a discrete variable.
  if (!is.factor(data[[cluster]])) {
    data[[cluster]] <- as.character(data[[cluster]])
  }

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

  # Helper: compute a density tibble from a vector of values.
  .dens_tbl <- function(vals) {
    if (length(vals) < 2) return(NULL)
    d <- stats::density(vals)
    tibble::tibble(x = d$x, y = d$y)
  }

  # Helper: compute the overall density as an equal-weight average of
  # per-cluster kernel densities, each estimated with the Sheather-Jones
  # bandwidth. Falls back to the default bandwidth if SJ fails.
  .even_weight_dens_tbl <- function(dens_vals, all_vals, v) {
    if (length(dens_vals) < 2) return(NULL)
    from <- min(dens_vals, na.rm = TRUE)
    to <- max(dens_vals, na.rm = TRUE)
    n_grid <- 512L
    x_grid <- seq(from, to, length.out = n_grid)

    y_list <- lapply(cluster_vec, function(cl) {
      cl_vals <- .filter_vals(all_vals[data[[cluster]] == cl], v)
      if (length(cl_vals) < 2) return(NULL)
      d <- tryCatch(
        stats::density(
          cl_vals, from = from, to = to, n = n_grid, bw = "SJ"
        ),
        error = function(e) {
          warning(
            "SJ bandwidth estimation failed for a cluster; ",
            "falling back to default bandwidth. Original error: ",
            conditionMessage(e),
            call. = FALSE
          )
          stats::density(cl_vals, from = from, to = to, n = n_grid)
        }
      )
      d$y
    })

    valid_y <- Filter(Negate(is.null), y_list)
    if (length(valid_y) == 0) return(.dens_tbl(dens_vals))

    y_mat <- do.call(cbind, valid_y)
    tibble::tibble(x = x_grid, y = rowMeans(y_mat))
  }

  # Helper: compute per-cluster density tibbles, optionally scaled.
  .cluster_dens_tbl <- function(all_vals, v, max_y_ref = NULL) {
    purrr::map_df(cluster_vec, function(cl) {
      cl_vals <- .filter_vals(all_vals[data[[cluster]] == cl], v)
      d <- .dens_tbl(cl_vals)
      if (is.null(d)) return(tibble::tibble())
      if (!is.null(max_y_ref)) {
        max_cl_y <- max(d$y)
        if (max_cl_y > 0) d$y <- d$y * max_y_ref / max_cl_y
      }
      d$cluster <- cl
      d
    })
  }

  # Helper: choose the effective rug mode.
  .rug_mode <- function() {
    if (!is.null(rug)) return(rug)
    if (density %in% c("cluster", "both")) "cluster" else "overall"
  }

  # Helper: add a rug layer to a plot (non-faceted).
  .add_rug <- function(p, all_vals, dens_vals, v) {
    mode <- .rug_mode()
    if (mode == "overall") {
      rug_tbl <- tibble::tibble(rug_x = dens_vals)
      p + ggplot2::geom_rug(
        data = rug_tbl,
        ggplot2::aes(x = .data$rug_x),
        sides = "b"
      )
    } else {
      rug_cl_tbl <- purrr::map_df(cluster_vec, function(cl) {
        cl_vals <- .filter_vals(all_vals[data[[cluster]] == cl], v)
        tibble::tibble(rug_x = cl_vals, cluster = cl)
      })
      p + ggplot2::geom_rug(
        data = rug_cl_tbl,
        ggplot2::aes(x = .data$rug_x, colour = .data$cluster),
        sides = "b"
      )
    }
  }

  # Helper: add a rug layer to a faceted plot.
  .add_rug_facet <- function(p) {
    mode <- .rug_mode()
    if (mode == "overall") {
      rug_long_tbl <- purrr::map_df(vars, function(v) {
        tibble::tibble(
          variable = v,
          rug_x = .filter_vals(data[[v]], v)
        )
      })
      p + ggplot2::geom_rug(
        data = rug_long_tbl,
        ggplot2::aes(x = .data$rug_x),
        sides = "b"
      )
    } else {
      rug_long_tbl <- purrr::map_df(vars, function(v) {
        purrr::map_df(cluster_vec, function(cl) {
          cl_vals <- .filter_vals(data[[v]][data[[cluster]] == cl], v)
          tibble::tibble(variable = v, rug_x = cl_vals, cluster = cl)
        })
      })
      p + ggplot2::geom_rug(
        data = rug_long_tbl,
        ggplot2::aes(x = .data$rug_x, colour = .data$cluster),
        sides = "b"
      )
    }
  }

  if (!use_facet) {
    plot_list <- stats::setNames(
      lapply(vars, function(v) {
        all_vals <- data[[v]]
        dens_vals <- .filter_vals(all_vals, v)

        if (density == "overall") {
          med_tbl <- purrr::map_df(cluster_vec, function(cl) {
            cl_vals <- .filter_vals(all_vals[data[[cluster]] == cl], v)
            tibble::tibble(
              cluster = cl,
              median = stats::median(cl_vals, na.rm = TRUE)
            )
          })

          if (!is.null(density_overall_weight)) {
            overall_d <- .even_weight_dens_tbl(dens_vals, all_vals, v)
            p <- ggplot2::ggplot() +
              ggplot2::geom_line(
                data = overall_d,
                ggplot2::aes(x = .data$x, y = .data$y)
              ) +
              ggplot2::geom_vline(
                data = med_tbl,
                ggplot2::aes(
                  xintercept = .data$median, colour = .data$cluster
                )
              ) +
              ggplot2::labs(x = v, y = "Density", colour = "Cluster")
          } else {
            dens_tbl <- tibble::tibble(value = dens_vals)
            p <- ggplot2::ggplot(
              dens_tbl, ggplot2::aes(x = .data$value)
            ) +
              ggplot2::geom_density() +
              ggplot2::geom_vline(
                data = med_tbl,
                ggplot2::aes(
                  xintercept = .data$median, colour = .data$cluster
                )
              ) +
              ggplot2::labs(x = v, y = "Density", colour = "Cluster")
          }
        } else {
          if (!is.null(density_overall_weight)) {
            overall_d <- .even_weight_dens_tbl(dens_vals, all_vals, v)
          } else {
            overall_d <- .dens_tbl(dens_vals)
          }
          max_y_ref <- if (scale == "max_overall" && !is.null(overall_d)) {
            max(overall_d$y)
          } else {
            NULL
          }
          cl_dens <- .cluster_dens_tbl(all_vals, v, max_y_ref)

          if (density == "cluster") {
            p <- ggplot2::ggplot(
              cl_dens,
              ggplot2::aes(
                x = .data$x, y = .data$y, colour = .data$cluster
              )
            ) +
              ggplot2::geom_line() +
              ggplot2::labs(x = v, y = "Density", colour = "Cluster")
          } else {
            p <- ggplot2::ggplot() +
              ggplot2::geom_line(
                data = overall_d,
                ggplot2::aes(x = .data$x, y = .data$y)
              ) +
              ggplot2::geom_line(
                data = cl_dens,
                ggplot2::aes(
                  x = .data$x, y = .data$y, colour = .data$cluster
                )
              ) +
              ggplot2::labs(x = v, y = "Density", colour = "Cluster")
          }
        }

        p <- .add_rug(p, all_vals, dens_vals, v)

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

  if (density == "overall") {
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

    if (!is.null(density_overall_weight)) {
      overall_dens_tbl <- purrr::map_df(vars, function(v) {
        all_vals <- data[[v]]
        dens_vals <- .filter_vals(all_vals, v)
        d <- .even_weight_dens_tbl(dens_vals, all_vals, v)
        if (is.null(d)) return(tibble::tibble())
        d$variable <- v
        d
      })
      p <- ggplot2::ggplot() +
        ggplot2::geom_line(
          data = overall_dens_tbl,
          ggplot2::aes(x = .data$x, y = .data$y)
        ) +
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
    } else {
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
    }
  } else {
    overall_dens_tbl <- purrr::map_df(vars, function(v) {
      all_vals <- data[[v]]
      dens_vals <- .filter_vals(all_vals, v)
      d <- if (!is.null(density_overall_weight)) {
        .even_weight_dens_tbl(dens_vals, all_vals, v)
      } else {
        .dens_tbl(dens_vals)
      }
      if (is.null(d)) return(tibble::tibble())
      d$variable <- v
      d
    })

    cluster_dens_tbl <- purrr::map_df(vars, function(v) {
      all_vals <- data[[v]]
      dens_vals <- .filter_vals(all_vals, v)
      max_y_ref <- if (scale == "max_overall") {
        od <- if (!is.null(density_overall_weight)) {
          .even_weight_dens_tbl(dens_vals, all_vals, v)
        } else {
          .dens_tbl(dens_vals)
        }
        if (!is.null(od)) max(od$y) else NULL
      } else {
        NULL
      }
      d <- .cluster_dens_tbl(all_vals, v, max_y_ref)
      if (nrow(d) == 0) return(tibble::tibble())
      d$variable <- v
      d
    })

    if (density == "cluster") {
      p <- ggplot2::ggplot(
        cluster_dens_tbl,
        ggplot2::aes(
          x = .data$x, y = .data$y, colour = .data$cluster
        )
      ) +
        ggplot2::geom_line() +
        ggplot2::facet_wrap(
          ~ .data$variable,
          scales = scales,
          ncol = n_col,
          nrow = n_row
        ) +
        ggplot2::labs(x = "Value", y = "Density", colour = "Cluster")
    } else {
      p <- ggplot2::ggplot() +
        ggplot2::geom_line(
          data = overall_dens_tbl,
          ggplot2::aes(x = .data$x, y = .data$y)
        ) +
        ggplot2::geom_line(
          data = cluster_dens_tbl,
          ggplot2::aes(
            x = .data$x, y = .data$y, colour = .data$cluster
          )
        ) +
        ggplot2::facet_wrap(
          ~ .data$variable,
          scales = scales,
          ncol = n_col,
          nrow = n_row
        ) +
        ggplot2::labs(x = "Value", y = "Density", colour = "Cluster")
    }
  }

  p <- .add_rug_facet(p)

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
