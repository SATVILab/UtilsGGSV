#' @md
#' @title Biaxial scatter plot with cluster medians overlaid
#'
#' @description
#' Creates a biaxial scatter plot with observations colored by cluster assignment.
#' Median cluster centroids are overlaid as large points and labeled with cluster names.
#' The plot can use either raw variables (specified by the user) or dimensionality
#' reduction components as axes.
#'
#' @param .data data.frame. Rows are observations. Must contain a column identifying cluster membership and numeric variables.
#' @param cluster character. Name of the column in `.data` that identifies cluster membership.
#' @param dim_red character or `NULL`. Dimensionality reduction method: one of `"none"`, `"pca"`, `"tsne"`, `"umap"`. If `NULL`, auto-selects `"none"` when exactly 2 numeric vars are available, otherwise `"pca"`.
#' @param vars character vector or `NULL`. Names of numeric columns in `.data` to use for the plot or reduction. If `NULL`, uses all numeric columns except `cluster` and `point_col_var`.
#' @param point_col_var character or `NULL`. Column to use for point colour mapping. Default is same as `cluster`.
#' @param point_col named vector or NULL. Custom colours for discrete `point_col_var` (named by level) or colour bounds for continuous `point_col_var` (length 3 low/mid/high).
#' @param point_size numeric. Size of observation points. Default is `2`.
#' @param point_alpha numeric. Alpha transparency for observation points and legend guide. Default is `0.65`.
#' @param centroid_size numeric. Size of centroid points. Default is `3`.
#' @param label_size numeric. Font size for centroid labels. Default is `4`.
#' @param label_offset numeric. Label repulsion padding for centroid labels in cm. Default is `0.3`.
#' @param ggrepel logical. Use `ggrepel::geom_text_repel` for centroid labels. Default is `TRUE`.
#' @param font_size numeric. Font size passed to `cowplot::theme_cowplot`. Default is `14`.
#' @param thm ggplot2 theme object or `NULL`. Default is `cowplot::theme_cowplot(font_size = font_size)` with white background.
#' @param x_lab character or `NULL`. Label for x axis; default uses reduction variable names.
#' @param y_lab character or `NULL`. Label for y axis; default uses reduction variable names.
#' @param show_legend logical. Whether to show the legend. Default is `TRUE`.
#'   Set to `FALSE` to hide the legend, as centroid labels may suffice.
#' @param grid ggplot2 layer or `NULL`. Background grid added to the plot.
#'   Default is `cowplot::background_grid(major = "xy")`. Set to `NULL` to
#'   suppress the grid.
#' @importFrom stats aggregate setNames
#' @export
#'
#' @examples
#' set.seed(1)
#' data <- data.frame(
#'   cluster = rep(paste0("C", 1:3), each = 20),
#'   var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2)),
#'   var2 = c(rnorm(20, -1), rnorm(20, 1), rnorm(20, 0)),
#'   var3 = c(rnorm(20, 1), rnorm(20, -1), rnorm(20, 0))
#' )
#' plot_cluster_scatter(data, cluster = "cluster")
#' plot_cluster_scatter(data, cluster = "cluster", dim_red = "none", vars = c("var1", "var2"))
#' plot_cluster_scatter(data, cluster = "cluster", show_legend = FALSE)
plot_cluster_scatter <- function(.data,
                                 cluster,
                                 dim_red = NULL,
                                 vars = NULL,
                                 point_col_var = NULL,
                                 point_col = NULL,
                                 point_size = 2,
                                 point_alpha = 0.65,
                                 centroid_size = 3,
                                 label_size = 4,
                                 label_offset = 0.3,
                                 ggrepel = TRUE,
                                 font_size = 14,
                                 x_lab = NULL,
                                 y_lab = NULL,
                                 show_legend = TRUE,
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
  if (!is.data.frame(.data)) {
    stop(".data must be a data frame.")
  }

  if (!is.character(cluster) || length(cluster) != 1 || !(cluster %in% colnames(.data))) {
    stop("cluster must be a single column name present in .data")
  }

  if (is.null(point_col_var)) {
    point_col_var <- cluster
  }

  if (!is.character(point_col_var) || length(point_col_var) != 1 || !(point_col_var %in% colnames(.data))) {
    stop("point_col_var must be a single column name present in .data")
  }

  if (!is.null(vars)) {
    if (!all(vars %in% colnames(.data))) {
      stop("All vars must exist in .data")
    }
    if (!all(sapply(.data[, vars, drop = FALSE], is.numeric))) {
      stop("All vars must be numeric")
    }
  }

  input_vars <- if (is.null(vars)) {
    candidates <- setdiff(colnames(.data), unique(c(cluster, point_col_var)))
    candidates[sapply(.data[, candidates, drop = FALSE], is.numeric)]
  } else {
    vars
  }

  if (length(input_vars) == 0) {
    stop("No numeric variables found for dimensionality reduction.")
  }

  if (is.null(dim_red)) {
    if (length(input_vars) == 2) {
      dim_red <- "none"
      message("dim_red automatically set to 'none' because exactly two numeric variables are available.")
    } else {
      dim_red <- "pca"
      message("dim_red automatically set to 'pca' because more than two numeric variables are available.")
    }
  }

  dim_red <- match.arg(dim_red, c("none", "pca", "tsne", "umap"))

  if (dim_red != "none" && length(input_vars) < 2) {
    stop("dim_red=\"", dim_red, "\" requires at least two variables in vars or numeric variables in .data.")
  }

  if (dim_red == "none" && length(input_vars) < 2) {
    stop("dim_red=\"none\" requires at least two variables in vars or numeric variables in .data.")
  }

  complete_idx <- stats::complete.cases(.data[, input_vars, drop = FALSE])
  if (sum(complete_idx) == 0) {
    stop("No complete cases after removing missing values for selected vars.")
  }

  data_clean <- .data[complete_idx, , drop = FALSE]

  if (dim_red == "none") {
    x_var <- input_vars[1]
    y_var <- input_vars[2]
    coords <- data_clean[, c(x_var, y_var), drop = FALSE]
    x_values <- coords[[1]]
    y_values <- coords[[2]]
  } else if (dim_red == "pca") {
    pca_result <- stats::prcomp(data_clean[, input_vars, drop = FALSE], scale. = TRUE)
    if (ncol(pca_result$x) < 2) {
      stop("PCA did not return at least two components.")
    }
    x_values <- pca_result$x[, 1]
    y_values <- pca_result$x[, 2]
    x_var <- "PC1"
    y_var <- "PC2"
  } else if (dim_red == "tsne") {
    if (!requireNamespace("Rtsne", quietly = TRUE)) {
      if (interactive()) {
        stop("Package 'Rtsne' is required for dim_red = 'tsne'. Please install it with install.packages('Rtsne').")
      }
      stop("Package 'Rtsne' is required for dim_red = 'tsne'.")
    }
    n_samples <- nrow(data_clean)
    perplexity <- max(1, min(30, floor((n_samples - 1)/3)))
    if (perplexity < 1) {
      stop("Not enough samples for t-SNE. Need at least 3 observations.")
    }
    tsne_res <- Rtsne::Rtsne(
      data_clean[, input_vars, drop = FALSE],
      dims = 2,
      perplexity = perplexity,
      check_duplicates = FALSE,
      pca = FALSE
    )
    x_values <- tsne_res$Y[, 1]
    y_values <- tsne_res$Y[, 2]
    x_var <- "tSNE1"
    y_var <- "tSNE2"
  } else {
    if (!requireNamespace("umap", quietly = TRUE)) {
      if (interactive()) {
        stop("Package 'umap' is required for dim_red = 'umap'. Please install it with install.packages('umap').")
      }
      stop("Package 'umap' is required for dim_red = 'umap'.")
    }
    umap_res <- umap::umap(data_clean[, input_vars, drop = FALSE])
    x_values <- umap_res$layout[, 1]
    y_values <- umap_res$layout[, 2]
    x_var <- "UMAP1"
    y_var <- "UMAP2"
  }

  plot_data <- tibble::tibble(
    x = x_values,
    y = y_values,
    cluster = data_clean[[cluster]],
    point_col = data_clean[[point_col_var]]
  )

  plot_data$cluster <- as.factor(plot_data$cluster)

  if (point_col_var == cluster) {
    plot_data$point_col <- as.factor(plot_data$point_col)
    point_col_discrete <- TRUE
  } else {
    point_col_discrete <- !is.numeric(plot_data$point_col)
    if (point_col_discrete) {
      plot_data$point_col <- as.factor(plot_data$point_col)
    }
  }

  centroid_tbl <- aggregate(cbind(x = x, y = y) ~ cluster, data = plot_data, FUN = stats::median)

  p <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = x, y = y, colour = point_col)
  ) +
    ggplot2::geom_point(size = point_size, alpha = point_alpha)

  if (point_col_discrete) {
    if (!is.null(point_col)) {
      if (is.null(names(point_col))) {
        n_unique <- length(unique(plot_data$point_col))
        if (length(point_col) < n_unique) {
          stop("point_col must have length at least the number of unique values in point_col_var.")
        }
        point_col <- setNames(point_col[seq_len(n_unique)], levels(plot_data$point_col))
      }
      p <- p + ggplot2::scale_colour_manual(values = point_col)
    } else {
      p <- p + ggplot2::scale_colour_brewer(palette = "Set2")
    }

    if (point_col_var == cluster) {
      if (!is.null(point_col)) {
        p <- p + ggplot2::scale_fill_manual(values = point_col)
      } else {
        p <- p + ggplot2::scale_fill_brewer(palette = "Set2")
      }
    } else {
      p <- p + ggplot2::scale_fill_brewer(palette = "Set2")
    }
  } else {
    col_vals <- if (is.null(point_col)) {
      c("#D53E4F", "white", "#3288BD")
    } else {
      if (length(point_col) != 3) {
        stop("For continuous point_col_var, point_col should be a numeric or character vector of length 3 for low/mid/high colours.")
      }
      point_col
    }
    midpoint <- mean(range(plot_data$point_col, na.rm = TRUE))
    p <- p + ggplot2::scale_colour_gradient2(low = col_vals[1], mid = col_vals[2], high = col_vals[3], midpoint = midpoint)
    p <- p + ggplot2::scale_fill_brewer(palette = "Set2")
  }

  if (!is.null(label_offset)) {
    if (ggrepel) {
      p <- p + ggrepel::geom_text_repel(
        data = centroid_tbl,
        ggplot2::aes(x = x, y = y, label = cluster),
        inherit.aes = FALSE,
        size = label_size,
        fontface = "bold",
        min.segment.length = 0,
        segment.colour = "black",
        point.padding = ggplot2::unit(label_offset, "cm")
      )
    } else {
      p <- p + ggplot2::geom_text(
        data = centroid_tbl,
        ggplot2::aes(x = x, y = y, label = cluster),
        inherit.aes = FALSE,
        colour = "black",
        size = label_size,
        fontface = "bold",
        vjust = -0.5,
        hjust = 0
      )
    }
  }

  p <- p +
    ggplot2::geom_point(
      data = centroid_tbl,
      ggplot2::aes(x = x, y = y, fill = cluster),
      inherit.aes = FALSE,
      size = centroid_size,
      alpha = 0.9,
      colour = "black",
      shape = 21
    ) +
    ggplot2::labs(
      x = if (is.null(x_lab)) x_var else x_lab,
      y = if (is.null(y_lab)) y_var else y_lab,
      colour = if (point_col_var == cluster) "Cluster" else point_col_var,
      fill = "Cluster"
    ) +
    ggplot2::guides(colour = ggplot2::guide_legend(override.aes = list(alpha = point_alpha)))

  if (!is.null(thm)) p <- p + thm
  if (!is.null(grid)) p <- p + grid

  if (!show_legend) {
    p <- p + ggplot2::theme(legend.position = "none")
  }

  p
}

