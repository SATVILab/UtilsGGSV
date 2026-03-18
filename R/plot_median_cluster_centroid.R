#' @md
#' @title Biaxial scatter plot with cluster medians overlaid
#'
#' @description
#' Creates a biaxial scatter plot with observations colored by cluster assignment.
#' Median cluster centroids are overlaid as large points and labeled with cluster names.
#' The plot can use either raw variables (specified by the user) or dimensionality
#' reduction components as axes.
#'
#' @param data data.frame. Rows are observations. Must contain a column
#'   identifying cluster membership and numeric columns for variable values.
#' @param cluster character. Name of the column in `data` that identifies
#'   cluster membership.
#' @param method character. Whether to use raw variables or dimensionality
#'   reduction. One of `"raw"` or `"pca"`. Default is `"pca"`.
#'   - `"raw"`: Uses specified raw variables as axes
#'   - `"pca"`: Performs PCA on specified variables and uses first two
#'     principal components as axes
#' @param vars character vector or `NULL`. Names of columns in `data` to
#'   use as variables. If `NULL`, all numeric columns except `cluster` are used.
#'   Default is `NULL`.
#' @param col_clusters named character vector or `NULL`. Per-cluster colours.
#'   Names should match cluster labels. When `NULL` (default), the default
#'   ggplot2 colour scale is used.
#' @param point_size numeric. Size of observation points. Default is `2`.
#' @param centroid_size numeric. Size of centroid points. Default is `5`.
#' @param label_size numeric. Font size for centroid labels. Default is `4`.
#' @param label_offset numeric. Offset for labels away from centroid points.
#'   Default is `0.3`.
#' @param font_size numeric. Font size passed to `cowplot::theme_cowplot`.
#'   Default is `14`.
#' @param thm ggplot2 theme object or `NULL`. Default is
#'   `cowplot::theme_cowplot(font_size = font_size)` with a white plot
#'   background. Set to `NULL` to apply no theme adjustment.
#' @param x_lab character or `NULL`. Label for the x-axis. If `NULL` (default),
#'   uses the variable name or "PC1" for PCA.
#' @param y_lab character or `NULL`. Label for the y-axis. If `NULL` (default),
#'   uses the variable name or "PC2" for PCA.
#' @param show_legend logical. Whether to show the legend. Default is `TRUE`.
#'   Set to `FALSE` to hide the legend, as centroid labels may suffice.
#' @export
#'
#' @examples
# set.seed(1)
# data <- data.frame(
#   cluster = rep(paste0("C", 1:3), each = 20),
#   var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2)),
#   var2 = c(rnorm(20, -1), rnorm(20, 1), rnorm(20, 0)),
#   var3 = c(rnorm(20, 1), rnorm(20, -1), rnorm(20, 0))
# )
# # PCA plot (default)
# plot_median_cluster_centroid(data, cluster = "cluster")
# 
# # Raw variables plot
# plot_median_cluster_centroid(data, cluster = "cluster", method = "raw")
# 
# # Customize axis labels and hide legend
# plot_median_cluster_centroid(data, cluster = "cluster", x_lab = "X Axis", y_lab = "Y Axis", show_legend = FALSE) + ggplot2::theme(legend.position = "none")
plot_median_cluster_centroid <- function(data,
                                         cluster,
                                         method = "pca",
                                         vars = NULL,
                                         col_clusters = NULL,
                                         point_size = 2,
                                         centroid_size = 3,
                                         label_size = 4,
                                         label_offset = 0.3,
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
  method <- match.arg(method, c("raw", "pca"))
  
  # Get variable names
  if (is.null(vars)) {
    vars <- setdiff(
      colnames(data),
      cluster
    )
    # Filter to numeric columns
    vars <- vars[sapply(data[, vars, drop = FALSE], is.numeric)]
  }
  
  if (length(vars) == 0) {
    stop("No numeric variables found in data.")
  }
  
  if (method == "pca") {
    if (length(vars) < 2) {
      stop(
        "PCA method requires at least 2 variables. ",
        "Use method='raw' if only using 2 specific variables."
      )
    }
    plot_data <- .plot_median_cluster_pca_data(
      data, cluster, vars
    )
    x_var <- "PC1"
    y_var <- "PC2"
    cluster_col <- ".cluster"
  } else {
    # Raw method: use first two variables
    if (length(vars) < 2) {
      stop(
        "Raw method requires at least 2 variables in `vars`."
      )
    }
    x_var <- vars[1]
    y_var <- vars[2]
    
    plot_data <- data[, c(cluster, x_var, y_var), drop = FALSE]
    plot_data[[".cluster"]] <- plot_data[[cluster]]
    cluster_col <- ".cluster"
  }
  
  cluster_vec <- unique(plot_data[[cluster_col]])
  
  # Calculate median centroids
  centroid_tbl <- purrr::map_df(cluster_vec, function(cl) {
    cl_data <- plot_data[plot_data[[cluster_col]] == cl, ]
    tibble::tibble(
      cluster = cl,
      x = stats::median(cl_data[[x_var]], na.rm = TRUE),
      y = stats::median(cl_data[[y_var]], na.rm = TRUE)
    )
  })
  
  # Create plot
  p <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(
      x = .data[[x_var]],
      y = .data[[y_var]],
      colour = .data[[cluster_col]]
    )
  ) +
    ggplot2::geom_point(size = point_size, alpha = 0.6) +
    ggplot2::geom_point(
      data = centroid_tbl,
      ggplot2::aes(x = .data$x, y = .data$y, fill = .data$cluster),
      size = centroid_size,
      alpha = 0.9,
      colour = "black",
      shape = 21
    ) +
    ggplot2::geom_text(
      data = centroid_tbl,
      ggplot2::aes(
        x = .data$x,
        y = .data$y,
        label = .data$cluster
      ),
      colour = "black",
      size = label_size,
      fontface = "bold",
      vjust = -0.5,  
      hjust = 0 
    ) +
    ggplot2::labs(
      x = if (is.null(x_lab)) x_var else x_lab,
      y = if (is.null(y_lab)) y_var else y_lab,
      colour = "Cluster",
      fill = "Cluster"
    )
  
  if (!is.null(col_clusters)) {
    p <- p + ggplot2::scale_colour_manual(values = col_clusters) +
      ggplot2::scale_fill_manual(values = col_clusters)
  }
  

  if (!is.null(thm)) p <- p + thm
  if (!is.null(grid)) p <- p + grid
  
  if (!show_legend) {
    p <- p + ggplot2::theme(legend.position = "none")
  }
  
  p
}


#' @keywords internal
#' @noRd
.plot_median_cluster_pca_data <- function(data, cluster, vars) {
  # Perform PCA on the specified variables
  pca_mat <- data[, vars, drop = FALSE]
  
  # Remove rows with missing values
  complete_idx <- stats::complete.cases(pca_mat)
  pca_mat_clean <- pca_mat[complete_idx, ]
  
  if (nrow(pca_mat_clean) == 0) {
    stop("No complete cases after removing missing values.")
  }
  
  # Run PCA
  pca_result <- stats::prcomp(pca_mat_clean, scale. = TRUE)
  
  # Extract first two PCs
  pc_scores <- pca_result$x[, 1:2, drop = FALSE]
  
  # Create result dataframe
  result <- tibble::tibble(
    PC1 = pc_scores[, 1],
    PC2 = pc_scores[, 2],
    .cluster = data[[cluster]][complete_idx]
  )
  
  result
}
