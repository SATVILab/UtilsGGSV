#' @md
#' @title Plot minimum-spanning tree of clusters
#'
#' @description
#' Computes the minimum-spanning tree (MST) over clusters, where the distance
#' between two clusters is the Euclidean distance between their median variable
#' profiles. The clusters are positioned in two dimensions using classical
#' multidimensional scaling (MDS) of the distance matrix. MST edges are drawn
#' as line segments and each cluster is shown as a labelled point.
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
#' plot_cluster_mst(data, cluster = "cluster")
plot_cluster_mst <- function(data,
                              cluster,
                              vars = NULL,
                              col_clusters = NULL,
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

  if (length(cluster_vec) < 2L) {
    stop("At least two clusters are required to compute an MST.")
  }

  med_mat <- .plot_cluster_mst_medians(data, cluster, vars, cluster_vec)
  dist_mat <- as.matrix(stats::dist(med_mat))
  node_tbl <- .plot_cluster_mst_layout(dist_mat, cluster_vec)
  edge_tbl <- .plot_cluster_mst_edges(dist_mat, node_tbl)

  .plot_cluster_mst_plot(
    node_tbl = node_tbl,
    edge_tbl = edge_tbl,
    col_clusters = col_clusters,
    thm = thm,
    grid = grid
  )
}

.plot_cluster_mst_medians <- function(data, cluster, vars, cluster_vec) {
  med_mat <- do.call(rbind, lapply(cluster_vec, function(cl) {
    cl_mask <- data[[cluster]] == cl
    vapply(
      vars,
      function(v) stats::median(data[[v]][cl_mask], na.rm = TRUE),
      numeric(1L)
    )
  }))
  rownames(med_mat) <- cluster_vec
  colnames(med_mat) <- vars
  med_mat
}

.plot_cluster_mst_layout <- function(dist_mat, cluster_vec) {
  k <- min(2L, length(cluster_vec) - 1L)
  coords <- stats::cmdscale(stats::as.dist(dist_mat), k = k)
  if (!is.matrix(coords)) {
    coords <- matrix(coords, ncol = 1L)
  }
  tibble::tibble(
    cluster = cluster_vec,
    x = coords[, 1L],
    y = if (ncol(coords) >= 2L) coords[, 2L] else rep(0, length(cluster_vec))
  )
}

.plot_cluster_mst_kruskal <- function(dist_mat) {
  cluster_vec <- rownames(dist_mat)
  n <- length(cluster_vec)

  from_vec <- character(0L)
  to_vec <- character(0L)
  weight_vec <- numeric(0L)
  for (i in seq_len(n - 1L)) {
    for (j in seq(i + 1L, n)) {
      from_vec <- c(from_vec, cluster_vec[i])
      to_vec <- c(to_vec, cluster_vec[j])
      weight_vec <- c(weight_vec, dist_mat[i, j])
    }
  }
  ord <- order(weight_vec)
  from_sorted <- from_vec[ord]
  to_sorted <- to_vec[ord]

  parent <- seq_len(n)
  names(parent) <- cluster_vec

  find_root <- function(i) {
    while (parent[i] != i) i <- parent[i]
    i
  }

  mst_from <- character(0L)
  mst_to <- character(0L)

  for (k in seq_along(from_sorted)) {
    ri <- find_root(match(from_sorted[k], cluster_vec))
    rj <- find_root(match(to_sorted[k], cluster_vec))
    if (ri != rj) {
      parent[ri] <- rj
      mst_from <- c(mst_from, from_sorted[k])
      mst_to <- c(mst_to, to_sorted[k])
    }
    if (length(mst_from) == n - 1L) break
  }

  tibble::tibble(from = mst_from, to = mst_to)
}

.plot_cluster_mst_edges <- function(dist_mat, node_tbl) {
  mst_tbl <- .plot_cluster_mst_kruskal(dist_mat)
  tibble::tibble(
    x    = node_tbl$x[match(mst_tbl$from, node_tbl$cluster)],
    xend = node_tbl$x[match(mst_tbl$to,   node_tbl$cluster)],
    y    = node_tbl$y[match(mst_tbl$from, node_tbl$cluster)],
    yend = node_tbl$y[match(mst_tbl$to,   node_tbl$cluster)]
  )
}

.plot_cluster_mst_plot <- function(node_tbl, edge_tbl, col_clusters, thm, grid) {
  p <- ggplot2::ggplot() +
    ggplot2::geom_segment(
      data = edge_tbl,
      ggplot2::aes(
        x = .data$x, y = .data$y,
        xend = .data$xend, yend = .data$yend
      )
    ) +
    ggplot2::geom_point(
      data = node_tbl,
      ggplot2::aes(x = .data$x, y = .data$y, colour = .data$cluster),
      size = 4
    ) +
    ggplot2::geom_text(
      data = node_tbl,
      ggplot2::aes(
        x = .data$x, y = .data$y,
        label = .data$cluster, colour = .data$cluster
      ),
      vjust = -1
    ) +
    ggplot2::labs(x = "MDS 1", y = "MDS 2", colour = "Cluster")

  if (!is.null(col_clusters)) {
    p <- p + ggplot2::scale_colour_manual(values = col_clusters)
  }

  if (!is.null(thm)) p <- p + thm
  if (!is.null(grid)) p <- p + grid

  p
}
