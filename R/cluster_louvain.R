#' @md
#' @title Louvain community-detection clustering
#'
#' @description
#' Builds a shared nearest-neighbour graph from the input data and applies
#' the Louvain algorithm ([igraph::cluster_louvain()]) to detect
#' communities.
#'
#' @param data matrix or data.frame.
#'   Rows are observations, columns are variables.
#' @param vars character vector or `NULL`.
#'   Column names to use for clustering.
#'   If `NULL` (default), all columns are used.
#' @param k integer(1).
#'   Number of nearest neighbours used to build the kNN graph.
#'   Default `15L`.
#' @param resolution numeric(1).
#'   Resolution parameter passed to [igraph::cluster_louvain()].
#'   Higher values yield more (smaller) clusters.
#'   Default `1`.
#'
#' @return A character vector of cluster labels, one per row of `data`.
#'   Labels are of the form `"1"`, `"2"`, etc.
#'
#' @details
#' The function:
#' 1. Scales the selected columns to zero mean and unit variance.
#' 2. Finds the `k` nearest neighbours for each observation using
#'    Euclidean distance (via [FNN::get.knn()]).
#' 3. Constructs a shared-nearest-neighbour (SNN) graph where the
#'    edge weight between two observations equals the number of
#'    shared neighbours.
#' 4. Applies Louvain community detection on the SNN graph.
#'
#' @export
#'
#' @examples
#' set.seed(1)
#' mat <- matrix(
#'   c(rnorm(50, -3), rnorm(50, 3), rnorm(100, 0)),
#'   ncol = 2,
#'   dimnames = list(NULL, c("v1", "v2"))
#' )
#' cl <- cluster_louvain(mat, k = 10)
#' table(cl)
cluster_louvain <- function(data, vars = NULL, k = 15L, resolution = 1) {
  .cluster_louvain_validate(data, vars, k, resolution)

  if (is.null(vars)) {
    vars <- colnames(data)
  }

  mat <- as.matrix(data[, vars, drop = FALSE])
  mat <- scale(mat)

  knn <- FNN::get.knn(mat, k = min(k, nrow(mat) - 1L))
  g <- .cluster_louvain_snn_graph(knn)
  membership <- igraph::cluster_louvain(g, resolution = resolution)$membership
  as.character(membership)
}

.cluster_louvain_validate <- function(data, vars, k, resolution) {
  if (!is.matrix(data) && !is.data.frame(data)) {
    stop("`data` must be a matrix or data.frame.", call. = FALSE)
  }
  if (nrow(data) < 2L) {
    stop("`data` must have at least 2 rows.", call. = FALSE)
  }
  if (!is.null(vars)) {
    if (!all(vars %in% colnames(data))) {
      stop("All `vars` must be column names of `data`.", call. = FALSE)
    }
  }
  if (!is.numeric(k) || length(k) != 1L || k < 1L) {
    stop("`k` must be a positive integer.", call. = FALSE)
  }
  if (!is.numeric(resolution) || length(resolution) != 1L || resolution <= 0) {
    stop("`resolution` must be a positive number.", call. = FALSE)
  }
  invisible(NULL)
}

#' Build a shared-nearest-neighbour graph from a kNN result.
#' @param knn List returned by [FNN::get.knn()] with element
#'   `nn.index` (integer matrix, n x k).
#' @return An [igraph::graph] object with SNN edge weights.
#' @keywords internal
#' @noRd
.cluster_louvain_snn_graph <- function(knn) {
  nn_idx <- knn$nn.index
  n <- nrow(nn_idx)
  k <- ncol(nn_idx)

  # Build neighbour sets (as list of integer vectors, including self)
  nb_sets <- lapply(seq_len(n), function(i) {
    c(i, as.integer(nn_idx[i, ]))
  })

  # Collect SNN edges
  edges_from <- integer(0)
  edges_to <- integer(0)
  weights <- numeric(0)

  for (i in seq_len(n)) {
    for (j in seq_len(k)) {
      neighbour <- nn_idx[i, j]
      if (neighbour <= i) next
      shared <- length(intersect(nb_sets[[i]], nb_sets[[neighbour]]))
      if (shared > 0L) {
        edges_from <- c(edges_from, i)
        edges_to <- c(edges_to, neighbour)
        weights <- c(weights, shared)
      }
    }
  }

  g <- igraph::make_empty_graph(n = n, directed = FALSE)
  if (length(edges_from) > 0L) {
    edge_list <- as.vector(rbind(edges_from, edges_to))
    g <- igraph::add_edges(g, edge_list, weight = weights)
  }
  g
}

#' @md
#' @title Louvain clustering with bin and unimodal merging
#'
#' @description
#' End-to-end wrapper that applies Louvain clustering
#' ([cluster_louvain()]), then merges clusters by variable bin
#' thresholds ([cluster_merge_bin()]), and finally merges unimodal
#' neighbours ([cluster_merge_unimodal()]).
#'
#' @inheritParams cluster_louvain
#' @param thresholds named list.
#'   Passed to [cluster_merge_bin()]. Each element is a numeric vector
#'   of threshold values for the variable given by the element name.
#' @param max_label_diff integer(1).
#'   Passed to [cluster_merge_unimodal()]. Default `2L`.
#' @param ignore_labels character vector or `NULL`.
#'   Passed to [cluster_merge_unimodal()]. Default `NULL`.
#' @param dip_threshold numeric(1).
#'   Passed to [cluster_merge_unimodal()]. Default `0.15`.
#' @param min_mode_dist numeric or named numeric vector or `NULL`.
#'   Passed to [cluster_merge_unimodal()]. Default `NULL`.
#' @param max_iterations integer(1).
#'   Passed to [cluster_merge_unimodal()]. Default `50L`.
#'
#' @return The list returned by [cluster_merge_unimodal()], with
#'   elements `assign` and `label` and a `"thresholds"` attribute.
#'
#' @export
#'
#' @examples
#' set.seed(1)
#' mat <- matrix(
#'   c(rnorm(50, -3), rnorm(50, 3), rnorm(100, 0)),
#'   ncol = 2,
#'   dimnames = list(NULL, c("v1", "v2"))
#' )
#' result <- cluster_louvain_merge(
#'   mat,
#'   vars = c("v1", "v2"),
#'   thresholds = list(v1 = 0, v2 = 0),
#'   k = 10
#' )
#' table(result$assign$merged)
cluster_louvain_merge <- function(data,
                                  vars = NULL,
                                  thresholds,
                                  k = 15L,
                                  resolution = 1,
                                  max_label_diff = 2L,
                                  ignore_labels = NULL,
                                  dip_threshold = 0.15,
                                  min_mode_dist = NULL,
                                  max_iterations = 50L) {
  cl <- cluster_louvain(data, vars = vars, k = k, resolution = resolution)
  bin_res <- cluster_merge_bin(data, cl, thresholds)
  cluster_merge_unimodal(
    data, bin_res,
    max_label_diff = max_label_diff,
    ignore_labels = ignore_labels,
    dip_threshold = dip_threshold,
    min_mode_dist = min_mode_dist,
    max_iterations = max_iterations
  )
}
