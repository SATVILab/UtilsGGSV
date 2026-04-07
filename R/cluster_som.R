#' @md
#' @title Self-Organizing Map (SOM) clustering
#'
#' @description
#' Trains a Self-Organizing Map on the input data using
#' [kohonen::som()] and returns the mapping-unit (node) assignment
#' for every observation as a cluster label.
#'
#' @param data matrix or data.frame.
#'   Rows are observations, columns are variables.
#' @param vars character vector or `NULL`.
#'   Column names to use for clustering.
#'   If `NULL` (default), all columns are used.
#' @param x_dim integer(1).
#'   Number of columns in the SOM grid. Default `5L`.
#' @param y_dim integer(1).
#'   Number of rows in the SOM grid. Default `5L`.
#' @param rlen integer(1).
#'   Number of training iterations. Default `100L`.
#' @param topo character(1).
#'   Grid topology passed to [kohonen::somgrid()].
#'   One of `"rectangular"` (default) or `"hexagonal"`.
#'
#' @return A character vector of cluster labels, one per row of `data`.
#'   Labels are of the form `"1"`, `"2"`, etc., corresponding to
#'   SOM grid nodes.
#'
#' @details
#' The function:
#' 1. Scales the selected columns to zero mean and unit variance.
#' 2. Creates a SOM grid of size `x_dim` × `y_dim`.
#' 3. Trains a SOM on the scaled data.
#' 4. Returns the unit assignment for each observation.
#'
#' The `kohonen` package must be installed.
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
#' cl <- cluster_som(mat, x_dim = 3, y_dim = 3)
#' table(cl)
cluster_som <- function(data,
                        vars = NULL,
                        x_dim = 5L,
                        y_dim = 5L,
                        rlen = 100L,
                        topo = "rectangular") {
  if (!requireNamespace("kohonen", quietly = TRUE)) {
    stop(
      'Package "kohonen" must be installed to use `cluster_som()`.',
      call. = FALSE
    )
  }
  .cluster_som_validate(data, vars, x_dim, y_dim, rlen, topo)

  if (is.null(vars)) {
    vars <- colnames(data)
  }

  mat <- as.matrix(data[, vars, drop = FALSE])
  mat <- scale(mat)

  grid <- kohonen::somgrid(xdim = x_dim, ydim = y_dim, topo = topo)
  som_model <- kohonen::som(mat, grid = grid, rlen = rlen)
  as.character(som_model$unit.classif)
}

.cluster_som_validate <- function(data, vars, x_dim, y_dim, rlen, topo) {
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
  if (!is.numeric(x_dim) || length(x_dim) != 1L || x_dim < 1L) {
    stop("`x_dim` must be a positive integer.", call. = FALSE)
  }
  if (!is.numeric(y_dim) || length(y_dim) != 1L || y_dim < 1L) {
    stop("`y_dim` must be a positive integer.", call. = FALSE)
  }
  if (!is.numeric(rlen) || length(rlen) != 1L || rlen < 1L) {
    stop("`rlen` must be a positive integer.", call. = FALSE)
  }
  topo <- match.arg(topo, c("rectangular", "hexagonal"))
  invisible(NULL)
}

#' @md
#' @title SOM clustering with bin and unimodal merging
#'
#' @description
#' End-to-end wrapper that applies SOM clustering
#' ([cluster_som()]), then merges clusters by variable bin
#' thresholds ([cluster_merge_bin()]), and finally merges unimodal
#' neighbours ([cluster_merge_unimodal()]).
#'
#' @inheritParams cluster_som
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
#' result <- cluster_som_merge(
#'   mat,
#'   vars = c("v1", "v2"),
#'   thresholds = list(v1 = 0, v2 = 0),
#'   x_dim = 3, y_dim = 3
#' )
#' table(result$assign$merged)
cluster_som_merge <- function(data,
                              vars = NULL,
                              thresholds,
                              x_dim = 5L,
                              y_dim = 5L,
                              rlen = 100L,
                              topo = "rectangular",
                              max_label_diff = 2L,
                              ignore_labels = NULL,
                              dip_threshold = 0.15,
                              min_mode_dist = NULL,
                              max_iterations = 50L) {
  cl <- cluster_som(
    data, vars = vars,
    x_dim = x_dim, y_dim = y_dim, rlen = rlen, topo = topo
  )
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
