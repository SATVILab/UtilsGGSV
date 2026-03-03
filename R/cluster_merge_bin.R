#' @md
#' @title Merge clusters based on variable bin thresholds
#'
#' @description
#' Assigns each cluster to a bin for each variable based on where the
#' cluster's median falls relative to the provided thresholds. Clusters
#' that fall in the same combination of bins across all variables are
#' merged.
#'
#' @param data matrix or data.frame. Rows are observations, columns are
#'   variables. Must contain columns whose names match those in
#'   `thresholds`.
#' @param cluster character vector. Cluster assignment for each
#'   observation. Length must equal `nrow(data)`.
#' @param thresholds named list. Each element is a numeric vector of
#'   threshold values for the variable given by the element name. The
#'   thresholds define bin boundaries: a single threshold `t` creates
#'   two bins (`<= t` and `> t`), two thresholds create three bins, and
#'   so on.
#'
#' @return A named list with two elements:
#'
#' - `assign`: a [tibble::tibble()] with one row per observation and
#'   columns `orig` (original cluster label) and `merged` (merged
#'   cluster label).
#' - `label`: a [tibble::tibble()] with one row per unique original
#'   cluster and columns `orig` (original cluster label), `level`
#'   (concise bin-combination label formed by joining per-variable bin
#'   indices with `"_"`) and `descriptive` (human-readable
#'   bin-combination description formed by joining per-variable range
#'   descriptions with `"; "`).
#'
#' @export
#'
#' @examples
#' set.seed(1)
#' mat <- matrix(
#'   c(rnorm(20, 2), rnorm(20, -2)),
#'   ncol = 2,
#'   dimnames = list(NULL, c("var1", "var2"))
#' )
#' cl <- rep(c("A", "B", "C", "D"), each = 5)
#' thresholds <- list(var1 = 0, var2 = 0)
#' cluster_merge_bin(mat, cl, thresholds)
cluster_merge_bin <- function(data, cluster, thresholds) {
  vars <- names(thresholds)
  cluster_uniq <- unique(cluster)

  label_tbl <- .cluster_merge_bin_label(
    data, cluster, cluster_uniq, thresholds, vars
  )

  assign_tbl <- tibble::tibble(
    orig = cluster,
    merged = label_tbl$level[match(cluster, label_tbl$orig)]
  )

  list(assign = assign_tbl, label = label_tbl)
}

.cluster_merge_bin_label <- function(data,
                                     cluster,
                                     cluster_uniq,
                                     thresholds,
                                     vars) {
  purrr::map_df(cluster_uniq, function(cl) {
    obs <- cluster == cl
    bins <- vapply(vars, function(v) {
      med <- stats::median(as.numeric(data[obs, v]))
      thrs <- sort(thresholds[[v]])
      sum(med > thrs) + 1L
    }, integer(1L))

    level <- paste(bins, collapse = "_")

    desc_parts <- vapply(seq_along(vars), function(i) {
      .cluster_merge_bin_desc_one(
        vars[i], bins[i], sort(thresholds[[vars[i]]])
      )
    }, character(1L))
    descriptive <- paste(desc_parts, collapse = "; ")

    tibble::tibble(orig = cl, level = level, descriptive = descriptive)
  })
}

.cluster_merge_bin_desc_one <- function(var, bin, thresholds) {
  n_thrs <- length(thresholds)
  if (bin == 1L) {
    paste0(var, " <= ", thresholds[1L])
  } else if (bin == n_thrs + 1L) {
    paste0(var, " > ", thresholds[n_thrs])
  } else {
    paste0(thresholds[bin - 1L], " < ", var, " <= ", thresholds[bin])
  }
}
