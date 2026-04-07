#' @md
#' @title Merge clusters that remain unimodal when combined
#'
#' @description
#' Takes the output of [cluster_merge_bin()] and iteratively merges
#' similarly-labelled clusters as long as the combined population
#' remains unimodal along every variable (assessed via Hartigan's
#' Dip Test).
#'
#' @param data matrix or data.frame.
#'   Rows are observations, columns are variables. Must contain the
#'   same variable columns used in the preceding call to
#'   [cluster_merge_bin()].
#' @param merge_result list.
#'   The object returned by [cluster_merge_bin()].
#'   Must carry a `"thresholds"` attribute (added automatically by
#'   `cluster_merge_bin`).
#' @param max_label_diff integer(1).
#'   Maximum allowed difference between bin indices (Chebyshev
#'   distance) for two clusters to be considered for merging.
#'   Default `2L`.
#' @param ignore_labels character vector or `NULL`.
#'   Merged-cluster labels to exclude from merging consideration.
#'   Default `NULL` (no labels ignored).
#' @param dip_threshold numeric(1).
#'   Minimum dip-test p-value required for every variable before a
#'   merge is accepted.
#'   A higher value is more conservative (demands stronger evidence
#'   of unimodality).  Default `0.15`.
#' @param min_mode_dist numeric or named numeric vector or `NULL`.
#'   If supplied, a candidate merge is rejected when the absolute
#'   difference between the two clusters' per-variable modes is
#'   below this distance **for every variable**.
#'   A named vector specifies per-variable distances; a scalar
#'   applies to all variables.  Default `NULL` (no mode-distance
#'   filter).
#' @param max_iterations integer(1).
#'   Safety cap on the number of merge-relabel-repeat cycles.
#'   Default `50L`.
#'
#' @return A named list with two elements and a `"thresholds"`
#' attribute (carried over from `merge_result`):
#'
#' - `assign`: a [tibble::tibble()] with one row per observation and
#'   columns `orig` (original cluster label from the raw data) and
#'   `merged` (final merged label after unimodal merging).
#' - `label`: a [tibble::tibble()] with one row per unique final
#'   merged cluster and columns `merged` (final merged label),
#'   `level` (bin-combination label) and `descriptive`
#'   (human-readable bin-combination description).
#'
#' @export
#'
#' @examples
#' set.seed(42)
#' n <- 40
#' mat <- matrix(
#'   c(rnorm(n, 0, 1), rnorm(n, 0.3, 1)),
#'   ncol = 2,
#'   dimnames = list(NULL, c("v1", "v2"))
#' )
#' cl <- rep(c("A", "B", "C", "D"), each = n / 4)
#' bin_res <- cluster_merge_bin(mat, cl, list(v1 = 0, v2 = 0))
#' cluster_merge_unimodal(mat, bin_res)
cluster_merge_unimodal <- function(data,
                                   merge_result,
                                   max_label_diff = 2L,
                                   ignore_labels = NULL,
                                   dip_threshold = 0.15,
                                   min_mode_dist = NULL,
                                   max_iterations = 50L) {
  # ---- input validation ----
  thresholds <- attr(merge_result, "thresholds")
  if (is.null(thresholds)) {
    stop(
      paste0(
        "`merge_result` must carry a \"thresholds\" attribute. ",
        "Use the output of `cluster_merge_bin()` (version >= 0.9.0)."
      ),
      call. = FALSE
    )
  }
  vars <- names(thresholds)
  if (!all(vars %in% colnames(data))) {
    stop(
      "`data` must contain all variable columns referenced by `thresholds`.",
      call. = FALSE
    )
  }

  # current cluster assignment (one per observation)
  current_cluster <- merge_result$assign$merged
  orig_cluster    <- merge_result$assign$orig

  # resolve min_mode_dist to a named vector

  if (!is.null(min_mode_dist)) {
    if (is.null(names(min_mode_dist))) {
      min_mode_dist <- stats::setNames(
        rep(min_mode_dist, length(vars)), vars
      )
    }
  }

  # track which labels were newly merged in the previous iteration
  newly_merged <- NULL

  for (iter in seq_len(max_iterations)) {
    cluster_labels <- unique(current_cluster)

    # ---- 1. eligible pairs ----
    pairs <- .cmu_eligible_pairs(
      cluster_labels, ignore_labels, max_label_diff, newly_merged
    )
    if (nrow(pairs) == 0L) break

    # ---- 2. assess candidates ----
    candidates <- .cmu_assess_candidates(
      pairs, data, current_cluster, vars,
      dip_threshold, min_mode_dist
    )
    if (nrow(candidates) == 0L) break

    # ---- 3. merge (mutual-exclusive first, then scored) ----
    merge_map <- .cmu_decide_merges(
      candidates, data, current_cluster, vars
    )
    if (length(merge_map) == 0L) break

    # ---- 4. apply merges & relabel ----
    relabel <- .cmu_apply_merges(
      merge_map, data, current_cluster, thresholds, vars
    )
    current_cluster <- relabel$cluster
    newly_merged    <- relabel$newly_merged
  }

  # ---- build return value ----
  unique_merged <- unique(current_cluster)
  label_tbl <- .cmu_build_label_tbl(
    data, current_cluster, unique_merged, thresholds, vars
  )

  assign_tbl <- tibble::tibble(
    orig   = orig_cluster,
    merged = current_cluster
  )

  result <- list(assign = assign_tbl, label = label_tbl)
  attr(result, "thresholds") <- thresholds
  result
}

# --------------------------------------------------------------------------
# Internal helpers
# --------------------------------------------------------------------------

#' Parse a bin label (e.g. "2_1") into an integer vector.
#' @keywords internal
#' @noRd
.cmu_parse_label <- function(label) {
  as.integer(strsplit(label, "_", fixed = TRUE)[[1L]])
}

#' Chebyshev distance between two bin labels.
#' @keywords internal
#' @noRd
.cmu_label_dist <- function(a, b) {
  max(abs(.cmu_parse_label(a) - .cmu_parse_label(b)))
}

#' Find eligible pairs of clusters to consider for merging.
#'
#' On the first iteration (`newly_merged` is NULL) all label pairs
#' within `max_label_diff` are returned.
#' On later iterations only pairs involving at least one
#' `newly_merged` label are returned.
#' @keywords internal
#' @noRd
.cmu_eligible_pairs <- function(labels, ignore_labels, max_label_diff,
                                newly_merged) {
  labels <- setdiff(labels, ignore_labels)
  n <- length(labels)
  if (n < 2L) {
    return(data.frame(a = character(0), b = character(0),
                      stringsAsFactors = FALSE))
  }

  pairs <- utils::combn(labels, 2L, simplify = FALSE)
  keep <- vapply(pairs, function(p) {
    dist_ok <- .cmu_label_dist(p[1L], p[2L]) <= max_label_diff
    iter_ok <- is.null(newly_merged) ||
      p[1L] %in% newly_merged ||
      p[2L] %in% newly_merged
    dist_ok && iter_ok
  }, logical(1L))

  pairs <- pairs[keep]
  if (length(pairs) == 0L) {
    return(data.frame(a = character(0), b = character(0),
                      stringsAsFactors = FALSE))
  }
  data.frame(
    a = vapply(pairs, `[`, character(1L), 1L),
    b = vapply(pairs, `[`, character(1L), 2L),
    stringsAsFactors = FALSE
  )
}

#' Assess candidate pairs for unimodality via Hartigan's Dip Test.
#' @keywords internal
#' @noRd
.cmu_assess_candidates <- function(pairs, data, cluster, vars,
                                   dip_threshold, min_mode_dist) {
  keep <- vapply(seq_len(nrow(pairs)), function(i) {
    a <- pairs$a[i]
    b <- pairs$b[i]
    idx <- cluster %in% c(a, b)

    # dip test per variable: p-value must be >= dip_threshold for ALL vars
    all_unimodal <- all(vapply(vars, function(v) {
      vals <- as.numeric(data[idx, v])
      if (length(unique(vals)) < 4L) return(TRUE)
      diptest::dip.test(vals)$p.value >= dip_threshold
    }, logical(1L)))
    if (!all_unimodal) return(FALSE)

    # mode distance filter (optional)
    if (!is.null(min_mode_dist)) {
      idx_a <- cluster == a
      idx_b <- cluster == b
      all_close <- all(vapply(vars, function(v) {
        mode_a <- .cmu_estimate_mode(as.numeric(data[idx_a, v]))
        mode_b <- .cmu_estimate_mode(as.numeric(data[idx_b, v]))
        abs(mode_a - mode_b) < min_mode_dist[v]
      }, logical(1L)))
      if (all_close) return(FALSE)
    }

    TRUE
  }, logical(1L))

  pairs[keep, , drop = FALSE]
}

#' Estimate the mode of a numeric vector via kernel density.
#' @keywords internal
#' @noRd
.cmu_estimate_mode <- function(x) {
  if (length(x) < 2L) return(x[1L])
  d <- stats::density(x)
  d$x[which.max(d$y)]
}

#' Decide which candidate pairs to merge.
#'
#' Mutually-exclusive pairs (each cluster only appears in one
#' candidate) are merged immediately.
#' For the remaining ambiguous candidates the SCAMP preference score
#' is used: pairs are ranked by decreasing score and greedily
#' merged.
#'
#' @return Named character vector where `names` are the cluster that
#'   will be absorbed and `values` are the cluster that absorbs it.
#' @keywords internal
#' @noRd
.cmu_decide_merges <- function(candidates, data, cluster, vars) {
  if (nrow(candidates) == 0L) return(character(0))

  # count how many candidate pairs each label appears in
  all_labs <- c(candidates$a, candidates$b)
  lab_count <- table(all_labs)

  # mutually exclusive: both members appear exactly once
  mut_excl <- vapply(seq_len(nrow(candidates)), function(i) {
    lab_count[candidates$a[i]] == 1L && lab_count[candidates$b[i]] == 1L
  }, logical(1L))

  merge_map <- character(0)
  merged_labels <- character(0)

  # merge mutually exclusive pairs
  if (any(mut_excl)) {
    for (i in which(mut_excl)) {
      a <- candidates$a[i]
      b <- candidates$b[i]
      if (a %in% merged_labels || b %in% merged_labels) next
      merge_map[b] <- a
      merged_labels <- c(merged_labels, a, b)
    }
  }

  # score remaining ambiguous pairs
  ambig <- candidates[!mut_excl, , drop = FALSE]
  if (nrow(ambig) > 0L) {
    scores <- .cmu_score_candidates(ambig, data, cluster, vars)
    ord <- order(scores, decreasing = TRUE)
    for (i in ord) {
      a <- ambig$a[i]
      b <- ambig$b[i]
      if (a %in% merged_labels || b %in% merged_labels) next
      merge_map[b] <- a
      merged_labels <- c(merged_labels, a, b)
    }
  }

  merge_map
}

#' SCAMP preference score for candidate merged clusters.
#'
#' For each candidate pair the score is:
#' \deqn{P(X_r) = \delta_r + \sigma_{nrm} + \sigma_{nrs}/2 +
#'   \tau_{nrm} + \tau_{nrs}/2 + \phi_{nrm} + \phi_{nrs}/2}
#' where:
#' \itemize{
#'   \item \eqn{\delta_r} = minimum dip-test p-value across dims
#'   \item \eqn{\sigma} = second L-moment (L-scale)
#'   \item \eqn{\tau} = L-skewness ratio
#'   \item \eqn{\phi} = L-kurtosis ratio
#'   \item \eqn{nrm} = normalised against max across candidates
#'   \item \eqn{nrs} = normalised against sum across dims
#' }
#'
#' L-moment terms are inverted (1 − normalised value) so that
#' tighter, more symmetric, lighter-tailed merges receive higher
#' scores.
#' @keywords internal
#' @noRd
.cmu_score_candidates <- function(candidates, data, cluster, vars) {
  n_cand <- nrow(candidates)
  n_vars <- length(vars)

  # pre-compute per-candidate, per-variable statistics
  delta   <- numeric(n_cand)
  sigma_m <- matrix(0, nrow = n_cand, ncol = n_vars)
  tau_m   <- matrix(0, nrow = n_cand, ncol = n_vars)
  phi_m   <- matrix(0, nrow = n_cand, ncol = n_vars)

  for (i in seq_len(n_cand)) {
    idx <- cluster %in% c(candidates$a[i], candidates$b[i])
    dip_pvals <- numeric(n_vars)
    for (j in seq_along(vars)) {
      vals <- as.numeric(data[idx, vars[j]])
      if (length(unique(vals)) < 4L) {
        dip_pvals[j] <- 1
        sigma_m[i, j] <- 0
        tau_m[i, j]   <- 0
        phi_m[i, j]   <- 0
        next
      }
      dip_pvals[j]  <- diptest::dip.test(vals)$p.value
      lm <- .cmu_lmoments(vals)
      sigma_m[i, j] <- lm$l2
      tau_m[i, j]   <- abs(lm$t3)
      phi_m[i, j]   <- abs(lm$t4)
    }
    delta[i] <- min(dip_pvals)
  }

  # aggregate across dimensions (mean per candidate)
  sigma_agg <- rowMeans(sigma_m)
  tau_agg   <- rowMeans(tau_m)
  phi_agg   <- rowMeans(phi_m)

  # nrm: normalise against max across candidates (inverted)
  safe_inv_nrm <- function(x) {
    mx <- max(x)
    if (mx == 0) return(rep(1, length(x)))
    1 - x / mx
  }
  sigma_nrm <- safe_inv_nrm(sigma_agg)
  tau_nrm   <- safe_inv_nrm(tau_agg)
  phi_nrm   <- safe_inv_nrm(phi_agg)

  # nrs: for each candidate, normalise per-dim values against
  #       sum across dims, then take max (inverted)
  safe_inv_nrs <- function(mat) {
    vapply(seq_len(nrow(mat)), function(i) {
      s <- sum(mat[i, ])
      if (s == 0) return(1)
      1 - max(mat[i, ] / s)
    }, numeric(1L))
  }
  sigma_nrs <- safe_inv_nrs(sigma_m)
  tau_nrs   <- safe_inv_nrs(tau_m)
  phi_nrs   <- safe_inv_nrs(phi_m)

  delta +
    sigma_nrm + sigma_nrs / 2 +
    tau_nrm   + tau_nrs   / 2 +
    phi_nrm   + phi_nrs   / 2
}

#' Compute sample L-moments (first four) for a numeric vector.
#' @return Named list with elements `l1`, `l2`, `t3`, `t4`.
#' @keywords internal
#' @noRd
.cmu_lmoments <- function(x) {
  x <- sort(x)
  n <- length(x)
  if (n < 4L) {
    return(list(l1 = mean(x), l2 = 0, t3 = 0, t4 = 0))
  }

  # probability-weighted moments
  b <- numeric(4L)
  for (j in seq_len(n)) {
    comb_vals <- numeric(4L)
    for (r in 0:3) {
      if (j - 1L >= r) {
        comb_vals[r + 1L] <- choose(j - 1L, r) / choose(n - 1L, r)
      }
    }
    b <- b + x[j] * comb_vals
  }
  b <- b / n

  l1 <- b[1L]
  l2 <- 2 * b[2L] - b[1L]
  l3 <- 6 * b[3L] - 6 * b[2L] + b[1L]
  l4 <- 20 * b[4L] - 30 * b[3L] + 12 * b[2L] - b[1L]

  t3 <- if (l2 != 0) l3 / l2 else 0
  t4 <- if (l2 != 0) l4 / l2 else 0

  list(l1 = l1, l2 = l2, t3 = t3, t4 = t4)
}

#' Apply merge map and relabel merged clusters.
#' @return List with `cluster` (updated assignment vector) and
#'   `newly_merged` (character vector of new labels).
#' @keywords internal
#' @noRd
.cmu_apply_merges <- function(merge_map, data, cluster,
                              thresholds, vars) {
  # apply absorptions: replace absorbed label with absorbing label
  for (absorbed in names(merge_map)) {
    cluster[cluster == absorbed] <- merge_map[[absorbed]]
  }

  # relabel all clusters that absorbed something
  absorbers <- unique(unname(merge_map))
  newly_merged <- character(0)
  for (ab in absorbers) {
    new_label <- .cmu_compute_label(data, cluster == ab, thresholds, vars)
    if (new_label != ab) {
      cluster[cluster == ab] <- new_label
    }
    newly_merged <- c(newly_merged, new_label)
  }

  list(cluster = cluster, newly_merged = unique(newly_merged))
}

#' Compute a bin label for a set of observations.
#' @keywords internal
#' @noRd
.cmu_compute_label <- function(data, mask, thresholds, vars) {
  bins <- vapply(vars, function(v) {
    med <- stats::median(as.numeric(data[mask, v]))
    thrs <- sort(thresholds[[v]])
    sum(med > thrs) + 1L
  }, integer(1L))
  paste(bins, collapse = "_")
}

#' Build the label tibble for the final output.
#' @keywords internal
#' @noRd
.cmu_build_label_tbl <- function(data, cluster, unique_merged,
                                 thresholds, vars) {
  purrr::map_df(unique_merged, function(cl) {
    mask <- cluster == cl
    bins <- vapply(vars, function(v) {
      med <- stats::median(as.numeric(data[mask, v]))
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

    tibble::tibble(merged = cl, level = level, descriptive = descriptive)
  })
}
