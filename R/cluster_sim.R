#' Simulate High-Dimensional Single-Cell Clusters (FAUST Method)
#'
#' Generates synthetic high-dimensional single-cell data replicating the
#' simulation studies from the FAUST paper (Greene et al., 2021, *Patterns*).
#' Supports configurable numbers of dimensions, samples, and clusters, along
#' with differential abundance spike-ins for benchmarking.
#'
#' @param n_samples Integer. Number of total samples to simulate.
#' @param n_clusters Integer. Number of mixture components.
#' @param n_dims Integer. Number of markers/dimensions.
#' @param n_cells_per_sample Integer. Total observations per sample.
#' @param base_cluster_weights Numeric vector. Custom base weights. If `NULL`,
#'   uses the FAUST reference vector with uniform residual distribution.
#' @param expr_modes Numeric vector. The values used to construct base mean
#'   vectors.
#' @param var_bounds Numeric vector of length 2. Min and max constraints for
#'   variance in covariance matrices.
#' @param mean_perturb_sd Numeric. Standard deviation for the Gaussian used to
#'   perturb means per sample.
#' @param transform Character. Options: `"none"` (MVN) or `"faust_gamma"` for
#'   `Gamma(1 + |x/4|)`.
#' @param spike_clusters Integer vector. Indices of the clusters to spike in
#'   abundance.
#' @param spike_samples Integer vector. Indices of the samples to apply the
#'   spike to (e.g., "responders").
#' @param spike_fold_change Numeric. The multiplier for the spiked cluster
#'   weights.
#' @param noise_dims Integer. Number of nuisance noise columns to append,
#'   sampled from a standard normal distribution. If `NULL` (default), no noise
#'   columns are added.
#' @param distribution Character. Distribution used to generate cell data.
#'   `"normal"` (default) uses [MASS::mvrnorm()]; `"t"` uses
#'   [mvtnorm::rmvt()] for heavier tails. Requires the \pkg{mvtnorm} package
#'   when set to `"t"`.
#' @param df Numeric. Degrees of freedom for the multivariate t-distribution.
#'   Only used when `distribution = "t"`. Default is `3`.
#' @param batch_effect_shift Numeric scalar or vector of length `n_dims`. Mean
#'   shift applied to samples indicated by `batch_samples` to simulate a
#'   batch effect. If `NULL` (default), no batch effect is applied.
#' @param batch_samples Integer vector. Indices of samples that receive the
#'   batch effect shift. Must be provided together with `batch_effect_shift`.
#' @param knockout_clusters Integer vector. Indices of clusters whose
#'   probability is set to zero in the samples given by `knockout_samples`.
#'   Must be provided together with `knockout_samples`.
#' @param knockout_samples Integer vector. Indices of samples where
#'   `knockout_clusters` are removed. Must be provided together with
#'   `knockout_clusters`.
#'
#' @return A list containing:
#'
#'   \item{data}{A tibble of the simulated data with `sample_id`, `cell_id`,
#'     `cluster_id`, and one column per dimension (`dim_1`, `dim_2`, ...).
#'     When `noise_dims` is set, additional columns `noise_1`, `noise_2`, ...
#'     are appended.}
#'   \item{metadata}{A list containing true parameters: base means, perturbed
#'     means per sample, covariance matrices, and exact weight vectors.}
#'
#' @export
#'
#' @examples
#' set.seed(42)
#' sim <- cluster_sim(
#'   n_samples = 2, n_clusters = 3, n_dims = 4,
#'   n_cells_per_sample = 100
#' )
#' head(sim$data)
cluster_sim <- function(n_samples = 20L,
                        n_clusters = 125L,
                        n_dims = 10L,
                        n_cells_per_sample = 25000L,
                        base_cluster_weights = NULL,
                        expr_modes = c(0, 8),
                        var_bounds = c(1, 2),
                        mean_perturb_sd = 1 / sqrt(2),
                        transform = c("none", "faust_gamma"),
                        spike_clusters = NULL,
                        spike_samples = NULL,
                        spike_fold_change = 2,
                        noise_dims = NULL,
                        distribution = c("normal", "t"),
                        df = 3,
                        batch_effect_shift = NULL,
                        batch_samples = NULL,
                        knockout_clusters = NULL,
                        knockout_samples = NULL) {
  transform <- match.arg(transform)
  distribution <- match.arg(distribution)
  .cluster_sim_validate(
    n_samples, n_clusters, n_dims, n_cells_per_sample,
    base_cluster_weights, expr_modes, var_bounds, mean_perturb_sd,
    spike_clusters, spike_samples, spike_fold_change,
    noise_dims, distribution, df,
    batch_effect_shift, batch_samples,
    knockout_clusters, knockout_samples
  )

  # 1. Initialize weights
  weights <- .cluster_sim_init_weights(n_clusters, base_cluster_weights)

  # 2. Assign base means randomly from expr_modes
  base_means <- matrix(
    sample(expr_modes, n_clusters * n_dims, replace = TRUE),
    nrow = n_clusters, ncol = n_dims
  )

  # 3-6. Generate data sample-by-sample
  data_list <- vector("list", n_samples)
  meta_perturbed_means <- vector("list", n_samples)
  meta_cov_matrices <- vector("list", n_samples)
  meta_weights <- vector("list", n_samples)
  dim_names <- paste0("dim_", seq_len(n_dims))

  # Resolve batch_effect_shift to a vector of length n_dims
  if (!is.null(batch_effect_shift) && length(batch_effect_shift) == 1L) {
    batch_effect_shift <- rep(batch_effect_shift, n_dims)
  }

  for (s in seq_len(n_samples)) {
    # 3. Determine sample-specific weights (apply spike if applicable)
    sample_weights <- if (!is.null(spike_samples) && s %in% spike_samples) {
      .cluster_sim_spike_weights(weights, spike_clusters, spike_fold_change)
    } else {
      weights
    }

    # Apply knockout: zero out specified clusters for knockout samples
    if (!is.null(knockout_samples) && s %in% knockout_samples) {
      sample_weights <- .cluster_sim_knockout_weights(
        sample_weights, knockout_clusters
      )
    }

    meta_weights[[s]] <- sample_weights

    # 4. Draw cell counts via rmultinom
    cell_counts <- as.integer(
      stats::rmultinom(1, size = n_cells_per_sample, prob = sample_weights)
    )

    # Determine if batch effect applies to this sample
    apply_batch <- !is.null(batch_samples) && s %in% batch_samples

    # 5. Generate per-cluster data
    all_x <- matrix(0, nrow = n_cells_per_sample, ncol = n_dims)
    all_cluster_id <- integer(n_cells_per_sample)
    perturbed_means_s <- matrix(0, nrow = n_clusters, ncol = n_dims)
    cov_matrices_s <- vector("list", n_clusters)
    row_idx <- 1L

    for (k in seq_len(n_clusters)) {
      # Perturb mean
      perturbation <- round(
        stats::rnorm(n_dims, mean = 0, sd = mean_perturb_sd)
      )
      mu_sk <- base_means[k, ] + perturbation

      # Apply batch effect shift to mean
      if (apply_batch) {
        mu_sk <- mu_sk + batch_effect_shift
      }

      perturbed_means_s[k, ] <- mu_sk

      # Generate covariance matrix
      sigma_sk <- .cluster_sim_gen_cov(n_dims, var_bounds)
      cov_matrices_s[[k]] <- sigma_sk

      n_k <- cell_counts[k]
      if (n_k > 0L) {
        x <- if (distribution == "t") {
          mvtnorm::rmvt(n = n_k, sigma = sigma_sk, df = df, delta = mu_sk)
        } else {
          MASS::mvrnorm(n = n_k, mu = mu_sk, Sigma = sigma_sk)
        }
        if (!is.matrix(x)) x <- matrix(x, nrow = 1L)

        # 6. Apply transformation if requested
        if (transform == "faust_gamma") {
          x <- gamma(1 + abs(x / 4))
        }

        end_idx <- row_idx + n_k - 1L
        all_x[row_idx:end_idx, ] <- x
        all_cluster_id[row_idx:end_idx] <- k
        row_idx <- end_idx + 1L
      }
    }

    meta_perturbed_means[[s]] <- perturbed_means_s
    meta_cov_matrices[[s]] <- cov_matrices_s

    dim_df <- as.data.frame(all_x)
    names(dim_df) <- dim_names

    # Append noise dimensions if requested
    if (!is.null(noise_dims) && noise_dims > 0L) {
      noise_mat <- matrix(
        stats::rnorm(n_cells_per_sample * noise_dims),
        nrow = n_cells_per_sample, ncol = noise_dims
      )
      noise_df <- as.data.frame(noise_mat)
      names(noise_df) <- paste0("noise_", seq_len(noise_dims))
      dim_df <- cbind(dim_df, noise_df)
    }

    data_list[[s]] <- cbind(
      data.frame(
        sample_id = rep(s, n_cells_per_sample),
        cell_id = seq_len(n_cells_per_sample),
        cluster_id = all_cluster_id
      ),
      dim_df
    )
  }

  data_tbl <- tibble::as_tibble(do.call(rbind, data_list))

  list(
    data = data_tbl,
    metadata = list(
      base_means = base_means,
      base_weights = weights,
      perturbed_means = meta_perturbed_means,
      cov_matrices = meta_cov_matrices,
      weights = meta_weights
    )
  )
}

# ---- Internal helpers -------------------------------------------------------

#' Validate cluster_sim inputs
#' @noRd
.cluster_sim_validate <- function(n_samples, n_clusters, n_dims,
                                  n_cells_per_sample,
                                  base_cluster_weights, expr_modes,
                                  var_bounds, mean_perturb_sd,
                                  spike_clusters, spike_samples,
                                  spike_fold_change,
                                  noise_dims, distribution, df,
                                  batch_effect_shift, batch_samples,
                                  knockout_clusters, knockout_samples) {
  stopifnot(
    is.numeric(n_samples), length(n_samples) == 1L, n_samples >= 1L,
    is.numeric(n_clusters), length(n_clusters) == 1L, n_clusters >= 1L,
    is.numeric(n_dims), length(n_dims) == 1L, n_dims >= 1L,
    is.numeric(n_cells_per_sample), length(n_cells_per_sample) == 1L,
    n_cells_per_sample >= 1L,
    is.numeric(expr_modes), length(expr_modes) >= 1L,
    is.numeric(var_bounds), length(var_bounds) == 2L,
    var_bounds[1] > 0, var_bounds[1] < var_bounds[2],
    is.numeric(mean_perturb_sd), length(mean_perturb_sd) == 1L,
    mean_perturb_sd > 0,
    is.numeric(spike_fold_change), length(spike_fold_change) == 1L,
    spike_fold_change > 0
  )

  if (!is.null(base_cluster_weights)) {
    stopifnot(
      is.numeric(base_cluster_weights),
      length(base_cluster_weights) == n_clusters,
      all(base_cluster_weights >= 0),
      abs(sum(base_cluster_weights) - 1) < 1e-10
    )
  }

  if (xor(is.null(spike_clusters), is.null(spike_samples))) {
    stop(
      "Both 'spike_clusters' and 'spike_samples' must be provided, ",
      "or both must be NULL."
    )
  }

  if (!is.null(spike_clusters)) {
    stopifnot(
      is.numeric(spike_clusters),
      all(spike_clusters >= 1L),
      all(spike_clusters <= n_clusters),
      all(spike_clusters == floor(spike_clusters))
    )
  }

  if (!is.null(spike_samples)) {
    stopifnot(
      is.numeric(spike_samples),
      all(spike_samples >= 1L),
      all(spike_samples <= n_samples),
      all(spike_samples == floor(spike_samples))
    )
  }

  # Validate noise_dims
  if (!is.null(noise_dims)) {
    stopifnot(
      is.numeric(noise_dims), length(noise_dims) == 1L, noise_dims >= 1L,
      noise_dims == floor(noise_dims)
    )
  }

  # Validate distribution / df
  if (distribution == "t") {
    if (!requireNamespace("mvtnorm", quietly = TRUE)) {
      stop(
        "Package 'mvtnorm' is required for distribution = \"t\". ",
        "Install it with install.packages(\"mvtnorm\")."
      )
    }
    stopifnot(is.numeric(df), length(df) == 1L, df > 0)
  }

  # Validate batch effect parameters
  if (xor(is.null(batch_effect_shift), is.null(batch_samples))) {
    stop(
      "Both 'batch_effect_shift' and 'batch_samples' must be provided, ",
      "or both must be NULL."
    )
  }

  if (!is.null(batch_effect_shift)) {
    stopifnot(
      is.numeric(batch_effect_shift),
      length(batch_effect_shift) == 1L || length(batch_effect_shift) == n_dims
    )
  }

  if (!is.null(batch_samples)) {
    stopifnot(
      is.numeric(batch_samples),
      all(batch_samples >= 1L),
      all(batch_samples <= n_samples),
      all(batch_samples == floor(batch_samples))
    )
  }

  # Validate knockout parameters
  if (xor(is.null(knockout_clusters), is.null(knockout_samples))) {
    stop(
      "Both 'knockout_clusters' and 'knockout_samples' must be provided, ",
      "or both must be NULL."
    )
  }

  if (!is.null(knockout_clusters)) {
    stopifnot(
      is.numeric(knockout_clusters),
      all(knockout_clusters >= 1L),
      all(knockout_clusters <= n_clusters),
      all(knockout_clusters == floor(knockout_clusters))
    )
    if (length(knockout_clusters) >= n_clusters) {
      stop("Cannot knock out all clusters.")
    }
  }

  if (!is.null(knockout_samples)) {
    stopifnot(
      is.numeric(knockout_samples),
      all(knockout_samples >= 1L),
      all(knockout_samples <= n_samples),
      all(knockout_samples == floor(knockout_samples))
    )
  }
}

#' Initialize FAUST reference weight vector
#' @noRd
.cluster_sim_init_weights <- function(n_clusters, base_cluster_weights) {
  if (!is.null(base_cluster_weights)) return(base_cluster_weights)

  w <- 0.1425 * (0.5 ^ (seq_len(n_clusters) - 1L))
  residual <- 1 - sum(w)
  if (abs(residual) > .Machine$double.eps) {
    w <- w + residual / n_clusters
  }
  w
}

#' Apply proportional spike-in to cluster weights
#' @noRd
.cluster_sim_spike_weights <- function(weights, spike_clusters,
                                       spike_fold_change) {
  spiked <- weights
  spiked[spike_clusters] <- spiked[spike_clusters] * spike_fold_change

  spiked_total <- sum(spiked[spike_clusters])
  if (spiked_total >= 1) {
    stop("Spiked cluster weights sum to >= 1; reduce 'spike_fold_change'.")
  }

  remaining <- 1 - spiked_total
  non_spike_idx <- setdiff(seq_along(weights), spike_clusters)
  non_spike_total <- sum(weights[non_spike_idx])

  if (non_spike_total > 0) {
    spiked[non_spike_idx] <- weights[non_spike_idx] *
      (remaining / non_spike_total)
  }

  spiked
}

#' Generate a random covariance matrix with bounded variances
#' @noRd
.cluster_sim_gen_cov <- function(n_dims, var_bounds) {
  if (n_dims == 1L) {
    return(matrix(stats::runif(1L, var_bounds[1], var_bounds[2]), 1L, 1L))
  }

  # Generate random correlation matrix via Wishart
  W <- stats::rWishart(1L, n_dims + 1L, diag(n_dims))[, , 1L]
  D_inv <- diag(1 / sqrt(diag(W)), nrow = n_dims)
  R <- D_inv %*% W %*% D_inv

  # Generate random variances within bounds
  vars <- stats::runif(n_dims, min = var_bounds[1], max = var_bounds[2])

  # Scale correlation matrix to covariance matrix
  D <- diag(sqrt(vars), nrow = n_dims)
  D %*% R %*% D
}

#' Zero out knocked-out clusters and renormalise weights
#' @noRd
.cluster_sim_knockout_weights <- function(weights, knockout_clusters) {
  w <- weights
  w[knockout_clusters] <- 0
  remaining <- sum(w)
  if (remaining <= 0) {
    stop("All cluster weights are zero after knockout.")
  }
  w / remaining
}
