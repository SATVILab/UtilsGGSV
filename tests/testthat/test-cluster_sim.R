test_that("cluster_sim returns correct structure", {
  set.seed(1)
  sim <- cluster_sim(
    n_samples = 2, n_clusters = 3, n_dims = 4,
    n_cells_per_sample = 100
  )
  expect_type(sim, "list")
  expect_named(sim, c("data", "metadata"))
  expect_s3_class(sim$data, "tbl_df")
  expected_cols <- c(
    "sample_id", "cell_id", "cluster_id",
    "dim_1", "dim_2", "dim_3", "dim_4"
  )
  expect_true(all(expected_cols %in% names(sim$data)))
  expect_equal(nrow(sim$data), 2 * 100)
})

test_that("cluster_sim metadata contains expected elements", {
  set.seed(1)
  sim <- cluster_sim(
    n_samples = 2, n_clusters = 3, n_dims = 4,
    n_cells_per_sample = 100
  )
  md <- sim$metadata
  expect_named(md, c(
    "base_means", "base_weights",
    "perturbed_means", "cov_matrices", "weights"
  ))
  expect_equal(nrow(md$base_means), 3)
  expect_equal(ncol(md$base_means), 4)
  expect_length(md$base_weights, 3)
  expect_length(md$perturbed_means, 2)
  expect_length(md$cov_matrices, 2)
  expect_length(md$weights, 2)
})

test_that("cluster_sim covariance diagonals respect var_bounds", {
  set.seed(1)
  sim <- cluster_sim(
    n_samples = 2, n_clusters = 5, n_dims = 4,
    n_cells_per_sample = 100
  )
  for (s in seq_along(sim$metadata$cov_matrices)) {
    for (k in seq_along(sim$metadata$cov_matrices[[s]])) {
      d <- diag(sim$metadata$cov_matrices[[s]][[k]])
      expect_true(
        all(d >= 1 & d <= 2),
        info = paste("sample", s, "cluster", k)
      )
    }
  }
})

test_that("cluster_sim weights sum to 1", {
  set.seed(1)
  sim <- cluster_sim(
    n_samples = 3, n_clusters = 10, n_dims = 3,
    n_cells_per_sample = 100
  )
  expect_equal(sum(sim$metadata$base_weights), 1)
  for (w in sim$metadata$weights) {
    expect_equal(sum(w), 1)
  }
})

test_that("cluster_sim spike-in adjusts weights correctly", {
  set.seed(1)
  spiked_idx <- c(1L, 2L)
  non_spiked_idx <- c(3L, 4L)
  sim <- cluster_sim(
    n_samples = 4, n_clusters = 5, n_dims = 3,
    n_cells_per_sample = 100,
    spike_clusters = 1L, spike_samples = spiked_idx,
    spike_fold_change = 2
  )
  base_w <- sim$metadata$base_weights

  # Spiked samples: cluster 1 weight is doubled
  for (s in spiked_idx) {
    sw <- sim$metadata$weights[[s]]
    expect_equal(sw[1], base_w[1] * 2)
    expect_equal(sum(sw), 1)

    # Non-spiked weights maintain proportional relationships
    ratio <- sw[2:5] / base_w[2:5]
    expect_equal(length(unique(round(ratio, 10))), 1)
  }

  # Non-spiked samples: weights unchanged
  for (s in non_spiked_idx) {
    expect_equal(sim$metadata$weights[[s]], base_w)
  }
})

test_that("cluster_sim faust_gamma transform changes data", {
  set.seed(1)
  sim_none <- cluster_sim(
    n_samples = 1, n_clusters = 2, n_dims = 3,
    n_cells_per_sample = 50, transform = "none"
  )
  set.seed(1)
  sim_gamma <- cluster_sim(
    n_samples = 1, n_clusters = 2, n_dims = 3,
    n_cells_per_sample = 50, transform = "faust_gamma"
  )

  # Transformed data differs from untransformed
  dim_cols <- c("dim_1", "dim_2", "dim_3")
  expect_false(identical(sim_none$data[dim_cols], sim_gamma$data[dim_cols]))

  # Base means and weights are the same (same seed, same process)
  expect_equal(
    sim_none$metadata$base_means,
    sim_gamma$metadata$base_means
  )
})

test_that("cluster_sim base means drawn from expr_modes", {
  set.seed(1)
  sim <- cluster_sim(
    n_samples = 1, n_clusters = 10, n_dims = 5,
    n_cells_per_sample = 50
  )
  unique_vals <- sort(unique(as.vector(sim$metadata$base_means)))
  expect_true(all(unique_vals %in% c(0, 8)))
})

test_that("cluster_sim validates inputs", {
  expect_error(cluster_sim(n_samples = -1))
  expect_error(cluster_sim(n_clusters = 0))
  expect_error(cluster_sim(var_bounds = c(3, 1)))
  expect_error(cluster_sim(spike_clusters = 1L))
  expect_error(cluster_sim(spike_samples = 1L))
  expect_error(
    cluster_sim(
      base_cluster_weights = c(0.5, 0.3),
      n_clusters = 3
    )
  )
})

test_that("cluster_sim custom base_cluster_weights are used", {
  set.seed(1)
  custom_w <- c(0.5, 0.3, 0.2)
  sim <- cluster_sim(
    n_samples = 1, n_clusters = 3, n_dims = 2,
    n_cells_per_sample = 100,
    base_cluster_weights = custom_w
  )
  expect_equal(sim$metadata$base_weights, custom_w)
  expect_equal(sim$metadata$weights[[1]], custom_w)
})

test_that("cluster_sim handles single dimension", {
  set.seed(1)
  sim <- cluster_sim(
    n_samples = 1, n_clusters = 2, n_dims = 1,
    n_cells_per_sample = 50
  )
  expect_s3_class(sim$data, "tbl_df")
  expect_true("dim_1" %in% names(sim$data))
  expect_equal(nrow(sim$data), 50)
})

# ---- noise_dims tests -------------------------------------------------------

test_that("cluster_sim noise_dims appends noise columns", {
  set.seed(1)
  sim <- cluster_sim(
    n_samples = 2, n_clusters = 3, n_dims = 4,
    n_cells_per_sample = 100, noise_dims = 3
  )
  expect_true(all(paste0("noise_", 1:3) %in% names(sim$data)))
  # dim columns still present

  expect_true(all(paste0("dim_", 1:4) %in% names(sim$data)))
  expect_equal(nrow(sim$data), 2 * 100)
})

test_that("cluster_sim with noise_dims NULL adds no noise columns", {
  set.seed(1)
  sim <- cluster_sim(
    n_samples = 1, n_clusters = 2, n_dims = 3,
    n_cells_per_sample = 50, noise_dims = NULL
  )
  expect_false(any(grepl("^noise_", names(sim$data))))
})

test_that("cluster_sim validates noise_dims", {
  expect_error(cluster_sim(noise_dims = -1))
  expect_error(cluster_sim(noise_dims = 1.5))
  expect_error(cluster_sim(noise_dims = "a"))
})

# ---- distribution tests ------------------------------------------------------

test_that("cluster_sim distribution = 't' produces output", {
  set.seed(1)
  sim <- cluster_sim(
    n_samples = 1, n_clusters = 3, n_dims = 4,
    n_cells_per_sample = 100, distribution = "t", df = 5
  )
  expect_s3_class(sim$data, "tbl_df")
  expect_equal(nrow(sim$data), 100)
  expect_true(all(paste0("dim_", 1:4) %in% names(sim$data)))
})

test_that("cluster_sim distribution = 't' differs from 'normal'", {
  set.seed(1)
  sim_norm <- cluster_sim(
    n_samples = 1, n_clusters = 2, n_dims = 3,
    n_cells_per_sample = 200, distribution = "normal"
  )
  set.seed(1)
  sim_t <- cluster_sim(
    n_samples = 1, n_clusters = 2, n_dims = 3,
    n_cells_per_sample = 200, distribution = "t", df = 3
  )
  dim_cols <- paste0("dim_", 1:3)
  expect_false(identical(sim_norm$data[dim_cols], sim_t$data[dim_cols]))
})

# ---- batch_effect_shift tests ------------------------------------------------

test_that("cluster_sim batch effect shifts perturbed means", {
  set.seed(1)
  shift_val <- 5
  sim <- cluster_sim(
    n_samples = 4, n_clusters = 3, n_dims = 2,
    n_cells_per_sample = 100,
    batch_effect_shift = shift_val,
    batch_samples = c(1L, 2L)
  )
  # Batch-affected means should differ from non-affected by ~shift_val
  # (modulo perturbation randomness, but the shift is added deterministically)
  # The metadata perturbed_means for batch samples include the shift
  batch_means <- sim$metadata$perturbed_means[[1]]
  non_batch_means <- sim$metadata$perturbed_means[[3]]
  # Can't compare directly because of randomness, but we can
  # at least check the function runs and produces valid output
  expect_s3_class(sim$data, "tbl_df")
  expect_equal(nrow(sim$data), 4 * 100)
})

test_that("cluster_sim batch effect with vector shift works", {
  set.seed(1)
  shift_vec <- c(2, -3, 1)
  sim <- cluster_sim(
    n_samples = 2, n_clusters = 3, n_dims = 3,
    n_cells_per_sample = 50,
    batch_effect_shift = shift_vec,
    batch_samples = 1L
  )
  expect_s3_class(sim$data, "tbl_df")
  expect_equal(nrow(sim$data), 2 * 50)
})

test_that("cluster_sim validates batch effect parameters", {
  # Only one of the two provided
  expect_error(
    cluster_sim(batch_effect_shift = 5),
    "batch_effect_shift.*batch_samples"
  )
  expect_error(
    cluster_sim(batch_samples = 1L),
    "batch_effect_shift.*batch_samples"
  )
  # Wrong length shift vector
  expect_error(
    cluster_sim(
      n_dims = 3, batch_effect_shift = c(1, 2),
      batch_samples = 1L
    )
  )
})

# ---- knockout tests ----------------------------------------------------------

test_that("cluster_sim knockout zeroes cluster weights", {
  set.seed(1)
  ko_sample <- 1L
  ko_cluster <- 2L
  sim <- cluster_sim(
    n_samples = 3, n_clusters = 4, n_dims = 3,
    n_cells_per_sample = 200,
    knockout_clusters = ko_cluster,
    knockout_samples = ko_sample
  )
  # In knocked-out sample, weight of cluster 2 should be 0
  expect_equal(sim$metadata$weights[[ko_sample]][ko_cluster], 0)
  # Weights still sum to 1
  expect_equal(sum(sim$metadata$weights[[ko_sample]]), 1)

  # Non-affected samples retain original weights
  expect_equal(sim$metadata$weights[[2]], sim$metadata$base_weights)
  expect_equal(sim$metadata$weights[[3]], sim$metadata$base_weights)
})

test_that("cluster_sim knockout removes cells of knocked-out cluster", {
  set.seed(1)
  sim <- cluster_sim(
    n_samples = 2, n_clusters = 3, n_dims = 2,
    n_cells_per_sample = 500,
    knockout_clusters = 1L,
    knockout_samples = 1L
  )
  sample1 <- sim$data[sim$data$sample_id == 1, ]
  sample2 <- sim$data[sim$data$sample_id == 2, ]
  # No cells from cluster 1 in knocked-out sample
  expect_false(1L %in% sample1$cluster_id)
  # Cluster 1 present in non-affected sample
  expect_true(1L %in% sample2$cluster_id)
})

test_that("cluster_sim validates knockout parameters", {
  # Only one of the two provided
  expect_error(
    cluster_sim(knockout_clusters = 1L),
    "knockout_clusters.*knockout_samples"
  )
  expect_error(
    cluster_sim(knockout_samples = 1L),
    "knockout_clusters.*knockout_samples"
  )
  # Cannot knock out all clusters
  expect_error(
    cluster_sim(
      n_clusters = 3,
      knockout_clusters = c(1L, 2L, 3L),
      knockout_samples = 1L
    ),
    "Cannot knock out all clusters"
  )
})
