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
  sim <- cluster_sim(
    n_samples = 4, n_clusters = 5, n_dims = 3,
    n_cells_per_sample = 100,
    spike_clusters = 1L, spike_samples = c(1L, 2L),
    spike_fold_change = 2
  )
  base_w <- sim$metadata$base_weights

  # Spiked samples: cluster 1 weight is doubled
 for (s in c(1, 2)) {
    sw <- sim$metadata$weights[[s]]
    expect_equal(sw[1], base_w[1] * 2)
    expect_equal(sum(sw), 1)

    # Non-spiked weights maintain proportional relationships
    ratio <- sw[2:5] / base_w[2:5]
    expect_equal(length(unique(round(ratio, 10))), 1)
  }

  # Non-spiked samples: weights unchanged
  for (s in c(3, 4)) {
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
