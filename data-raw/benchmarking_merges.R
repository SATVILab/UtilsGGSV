# data-raw/benchmarking_merges.R
#
# Pre-computes the benchmarking results used by the
# "benchmarking-merges" vignette.  Running this script reproduces the
# full simulation grid from the FAUST review paper (Greene et al., 2021)
# and saves the summary data frame as an RDS in inst/extdata/.
#
# Requirements: all Imports and Suggests of UtilsGGSV must be installed.
#
# Usage:
#   Rscript data-raw/benchmarking_merges.R
#
# The script is computationally expensive (≈ hours on a single core).
# Adjust n_iter, n_cells_per_sample, or the cluster_counts grid below
# for quicker exploratory runs.

pkgload::load_all(".")

set.seed(42)

# ---- Configuration ----------------------------------------------------------

cluster_counts <- seq(5, 115, by = 10)
n_iter         <- 5L
n_samples      <- 10L
n_cells_per_sample <- 25000L
n_dims         <- 10L
noise_dims     <- 5L
transforms     <- c("none", "faust_gamma")

# ---- Helper: run algorithms on a data matrix --------------------------------

run_algorithms <- function(mat, vars, n_true_clusters) {
  thresholds <- stats::setNames(
    lapply(vars, function(v) 0),
    vars
  )

  # 1. FlowSOM (standard SOM)
  som_cl <- cluster_som(mat, vars = vars)
  n_som  <- length(unique(som_cl))

  # 2. cluster_som_merge (our method)
  som_merge_res <- cluster_som_merge(mat, vars = vars, thresholds = thresholds)
  som_merge_cl  <- som_merge_res$assign$merged
  n_som_merge   <- length(unique(som_merge_cl))

  # 3. Louvain (standard)
  louv_cl <- cluster_louvain(mat, vars = vars)
  n_louv  <- length(unique(louv_cl))

  # 4. cluster_louvain_merge (our method)
  louv_merge_res <- cluster_louvain_merge(
    mat, vars = vars, thresholds = thresholds
  )
  louv_merge_cl  <- louv_merge_res$assign$merged
  n_louv_merge   <- length(unique(louv_merge_cl))

  list(
    FlowSOM            = list(labels = som_cl,        k = n_som),
    SOM_merge           = list(labels = som_merge_cl,  k = n_som_merge),
    Louvain             = list(labels = louv_cl,       k = n_louv),
    Louvain_merge       = list(labels = louv_merge_cl, k = n_louv_merge)
  )
}

# ---- Helper: Adjusted Rand Index -------------------------------------------

#' Simple ARI implementation (no external dependency).
ari <- function(labels_true, labels_pred) {
  ct <- table(labels_true, labels_pred)
  sum_comb2 <- function(x) sum(choose(x, 2))

  a <- sum_comb2(ct)
  b <- sum_comb2(rowSums(ct))
  c <- sum_comb2(colSums(ct))
  n <- sum(ct)
  d <- choose(n, 2)

  expected <- b * c / d
  max_index <- (b + c) / 2
  if (max_index == expected) return(1)
  (a - expected) / (max_index - expected)
}

# ---- Main simulation loop ---------------------------------------------------

results <- data.frame(
  transform   = character(0),
  n_clusters  = integer(0),
  iter        = integer(0),
  algorithm   = character(0),
  k_estimated = integer(0),
  ari         = numeric(0),
  stringsAsFactors = FALSE
)

for (tx in transforms) {
  for (nc in cluster_counts) {
    for (it in seq_len(n_iter)) {
      message(
        sprintf("transform=%s  clusters=%d  iter=%d", tx, nc, it)
      )

      sim <- cluster_sim(
        n_samples          = n_samples,
        n_clusters         = nc,
        n_dims             = n_dims,
        n_cells_per_sample = n_cells_per_sample,
        noise_dims         = noise_dims,
        transform          = tx
      )

      dim_vars <- paste0("dim_", seq_len(n_dims))
      mat <- as.data.frame(sim$data[, dim_vars])

      true_labels <- as.character(sim$data$cluster_id)

      algo_res <- run_algorithms(mat, dim_vars, nc)

      for (alg_name in names(algo_res)) {
        results <- rbind(results, data.frame(
          transform   = if (tx == "none") "Gaussian" else "Non-Gaussian",
          n_clusters  = nc,
          iter        = it,
          algorithm   = alg_name,
          k_estimated = algo_res[[alg_name]]$k,
          ari         = ari(true_labels, algo_res[[alg_name]]$labels),
          stringsAsFactors = FALSE
        ))
      }
    }
  }
}

# ---- Summarise: median across iterations ------------------------------------

bench_summary <- results |>
  dplyr::group_by(transform, n_clusters, algorithm) |>
  dplyr::summarise(
    median_k   = stats::median(k_estimated),
    median_ari = stats::median(ari),
    .groups = "drop"
  )

# ---- Single-population sanity check -----------------------------------------

single_pop_results <- data.frame(
  algorithm   = character(0),
  k_estimated = integer(0),
  stringsAsFactors = FALSE
)

for (it in seq_len(5L)) {
  sim1 <- cluster_sim(
    n_samples          = 1L,
    n_clusters         = 1L,
    n_dims             = 1L,
    n_cells_per_sample = 1000L,
    base_cluster_weights = 1
  )
  mat1 <- as.data.frame(sim1$data[, "dim_1", drop = FALSE])

  thresholds1 <- list(dim_1 = 0)

  som_cl1 <- cluster_som(mat1, vars = "dim_1", x_dim = 3, y_dim = 3)
  som_merge_res1 <- cluster_som_merge(
    mat1, vars = "dim_1", thresholds = thresholds1, x_dim = 3, y_dim = 3
  )
  louv_cl1 <- cluster_louvain(mat1, vars = "dim_1")
  louv_merge_res1 <- cluster_louvain_merge(
    mat1, vars = "dim_1", thresholds = thresholds1
  )

  single_pop_results <- rbind(single_pop_results, data.frame(
    iter        = it,
    algorithm   = c("FlowSOM", "SOM_merge", "Louvain", "Louvain_merge"),
    k_estimated = c(
      length(unique(som_cl1)),
      length(unique(som_merge_res1$assign$merged)),
      length(unique(louv_cl1)),
      length(unique(louv_merge_res1$assign$merged))
    ),
    stringsAsFactors = FALSE
  ))
}

# ---- Save -------------------------------------------------------------------

saveRDS(
  list(
    results         = results,
    bench_summary   = bench_summary,
    single_pop      = single_pop_results
  ),
  file = "inst/extdata/benchmarking_merges.rds"
)

message("Done. Results saved to inst/extdata/benchmarking_merges.rds")
