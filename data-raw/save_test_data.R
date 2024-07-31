devtools::load_all()
# prep data
if (!requireNamespace("flowCore", quietly = TRUE)) {
  renv::install("bioc::flowCore")
}
utils::data("GvHD", package = "flowCore")
ex_tbl <- flowCore::exprs(GvHD[[1]]) |>
  tibble::as_tibble()
saveRDS(
  object = ex_tbl,
  file = testthat::test_path("ex_tbl.rds")
)

# save p
marker <- c("FL2-H", "FL3-H")

if (!requireNamespace("UtilsCytoRSV", quietly = TRUE)) {
  renv::install("SATVILab/UtilsCytoRSV")
}

p_axis_limits <- UtilsCytoRSV::plot_cyto(
  data = ex_tbl,
  marker = marker
)

saveRDS(
  object = p_axis_limits,
  file = testthat::test_path("p_axis_limits.rds")
)
