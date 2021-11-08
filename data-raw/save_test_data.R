devtools::load_all()
# prep data
data("GvHD", package = "flowCore")
ex_tbl <- flowCore::exprs(GvHD[[1]]) %>%
  tibble::as_tibble()
saveRDS(
  object = ex_tbl,
  file = file.path(
    here::here(),
    "tests",
    "ex_tbl.rds"
  )
)

# save p
marker <- c("FL2-H", "FL3-H")

p_axis_limits <- cytoutils::plot_cyto(
  data = ex_tbl,
  marker = marker
)

saveRDS(
  object = p_axis_limits,
  file = file.path(
    here::here(),
    "tests",
    "p_axis_limits.rds"
  )
)
