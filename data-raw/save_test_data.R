# prep data
data('GvHD', package = 'flowCore')
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
