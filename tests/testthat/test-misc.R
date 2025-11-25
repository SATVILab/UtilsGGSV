test_that("corr_lab_auto is defined correctly", {
  expect_true(exists("corr_lab_auto"))
  expect_type(corr_lab_auto, "character")
  expect_true("concordance" %in% names(corr_lab_auto))
  expect_true("pearson" %in% names(corr_lab_auto))
  expect_true("spearman" %in% names(corr_lab_auto))
  expect_true("kendall" %in% names(corr_lab_auto))
})
