test_that("get_trans works", {
  expect_s3_class(
    get_trans("sqrt"),
    "transform"
  )
  expect_s3_class(
    get_trans("root_2"),
    "transform"
  )
})
