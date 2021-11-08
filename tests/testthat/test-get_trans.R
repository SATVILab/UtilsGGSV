test_that("get_trans works", {
  expect_s3_class(
    get_trans("sqrt"),
    "trans"
  )
  expect_s3_class(
    get_trans("root_2"),
    "trans"
  )
})
