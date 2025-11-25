test_that("get_trans works for root transformations", {
  expect_s3_class(
    get_trans("sqrt"),
    "transform"
  )
  expect_s3_class(
    get_trans("root_2"),
    "transform"
  )
  expect_s3_class(
    get_trans("root_quadratic"),
    "transform"
  )
  expect_s3_class(
    get_trans("root_quad"),
    "transform"
  )
  expect_s3_class(
    get_trans("root_second"),
    "transform"
  )
  expect_s3_class(
    get_trans("root_two"),
    "transform"
  )
  expect_s3_class(
    get_trans("root_3"),
    "transform"
  )
  expect_s3_class(
    get_trans("root_three"),
    "transform"
  )
  expect_s3_class(
    get_trans("root_third"),
    "transform"
  )
  expect_s3_class(
    get_trans("root_cube"),
    "transform"
  )
  expect_s3_class(
    get_trans("root_4"),
    "transform"
  )
  expect_s3_class(
    get_trans("root_four"),
    "transform"
  )
  expect_s3_class(
    get_trans("root_fourth"),
    "transform"
  )
  expect_s3_class(
    get_trans("root_5"),
    "transform"
  )
  expect_s3_class(
    get_trans("root_five"),
    "transform"
  )
  expect_s3_class(
    get_trans("root_fifth"),
    "transform"
  )
})

test_that("get_trans works for standard transformations", {
  expect_s3_class(
    get_trans("identity"),
    "transform"
  )
  expect_s3_class(
    get_trans("log10"),
    "transform"
  )
  expect_s3_class(
    get_trans("log2"),
    "transform"
  )
  expect_s3_class(
    get_trans("log1p"),
    "transform"
  )
  expect_s3_class(
    get_trans("log"),
    "transform"
  )
  expect_s3_class(
    get_trans("asinh"),
    "transform"
  )
  expect_s3_class(
    get_trans("asn"),
    "transform"
  )
  expect_s3_class(
    get_trans("exp"),
    "transform"
  )
  expect_s3_class(
    get_trans("reciprocal"),
    "transform"
  )
})

test_that("get_trans passes through trans objects", {
  trans_obj <- scales::identity_trans()
  result <- get_trans(trans_obj)
  expect_identical(result, trans_obj)
})

test_that("get_trans errors for unknown class", {
  expect_error(
    get_trans(123),
    "not recognised"
  )
})

test_that("get_trans transformations work correctly", {
  # Test sqrt transformation
  sqrt_trans <- get_trans("sqrt")
  expect_equal(sqrt_trans$transform(4), 2)
  expect_equal(sqrt_trans$inverse(2), 4)
  expect_equal(sqrt_trans$inverse(-1), 0)  # Edge case: negative input

  # Test root_3 transformation
  cube_trans <- get_trans("root_cube")
  expect_equal(cube_trans$transform(27), 3)
  expect_equal(cube_trans$inverse(3), 27)
  expect_equal(cube_trans$inverse(-1), 0)  # Edge case: negative input

  # Test root_4 transformation
  fourth_trans <- get_trans("root_fourth")
  expect_equal(fourth_trans$transform(16), 2)
  expect_equal(fourth_trans$inverse(2), 16)
  expect_equal(fourth_trans$inverse(-1), 0)  # Edge case: negative input

  # Test root_5 transformation
  fifth_trans <- get_trans("root_fifth")
  expect_equal(fifth_trans$transform(32), 2)
  expect_equal(fifth_trans$inverse(2), 32)
  expect_equal(fifth_trans$inverse(-1), 0)  # Edge case: negative input

  # Test asinh transformation
  asinh_trans <- get_trans("asinh")
  expect_equal(asinh_trans$transform(0), 0)
  expect_equal(asinh_trans$inverse(0), 0)
})
