test_that("set_", {
  expect_true(set_env_var_r_gscmd())
  expect_error(set_env_var_r_gscmd('a'))
  expect_error(set_env_var_r_gscmd(1))
  expect_true(set_gs_var())
})
