test_that(".utilsggsv_dep_install returns invisibly for installed packages", {
  # Test with a package that is definitely installed
  result <- .utilsggsv_dep_install("testthat")
  expect_true(result)
})

test_that(".utilsggsv_renv_detect works", {
  # This should return a logical value
  result <- .utilsggsv_renv_detect()
  expect_type(result, "logical")
})

test_that(".utilsggsv_renv_lockfile_path_get works", {
  # This should return a character path
  result <- .utilsggsv_renv_lockfile_path_get()
  expect_type(result, "character")
})

test_that(".utilsggsv_renv_lockfile_path_get_non_override works", {
  # This should return a character path
  result <- .utilsggsv_renv_lockfile_path_get_non_override()
  expect_type(result, "character")
})

test_that(".utilsggsv_path_rscript_get returns valid path", {
  result <- .utilsggsv_path_rscript_get()
  expect_type(result, "character")
  expect_true(grepl("Rscript", result))
})

test_that(".utilsggsv_renv_lockfile_path_get uses RENV_PATHS_LOCKFILE override", {
  # Test with override set to a directory path (ending with /)
  withr::with_envvar(
    c("RENV_PATHS_LOCKFILE" = "/tmp/test/"),
    {
      result <- .utilsggsv_renv_lockfile_path_get()
      expect_equal(result, "/tmp/test/renv.lock")
    }
  )

  # Test with override set to a file path (not ending with /)
  withr::with_envvar(
    c("RENV_PATHS_LOCKFILE" = "/tmp/test/custom.lock"),
    {
      result <- .utilsggsv_renv_lockfile_path_get()
      expect_equal(result, "/tmp/test/custom.lock")
    }
  )
})
