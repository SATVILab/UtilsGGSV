test_that("save_plot works", {
  p <- ggplot2::ggplot()
  save_plot(filename = file.path(tempdir(), "test_p"), plot = p)
  expect_true(file.exists(file.path(tempdir(), "test_p.pdf")))
  expect_true(file.exists(file.path(tempdir(), "test_p.png")))
  try(
    file.remove(file.path(tempdir(), c("test_p.pdf", "test_p.png"))),
    silent = TRUE
    )
})

test_that("save_plot handles .png extension in filename", {
  p <- ggplot2::ggplot()
  result <- save_plot(filename = file.path(tempdir(), "test_p2.png"), plot = p)
  expect_true(file.exists(file.path(tempdir(), "test_p2.pdf")))
  expect_true(file.exists(file.path(tempdir(), "test_p2.png")))
  expect_equal(result, file.path(tempdir(), "test_p2.png"))
  try(
    file.remove(file.path(tempdir(), c("test_p2.pdf", "test_p2.png"))),
    silent = TRUE
  )
})

test_that("save_plot handles .pdf extension in filename", {
  p <- ggplot2::ggplot()
  result <- save_plot(filename = file.path(tempdir(), "test_p3.pdf"), plot = p)
  expect_true(file.exists(file.path(tempdir(), "test_p3.pdf")))
  expect_true(file.exists(file.path(tempdir(), "test_p3.png")))
  expect_equal(result, file.path(tempdir(), "test_p3.pdf"))
  try(
    file.remove(file.path(tempdir(), c("test_p3.pdf", "test_p3.png"))),
    silent = TRUE
  )
})

test_that("save_plot creates directory if it doesn't exist", {
  p <- ggplot2::ggplot()
  new_dir <- file.path(tempdir(), "new_subdir_save_plot")
  save_plot(filename = file.path(new_dir, "test_p4"), plot = p)
  expect_true(dir.exists(new_dir))
  expect_true(file.exists(file.path(new_dir, "test_p4.pdf")))
  expect_true(file.exists(file.path(new_dir, "test_p4.png")))
  try(
    unlink(new_dir, recursive = TRUE),
    silent = TRUE
  )
})

test_that("save_plot works with single device", {
  p <- ggplot2::ggplot()
  save_plot(filename = file.path(tempdir(), "test_p5"), plot = p, device = "png")
  expect_true(file.exists(file.path(tempdir(), "test_p5.png")))
  expect_false(file.exists(file.path(tempdir(), "test_p5.pdf")))
  try(
    file.remove(file.path(tempdir(), "test_p5.png")),
    silent = TRUE
  )
})

test_that("ggsave2 works", {
  p <- ggplot2::ggplot()
  ggsave2(filename = file.path(tempdir(), "test_p6"), plot = p)
  expect_true(file.exists(file.path(tempdir(), "test_p6.pdf")))
  expect_true(file.exists(file.path(tempdir(), "test_p6.png")))
  try(
    file.remove(file.path(tempdir(), c("test_p6.pdf", "test_p6.png"))),
    silent = TRUE
  )
})

test_that("ggsave2 handles .png extension in filename", {
  p <- ggplot2::ggplot()
  result <- ggsave2(filename = file.path(tempdir(), "test_p7.png"), plot = p)
  expect_true(file.exists(file.path(tempdir(), "test_p7.pdf")))
  expect_true(file.exists(file.path(tempdir(), "test_p7.png")))
  expect_equal(result, file.path(tempdir(), "test_p7.png"))
  try(
    file.remove(file.path(tempdir(), c("test_p7.pdf", "test_p7.png"))),
    silent = TRUE
  )
})

test_that("ggsave2 handles .pdf extension in filename", {
  p <- ggplot2::ggplot()
  result <- ggsave2(filename = file.path(tempdir(), "test_p8.pdf"), plot = p)
  expect_true(file.exists(file.path(tempdir(), "test_p8.pdf")))
  expect_true(file.exists(file.path(tempdir(), "test_p8.png")))
  expect_equal(result, file.path(tempdir(), "test_p8.pdf"))
  try(
    file.remove(file.path(tempdir(), c("test_p8.pdf", "test_p8.png"))),
    silent = TRUE
  )
})

test_that("ggsave2 creates directory if it doesn't exist", {
  p <- ggplot2::ggplot()
  new_dir <- file.path(tempdir(), "new_subdir_ggsave2")
  ggsave2(filename = file.path(new_dir, "test_p9"), plot = p)
  expect_true(dir.exists(new_dir))
  expect_true(file.exists(file.path(new_dir, "test_p9.pdf")))
  expect_true(file.exists(file.path(new_dir, "test_p9.png")))
  try(
    unlink(new_dir, recursive = TRUE),
    silent = TRUE
  )
})
