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
