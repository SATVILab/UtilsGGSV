---
applyTo: "tests/**/*"
---

# Testing Guidelines

## Purpose & Scope

Guidelines for writing and organizing tests using testthat edition 3.

---

## Test Organization

- Place tests in `tests/testthat/test-{function}.R`
- Name test files after the function they test
- Group related tests with `describe()` blocks

```r
# ✅ Good: tests/testthat/test-axis_limits.R
test_that("axis_limits returns ggplot object", {
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg, hp)) +
    ggplot2::geom_point()
  result <- axis_limits(p, limits_equal = TRUE)
  expect_s3_class(result, "ggplot")
})
```

## Test Structure

Use testthat edition 3 syntax:

```r
# ✅ Good: Clear test structure
test_that("function handles edge cases", {
  # Arrange

  data <- data.frame(x = 1:5, y = 1:5)

  # Act
  result <- my_function(data)

  # Assert
  expect_equal(nrow(result), 5)
  expect_true(all(result$y > 0))
})
```

## What to Test

- **Core functionality**: Main function behavior
- **Edge cases**: Empty data, single points, NA values
- **Error conditions**: Invalid inputs trigger errors
- **Transformations**: Behavior with different scales

```r
# ✅ Good: Testing edge cases
test_that("ggcorr handles two groups", {
  data <- data.frame(
    group = rep(c("a", "b"), each = 5),
    response = rnorm(10),
    pid = rep(1:5, 2)
  )
  result <- ggcorr(data, grp = "group", y = "response", id = "pid")
  expect_s3_class(result, "ggplot")
})

test_that("ggcorr errors with one group", {
  data <- data.frame(group = rep("a", 5), response = 1:5, pid = 1:5)
  expect_error(
    ggcorr(data, grp = "group", y = "response", id = "pid"),
    "at least two levels"
  )
})
```

## Testing Plots

For plotting functions:

- Check output is ggplot object
- Verify layers are added correctly
- Test with different transformations

```r
# ✅ Good: Plot testing
test_that("add_text_column adds text layer", {
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg, hp)) +
    ggplot2::geom_point()
  result <- add_text_column(p, x = mtcars$mpg, y = mtcars$hp,
                            text = "test", coord = c(0.5, 0.5))
  expect_s3_class(result, "ggplot")
  # Check text layer was added
  expect_true(any(sapply(result$layers, function(l) {
    inherits(l$geom, "GeomText")
  })))
})
```

---

## Running Tests

```r
# Run all tests
devtools::test()

# Run specific test file
testthat::test_file("tests/testthat/test-axis_limits.R")

# Run with verbose output
devtools::test(reporter = "progress")
```

## Test Patterns

### Skip Expensive Tests

```r
test_that("expensive visual test", {
  skip_on_cran()
  skip_if_not_installed("vdiffr")
  # Visual regression test
})
```

### Use Fixtures

```r
# Helper function in tests/testthat/helper.R
make_test_data <- function(n = 10) {
  data.frame(
    x = rnorm(n),
    y = rnorm(n),
    group = sample(letters[1:3], n, replace = TRUE)
  )
}
```

---

## Common Mistakes

```r
# ❌ Bad: No expectations
test_that("function works", {
  result <- my_function(data)
  # Missing expect_*() calls
})

# ❌ Bad: Testing implementation, not behavior
test_that("uses dplyr internally", {
  # Don't test internal implementation details
})

# ✅ Good: Testing behavior
test_that("function returns correct structure", {
  result <- my_function(data)
  expect_s3_class(result, "data.frame")
  expect_named(result, c("x", "y", "value"))
})
```
