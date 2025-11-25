---
applyTo: "**/*.R"
---

# R Coding Standards

## Purpose & Scope

Guidelines for R code style, naming conventions, and documentation in this package.

---

## Naming Conventions

- Use `snake_case` for function names (e.g., `add_text_column`, `axis_limits`)
- Use `.` prefix for internal functions (e.g., `.ggcorr_prep`)
- Use descriptive parameter names
- Suffix data frames with `_tbl` or `_df` for clarity

## Function Documentation

Use roxygen2 comments for all exported functions:

```r
# ✅ Good: Complete roxygen documentation
#' @title Add text column to plot
#'
#' @description Adds a column of text to a ggplot object.
#'
#' @param p A ggplot object
#' @param text Character vector of text to add
#' @param coord Numeric vector of coordinates
#'
#' @return A ggplot object with text added
#' @export
#'
#' @examples
#' p <- ggplot(mtcars, aes(mpg, hp)) + geom_point()
#' add_text_column(p, "Example", c(0.1, 0.9))
```

```r
# ❌ Bad: Missing documentation
add_text_column <- function(p, text, coord) {
  # No roxygen comments
}
```

## Code Organization

- One main function per file
- Supporting internal functions in same file
- Keep functions focused on single responsibilities

```r
# ✅ Good: Main function with internal helpers
#' @export
ggcorr <- function(data, ...) {
  .ggcorr_check(data)
  .ggcorr_prep(data)
  .ggcorr_plot(data)
}

.ggcorr_check <- function(data) {
  # Internal validation
}
```

## Dependencies

- Use explicit imports via roxygen2 `@importFrom`
- Prefer `package::function()` for clarity
- Core dependencies go in `Imports:` in DESCRIPTION
- Optional packages go in `Suggests:`

```r
# ✅ Good: Explicit namespace
#' @importFrom dplyr filter mutate
#' @importFrom ggplot2 ggplot aes geom_point
```

## ggplot2 Integration

- Functions should accept and return ggplot objects
- Support both `|>` and `%>%` pipes
- Use `ggplot2::` prefix for ggplot2 functions

```r
# ✅ Good: Returns ggplot object, uses namespace
axis_limits <- function(p, limits_equal = FALSE) {
  p + ggplot2::expand_limits(...)
}
```

## Transformations

- Support scale transformations via `get_trans()`
- Use `scales` package for transformation objects
- Text placement should respect transformations

---

## Code Examples

### Correct Pattern

```r
#' @title Process data for plotting
#' @param data A data frame
#' @return A processed data frame
#' @export
process_data <- function(data) {
  data |>
    dplyr::filter(!is.na(value)) |>
    dplyr::mutate(scaled = value / max(value))
}
```

### Incorrect Pattern

```r
# Missing documentation, uses library() instead of imports
process_data <- function(data) {
  library(dplyr)  # ❌ Never use library() in package code
  filter(data, !is.na(value))
}
```
