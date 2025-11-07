# Copilot Instructions for UtilsGGSV

This is an R package that provides utility functions for working with `ggplot2` objects. The package is focused on plotting utilities for the SATVILab team.

## Package Information

- **Type**: R Package
- **Main Purpose**: Utility functions for ggplot2 plotting
- **Key Dependencies**: ggplot2, cowplot, scales, dplyr, tibble, purrr
- **Test Framework**: testthat (edition 3)

## Development Workflow

### Building the Package

```r
# Load package in development mode
devtools::load_all()

# Build documentation
devtools::document()

# Build package
devtools::build()
```

### Running Tests

```r
# Run all tests
devtools::test()

# Or using R CMD check (recommended before committing)
devtools::check()
```

For CI/CD validation, the package uses R CMD check via GitHub Actions. The workflow is defined in `.github/workflows/R-CMD-check.yaml` and tests across multiple R versions and operating systems.

### Documentation

- Documentation is generated using roxygen2
- Always update roxygen comments when modifying functions
- README.md is generated from README.Rmd - edit the .Rmd file, not .md directly
- Run `devtools::document()` to regenerate man/ files after editing roxygen comments

## Repository Structure

- `R/`: Core package functions
  - `add_text_column.R`: Add text columns to plots
  - `axis_limits.R`: Manage axis limits and equality
  - `ggcorr.R`: Correlation plotting functions
  - `get_trans.R`: Transformation utilities
  - `save_plot.R`: Plot saving utilities
  - `misc.R`: Miscellaneous utility functions
- `man/`: Auto-generated documentation (do not edit manually)
- `tests/testthat/`: Test files
- `data-raw/`: Raw data and data generation scripts
- `_dependencies.R`: Additional development dependencies
- `.github/workflows/`: CI/CD workflows
  - `R-CMD-check.yaml`: Standard R package check
  - `copilot-setup-steps.yml`: Copilot environment setup

## Code Style and Conventions

### R Code Standards

1. **Function Documentation**
   - Use roxygen2 comments (`#'`) for all exported functions
   - Include `@param`, `@return`, `@export`, and `@examples` sections
   - Enable markdown in roxygen: `Roxygen: list(markdown = TRUE)` is already configured

2. **Naming Conventions**
   - Use snake_case for function names (e.g., `add_text_column`, `axis_limits`)
   - Use descriptive parameter names
   - Suffix tibbles/data frames with `_tbl` or `_df` where helpful for clarity

3. **Code Organization**
   - One main function per file (with supporting internal functions allowed)
   - Use helper functions for complex operations
   - Keep functions focused on single responsibilities

4. **Dependencies**
   - Import functions explicitly in NAMESPACE (managed via roxygen2 @importFrom)
   - Suggest optional packages in DESCRIPTION under Suggests:
   - Core dependencies go in Imports:

5. **Testing**
   - Write tests for all exported functions
   - Use testthat edition 3 syntax
   - Place tests in `tests/testthat/test-{function}.R`
   - Test edge cases and error conditions
   - Include visual regression tests where appropriate for plotting functions

### Package-Specific Guidelines

1. **ggplot2 Integration**
   - Functions should accept and return ggplot objects where possible
   - Maintain compatibility with the tidyverse pipe (`|>` or `%>%`)
   - Use `ggplot2::` namespace prefix for ggplot2 functions to avoid conflicts

2. **Transformation Handling**
   - Support scale transformations consistently across functions
   - Use the `scales` package for transformation objects
   - The package provides custom transformations via `get_trans()`

3. **Plot Utilities**
   - Coordinate-based positioning should respect transformations
   - Text placement should remain consistent across different scales
   - Support common plotting themes (particularly cowplot)

## Common Tasks

### Adding a New Function

1. Create the function in the appropriate R/ file
2. Add roxygen2 documentation
3. Run `devtools::document()` to generate documentation
4. Add tests in tests/testthat/test-{function}.R
5. Update README.Rmd with examples if it's a major feature
6. Run `devtools::check()` to ensure R CMD check passes
7. Knit README.Rmd to update README.md if modified

### Modifying Existing Functions

1. Update the function code
2. Update roxygen2 documentation if signature or behavior changes
3. Update or add tests to cover the changes
4. Run `devtools::test()` to ensure tests pass
5. Run `devtools::check()` to ensure R CMD check passes
6. Update examples in README.Rmd if user-facing behavior changed

### Before Committing

Always run before committing code:
```r
# Check package
devtools::check()

# Ensure tests pass
devtools::test()

# Update documentation
devtools::document()
```

## Environment Setup

The repository uses `renv` for dependency management. When working locally (not in GitHub Actions):
- Dependencies are loaded via `renv/activate.R` (automatically sourced from `.Rprofile`)
- Run `renv::restore()` to install dependencies from `renv.lock`

In GitHub Actions/Copilot environments:
- Dependencies are managed via `r-lib/actions/setup-r-dependencies@v2`
- System dependencies are pre-installed in copilot-setup-steps.yml

## Notes for Copilot

- This is a plotting utility package, so changes should maintain backward compatibility with existing plots
- Visual consistency is important - test plot outputs when modifying plotting functions
- The package follows R package best practices and tidyverse style conventions
- Always maintain roxygen2 documentation in sync with code changes
- Test across edge cases for plotting (empty data, single points, transformations, etc.)
