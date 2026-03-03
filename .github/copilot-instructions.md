# Copilot Instructions for UtilsGGSV

R package providing utility functions for `ggplot2` plotting.

## Package Overview

- **Type**: R Package
- **Purpose**: ggplot2 plotting utilities
- **Dependencies**: ggplot2, cowplot, scales, dplyr, tibble, purrr
- **Tests**: testthat (edition 3)

## Repository Structure

- `R/` - Source code (exported functions, `.` prefix for internal)
- `man/` - Auto-generated docs (DO NOT edit directly)
- `tests/testthat/` - Tests (use `test-{function}.R` naming)
- `vignettes/` - Package vignettes

## Key Functions

### Cluster-Specific Plots

- `plot_cluster_heatmap` - Heat map of ECDF-standardised variable values per
  cluster. Tiles show the percentile of a cluster's median value compared
  against observations outside the cluster.
- `plot_clust_density` - Density plot for each variable with per-cluster median
  lines. Plots the overall density and overlays a coloured vertical line for
  each cluster at its median.

### Other Plotting Utilities

- `ggcorr` - Scatter plots with correlation coefficients overlaid
- `axis_limits` - Force equal axis limits or expand axis coordinates
- `add_text_column` - Add a column of text annotations to a plot
- `save_plot` / `ggsave2` - Save plots to disk
- `get_trans` - Return `scales` transformation objects by name

## Code Quality

- Make minimal, surgical changes to fix issues
- Maintain backward compatibility when possible
- Follow existing patterns in the codebase
- Add tests for new functionality or bug fixes
- Never leave trailing whitespace at the end of lines or on blank lines
- Always add a blank line between headings and bullet points

## Before Committing

```r
devtools::document()  # Update documentation
devtools::test()      # Run tests
devtools::check()     # Full package check
```

**IMPORTANT**: If adding or removing exported functions, update `_pkgdown.yml` reference index to include all exported functions.

## Topic-Specific Instructions

See `.github/instructions/` for detailed guidelines:

- `r-coding-standards.instructions.md` - R code style and documentation
- `testing.instructions.md` - Test patterns and testthat usage
- `package-development.instructions.md` - Development workflow

## Maintaining These Instructions

When updating copilot instructions:

- Keep files under 250 lines (max 1000)
- Use headings, bullets, clear sections
- Use short, imperative rules over long paragraphs
- Include code examples (correct and incorrect patterns)
- No external links - copy info instead
- No vague language - avoid "be more accurate", "identify all issues"
- Use `applyTo` frontmatter in topic files for path-specific rules
