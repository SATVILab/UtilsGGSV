# Contributing to UtilsGGSV

Thank you for considering a contribution to UtilsGGSV! The following guidelines
will help you get started quickly and ensure your contribution fits smoothly
into the project.

## Code of conduct

Please be respectful and constructive in all interactions. This project follows
the [Contributor Covenant Code of Conduct](https://www.contributor-covenant.org/).

## How to contribute

### Reporting bugs

Open an issue on [GitHub Issues](https://github.com/SATVILab/UtilsGGSV/issues)
and include:

- A minimal reproducible example (use `reprex::reprex()` if possible).
- The output of `sessionInfo()`.
- A clear description of the expected vs. actual behaviour.

### Suggesting enhancements

Open an issue labelled *enhancement* and describe:

- The problem you want to solve.
- A sketch of the proposed API (function name, parameters, return value).
- Why the enhancement belongs in this package rather than user code.

### Submitting a pull request

1. Fork the repository and create a feature branch from `main`:

   ```bash
   git checkout -b my-feature
   ```

2. Install development dependencies:

   ```r
   renv::restore()
   ```

3. Make your changes, following the style guide below.

4. Add or update tests in `tests/testthat/test-{function}.R`.

5. Run the full check locally and confirm it passes with no ERRORs, WARNINGs,
   or unexpected NOTEs:

   ```r
   devtools::document()
   devtools::test()
   devtools::check()
   ```

6. Push your branch and open a pull request against `main`.

## Style guide

- Follow the conventions described in `.github/instructions/` (R coding
  standards, testing guidelines, package development workflow).
- Use `snake_case` for function and variable names.
- Prefix internal (non-exported) helpers with `.` (e.g. `.my_helper()`).
- Document all exported functions with roxygen2. Every `@param`, `@return`,
  and `@examples` section is required.
- Do not leave trailing whitespace at the end of lines or on blank lines.
- Use `package::function()` notation rather than attaching packages with
  `library()` inside package code.

## Adding a new exported function

1. Create `R/{function_name}.R` with a complete roxygen2 header.
2. Run `devtools::document()` to regenerate `NAMESPACE` and `man/`.
3. Create `tests/testthat/test-{function_name}.R` with tests.
4. Update `_pkgdown.yml` to include the new function in the reference index.
5. Mention the addition in `NEWS.md` under `# UtilsGGSV (development version)`.

## Versioning

This package follows [Semantic Versioning](https://semver.org/). In brief:

| Change type | Version component bumped |
|---|---|
| Bug fixes and documentation only | patch (`x.y.Z`) |
| New backwards-compatible functionality | minor (`x.Y.0`) |
| Breaking changes | major (`X.0.0`) |

## Questions

If you are unsure about anything, open an issue or start a discussion on
[GitHub](https://github.com/SATVILab/UtilsGGSV/discussions).
