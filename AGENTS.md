# Repository Guidelines

## Project Structure & Module Organization

This repository is an R package targeting R 4.1 or newer. Put package code in `R/`, grouped
by topic (for example, `R/sas.R` and `R/ibge_pnadc.R`). Tests live in `tests/testthat/` and
follow the source topics; snapshot expectations are stored in `tests/testthat/_snaps/`.
Roxygen2 generates `man/*.Rd` and `NAMESPACE`, so edit documentation comments in `R/`
instead of generated files. Small package fixtures belong in `inst/extdata/`. The `dev/`
directory contains exploratory scripts, benchmarks, and large work-in-progress artifacts;
it is excluded from package builds. Edit `README.Rmd`, then regenerate `README.md`.

## Build, Test, and Development Commands

- `Rscript -e "devtools::install_deps(dependencies = TRUE)"` installs development dependencies.
- `Rscript -e "devtools::load_all()"` loads the package for an interactive development cycle.
- `Rscript -e "devtools::test()"` runs the complete parallel `testthat` suite.
- `Rscript -e "devtools::test(filter = 'sas')"` runs matching test files while iterating.
- `Rscript -e "devtools::document()"` refreshes `NAMESPACE` and `man/` after roxygen changes.
- `Rscript -e "devtools::check()"` performs the full package build and R CMD check.
- `air format .` formats R sources using the repository's `air.toml` settings.

## Coding Style & Naming Conventions

Use two-space indentation, a 100-character line width, `<-` for assignment, and the base
pipe `|>`. Name functions and objects in `snake_case`; keep related public and internal
helpers in the same topical source file. Qualify external calls (`dplyr::mutate`) unless an
import is deliberately declared. Document exported functions with roxygen2 and include
parameters, return values, examples, and `@export`. Add user-visible changes to `NEWS.md`.

## Testing Guidelines

Use `testthat` edition 3. Name files `test-<topic>.R` and write behavior-focused
`test_that()` descriptions. Place new tests beside similar coverage. Prefer precise
expectations; capture errors and warnings with `expect_snapshot()` and review changed
snapshots before committing. No numeric coverage threshold is configured, but every bug fix
or behavior change should include a regression test.

## Commit & Pull Request Guidelines

Recent commits use short, imperative summaries, usually in Portuguese (for example,
`Atualiza ...`, `Remove ...`, or `Corrige ...`). Keep each commit focused and describe the
user-visible outcome. Pull requests should summarize the change, explain motivation, link
relevant issues, and report `devtools::test()` and `devtools::check()` results. Include
before/after output or benchmark results when behavior or performance changes; screenshots
are only needed for rendered documentation changes.
