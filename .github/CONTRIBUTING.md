# Contributing to WildObsR

Thank you for helping improve WildObsR. This page explains how to report a problem, ask for a
tutorial, and propose a change.

## Report a problem or ask for a tutorial

Open an [issue](https://github.com/WildObs/WildObsR/issues/new/choose) and pick a template:

- **Bug report**, when a function does not do what its help page says. Include the shortest
  code that shows the problem. If you can, use `example_datapackages()`, so others can run it
  without an API key. Never paste your API key into an issue.
- **Lesson or tutorial request**, when you want to run an analysis the articles do not cover
  yet.

Questions about a project's data or metadata, or corrections to them, go to the WildObs team
at <support@wildobs.org.au>, since the database is maintained separately from this package.

## Propose a change

1. Open an issue first for anything beyond a small fix, so we can agree on the approach.
2. Fork the repository and create a branch from `main`.
3. Make the change, with tests in `tests/testthat/` for any new behaviour.
4. Run `devtools::document()`, `devtools::test()` and `devtools::check()`. A pull request
   should leave the check with no errors and no warnings.
5. Add a line to `NEWS.md` under the development version, saying what changed for users.
6. Open a pull request that describes what the change does and why.

Tests that query the live WildObs database run only when a `WILDOBSR_API_KEY` is set, and
never in CI. Everything else runs offline, against the bundled example data and a recorded
slice of the public API (`tests/testthat/fixtures/`).

## Code conventions

- Use the native pipe `|>` and `<-` for assignment.
- Build file paths with `here::here()`.
- Comment generously and in plain English: say what each step does to the data and why, not
  what the R syntax is. Put a short comment above each step of a pipe chain.
- Write error messages that say what went wrong and what to do about it.
- Document every exported function with roxygen2, including `@examples` that run, preferably
  on `example_datapackages()`.

## Vignettes

Vignettes that query WildObs are written as `vignettes/<name>.Rmd.orig` and knitted with
`Rscript vignettes/precompute.R <name>`, using your own API key. Commit both the `.Rmd.orig`
and the knitted `.Rmd`. Results must come only from the public API.

## Code of conduct

Be respectful and constructive. We aim for a welcoming community of ecologists and
developers at every level of experience.
