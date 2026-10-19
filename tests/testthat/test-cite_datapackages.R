## Tests for cite_datapackages() ----

## Runs offline on three kinds of package: a WildObs project from the fake API
## (helper-fake_api.R), the camtrapdp package's example (a non-WildObs Camtrap DP),
## and small synthetic descriptors for the edge cases.

## a minimal Camtrap DP descriptor with no citation, so one has to be built
bare_package <- function(...) {
  dp <- list(
    id = "test-package",
    title = "Camera traps in a test forest",
    created = "2021-03-04T10:00:00Z",
    profile = "https://raw.githubusercontent.com/tdwg/camtrap-dp/1.0.2/camtrap-dp-profile.json",
    version = "1.0.2",
    homepage = "https://example.org/test",
    contributors = list(
      list(title = "Ada Lovelace", role = "principalInvestigator"),
      list(title = "Test Agency", role = "rightsHolder"),
      list(title = "Charles Babbage", role = "contributor"),
      list(title = "Grace Hopper")
    ),
    licenses = list(list(name = "CC-BY-4.0", scope = "data"))
  )
  # let a test overwrite or add any key, whole
  args <- list(...)
  dp[names(args)] <- args
  return(dp)
} # end bare package


#
##
### WildObs packages ----

test_that("cite_datapackages keeps a WildObs citation word for word and adds the release", {
  fx <- local_fake_wildobs_api()
  dl <- suppressMessages(wildobs_dp_download(api_key = "fake", project_ids = fx$project,
                                             metadata_only = TRUE))

  out <- cite_datapackages(dl)

  # one reference, named by the project
  expect_named(out, fx$project)
  # the provider's citation is there in full, RAiD included
  expect_true(startsWith(out[[1]], dl[[1]]$bibliographicCitation))
  # followed by the database release it was downloaded from
  expect_match(out[[1]], sprintf("WildObs database version %s\\.$", dl[[1]]$versionControlWildObs))
  # and nothing is added when the version is not wanted
  expect_identical(cite_datapackages(dl, include_version = FALSE)[[1]],
                   sub("([^.])$", "\\1.", dl[[1]]$bibliographicCitation))
})

test_that("cite_datapackages reads authors, year and RAiD from a WildObs citation", {
  fx <- local_fake_wildobs_api()
  dl <- suppressMessages(wildobs_dp_download(api_key = "fake", project_ids = fx$project,
                                             metadata_only = TRUE))

  df <- cite_datapackages(dl, format = "data.frame")

  # every author the citation names, including one the contributors list as rights holder
  expect_identical(df$authors, "Zachary Amir; Tom Bruce; Matthew Luskin")
  expect_identical(df$year, "2022")
  # the RAiD is the identifier, and the link
  expect_match(df$identifier, "^https://raid\\.org/")
  expect_identical(df$url, df$identifier)
  expect_identical(df$licenses, "CC-BY-4.0")
})

test_that("cite_datapackages cites each covariate product once", {
  fx <- local_fake_wildobs_api()
  dl <- suppressMessages(wildobs_dp_download(api_key = "fake", project_ids = fx$project))

  df <- cite_datapackages(dl, format = "data.frame", covariates = TRUE)
  cov <- df[df$type == "covariate product", ]

  # the package comes first, then the products behind its covariates
  expect_identical(df$type[1], "data package")
  expect_gt(nrow(cov), 5)
  # each product is cited once, however many buffer scales use it
  expect_false(any(duplicated(cov$citation)))
  # the forest integrity index is among them
  expect_true(any(grepl("Grantham", cov$citation)))
})

test_that("cite_datapackages says why there are no covariate citations for metadata only", {
  fx <- local_fake_wildobs_api()
  dl <- suppressMessages(wildobs_dp_download(api_key = "fake", project_ids = fx$project,
                                             metadata_only = TRUE))

  # a metadata-only package has no covariates schema to read
  expect_message(out <- cite_datapackages(dl, covariates = TRUE), "no covariate sources found")
  # and still cites the package itself
  expect_length(out, 1)
})


#
##
### Any Camtrap DP package ----

test_that("cite_datapackages cites the camtrapdp example without adding its standard version", {
  skip_if_not_installed("camtrapdp", minimum_version = "0.5.0")
  x <- camtrapdp::example_dataset()

  out <- cite_datapackages(x)

  # a single package is accepted without wrapping it in a list
  expect_length(out, 1)
  # its own citation, which already states the version, is not given a second one
  expect_identical(unname(out), sub("([^.])$", "\\1.", x$bibliographicCitation))
  # the "Family I" authors are split into people for BibTeX
  bib <- cite_datapackages(x, format = "bibtex")
  expect_identical(bib$author[[1]]$family, "Desmet")
  expect_identical(bib$author[[1]]$given, "P")
  # and the link falls back to the homepage
  expect_identical(bib$url, x$homepage)
})

test_that("cite_datapackages builds a citation from the descriptor when there is none", {
  out <- cite_datapackages(bare_package())

  # credited people in order, the year created, the title and the link;
  # the rights holder is not an author, and someone with no role is
  expect_identical(unname(out), paste0(
    "Ada Lovelace, Charles Babbage, Grace Hopper (2021). Camera traps in a test forest. ",
    "https://example.org/test."))
})

test_that("cite_datapackages states a real package version but not the standard's", {
  # the version equals the Camtrap DP version in the profile, so it says nothing
  expect_false(grepl("Version", cite_datapackages(bare_package())))
  # a version of the data itself is stated
  expect_match(cite_datapackages(bare_package(version = "2.3")), "Version 2\\.3\\.$")
})

test_that("cite_datapackages adds the access date and checks it", {
  # a date string is accepted and appended
  expect_match(cite_datapackages(bare_package(), accessed = "2026-10-15"), "Accessed 2026-10-15\\.$")
  # as is a Date
  expect_match(cite_datapackages(bare_package(), accessed = as.Date("2026-01-02")), "Accessed 2026-01-02\\.$")
  # something that is not a date stops with a message saying what to give
  expect_error(cite_datapackages(bare_package(), accessed = "last week"), "could not read accessed")
})


#
##
### Ordering and formats ----

test_that("cite_datapackages sorts by author, by year, or not at all", {
  # three packages whose order differs by each rule
  dps <- list(
    bare_package(id = "b", contributors = list(list(title = "Bea Zed")), created = "2019-01-01"),
    bare_package(id = "a", contributors = list(list(title = "Al Young")), created = "2023-01-01"),
    bare_package(id = "c", contributors = list(list(title = "Cy Ames")), created = "2021-01-01")
  )

  # by the first author's family name: Ames, Young, Zed
  expect_named(cite_datapackages(dps), c("c", "a", "b"))
  expect_named(cite_datapackages(dps, sort_by = "year"), c("b", "c", "a"))
  expect_named(cite_datapackages(dps, sort_by = "none"), c("b", "a", "c"))
})

test_that("cite_datapackages returns BibTeX entries keyed by package id", {
  bib <- cite_datapackages(list(bare_package(id = "one"), bare_package(id = "two")),
                           format = "bibtex")

  # a bibentry with one entry per package
  expect_s3_class(bib, "bibentry")
  expect_length(bib, 2)
  expect_setequal(unlist(bib$key), c("one", "two"))
  # and it writes as BibTeX
  expect_true(any(grepl("^@Misc\\{one,", utils::toBibtex(bib))))
})


#
##
### Input checks ----

test_that("cite_datapackages stops on input that is not a data package", {
  # nothing at all
  expect_error(cite_datapackages(list()), "needs a data package or a list of them")
  # a list holding something else beside a package
  expect_error(cite_datapackages(list(bare_package(), list(1, 2))),
               "1 element\\(s\\) of dp_list that are not data packages \\(positions 2\\)")
  # a package with nothing to cite
  expect_error(cite_datapackages(list(list(id = "empty"))),
               "empty has neither a bibliographicCitation nor a title")
})
