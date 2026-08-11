## Tests for the runtime version check (issue #114) ----

## Almost everything here tests .compare_versions(), which is pure: it takes two
## strings and returns either NULL or a message. That is the whole reason the
## network call was split out into its own function, so these tests need no
## mocking, no fixtures, and no network.


## The installed copy is behind the release ----

test_that(".compare_versions returns a message when the installed version is behind", {
  msg <- .compare_versions(installed = "0.1.0", remote = "0.2.0")

  # something must be said
  expect_type(msg, "character")
  expect_length(msg, 1)

  # both versions belong in the message so the user can see the gap
  expect_true(grepl("0.1.0", msg, fixed = TRUE))
  expect_true(grepl("0.2.0", msg, fixed = TRUE))

  # and the one command that fixes it must be present verbatim
  expect_true(grepl('devtools::install_github("WildObs/WildObsR")', msg, fixed = TRUE))
})

test_that(".compare_versions compares numerically, not as text", {
  # "0.10.0" sorts BEFORE "0.9.0" as plain text, so a string comparison would
  # wrongly stay silent here. package_version() gets it right.
  expect_type(.compare_versions(installed = "0.9.0", remote = "0.10.0"), "character")

  # and the mirror image: 0.10.0 installed is ahead of 0.9.0 remote
  expect_null(.compare_versions(installed = "0.10.0", remote = "0.9.0"))
})

test_that(".compare_versions detects a major version gap", {
  expect_type(.compare_versions(installed = "0.2.0", remote = "1.0.0"), "character")
})


## The installed copy is level or ahead: stay silent ----

test_that(".compare_versions is silent when versions are exactly equal", {
  expect_null(.compare_versions(installed = "0.2.0", remote = "0.2.0"))
})

test_that(".compare_versions is silent when the installed version is ahead", {
  # a developer running an unreleased build should never be nagged
  expect_null(.compare_versions(installed = "0.3.0", remote = "0.2.0"))
  expect_null(.compare_versions(installed = "1.0.0", remote = "0.9.9"))
})


## Unusable input: stay silent rather than error ----

test_that(".compare_versions returns NULL when the remote version is NULL", {
  # NULL is what .fetch_remote_version() returns when the network check failed,
  # including while the repository is still private. That must be silent.
  expect_null(.compare_versions(installed = "0.1.0", remote = NULL))
})

test_that(".compare_versions returns NULL when the installed version is NULL", {
  expect_null(.compare_versions(installed = NULL, remote = "0.2.0"))
})

test_that(".compare_versions returns NULL for malformed version strings", {
  # none of these can be parsed by package_version(), and none should error
  expect_null(.compare_versions(installed = "0.1.0", remote = "not-a-version"))
  expect_null(.compare_versions(installed = "banana", remote = "0.2.0"))
  expect_null(.compare_versions(installed = "0.1.0", remote = ""))
  expect_null(.compare_versions(installed = "", remote = "0.2.0"))
})

test_that(".compare_versions returns NULL for NA and multi-element input", {
  expect_null(.compare_versions(installed = "0.1.0", remote = NA))
  expect_null(.compare_versions(installed = NA_character_, remote = "0.2.0"))

  # a vector of versions is meaningless here and must not be recycled
  expect_null(.compare_versions(installed = "0.1.0", remote = c("0.2.0", "0.3.0")))
  expect_null(.compare_versions(installed = character(0), remote = "0.2.0"))
})

test_that(".compare_versions never throws, whatever it is handed", {
  # the whole point is that a version check can never break a user's session
  expect_no_error(.compare_versions(installed = "0.1.0", remote = list(1, 2)))
  expect_no_error(.compare_versions(installed = TRUE, remote = "0.2.0"))
})


## The network function: one guarded test only ----

test_that(".fetch_remote_version returns a character string or NULL, never errors", {
  skip_if_offline()

  result <- .fetch_remote_version()

  # while the repository is private GitHub answers 404, so NULL is the expected
  # result today. Once it is public this becomes a version string. Both pass.
  expect_true(is.null(result) || is.character(result))
  if (is.character(result)) expect_length(result, 1)
})


## Reachability probe and the attach hook ----

test_that(".wildobs_api_reachable returns FALSE for an unreachable host", {
  # .invalid is reserved by RFC 2606 and can never resolve, so this exercises
  # the failure path deterministically without needing the network to be down
  expect_false(.wildobs_api_reachable("https://wildobs-does-not-exist.invalid/"))
})

test_that(".onAttach does not error when the database is unreachable", {
  # clear the once-per-session flag so the hook actually runs
  .wildobsr_warned$db_status <- NULL
  on.exit(.wildobsr_warned$db_status <- TRUE, add = TRUE)

  # attaching the package must succeed on an offline machine, so this must not
  # error whether or not the network happens to be available right now.
  # Reached via ::: because load hooks are not visible from the test environment
  # the way ordinary internal functions are.
  expect_no_error(WildObsR:::.onAttach("dummy_lib", "WildObsR"))
})

test_that(".check_wildobs_version only contacts GitHub once per session", {
  # pretend the check has already run
  .wildobsr_warned$version_check <- TRUE
  on.exit(.wildobsr_warned$version_check <- TRUE, add = TRUE)

  # with the flag set it must return immediately and silently, no network call
  expect_silent(.check_wildobs_version())
  expect_no_error(.check_wildobs_version())
})
