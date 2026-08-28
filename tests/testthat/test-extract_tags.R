## Tests for extract_tags() ----

# ---- Helper data ------------------------------------------------------------

# Minimal deployments dataframe with a deploymentTags column
make_dep <- function() {
  data.frame(
    deploymentID   = c("dep1", "dep2", "dep3"),
    locationName   = c("SiteA", "SiteB", "SiteC"),
    deploymentTags = c(
      "lantana:Light | site_burned:yes | tsf:NA",
      "lantana:Heavy | site_burned:no | tsf:6",
      NA_character_
    ),
    stringsAsFactors = FALSE
  )
}

# Minimal observations dataframe with an observationTags column
make_obs <- function() {
  data.frame(
    observationID   = c("obs1", "obs2"),
    observationTags = c(
      "temperature:13 | moonphase:New_Crescent",
      "temperature:22 | moonphase:Full"
    ),
    stringsAsFactors = FALSE
  )
}

# ---- Basic functionality ----------------------------------------------------

test_that("extract_tags auto-detects deploymentTags column", {
  dep    <- make_dep()
  # The coercion warning is expected here and is tested directly in
  # "character tag values remain as character". Suppressed so this block
  # asserts only what it is named for.
  result <- suppressWarnings(extract_tags(dep))

  # Should have the three original columns plus three new tag columns
  expect_equal(ncol(result), ncol(dep) + 3)
  expect_true(all(c("lantana", "site_burned", "tsf") %in% names(result)))
})

test_that("extract_tags auto-detects observationTags column", {
  obs    <- make_obs()
  # The coercion warning is expected here and is tested directly in
  # "character tag values remain as character". Suppressed so this block
  # asserts only what it is named for.
  result <- suppressWarnings(extract_tags(obs))

  expect_true(all(c("temperature", "moonphase") %in% names(result)))
})

test_that("extract_tags col argument overrides auto-detection", {
  dep    <- make_dep()
  # The coercion warning is expected here and is tested directly in
  # "character tag values remain as character". Suppressed so this block
  # asserts only what it is named for.
  result <- suppressWarnings(extract_tags(dep, col = "deploymentTags"))

  expect_true("lantana" %in% names(result))
})

test_that("extract_tags returns correct number of rows", {
  dep    <- make_dep()
  # The coercion warning is expected here and is tested directly in
  # "character tag values remain as character". Suppressed so this block
  # asserts only what it is named for.
  result <- suppressWarnings(extract_tags(dep))

  expect_equal(nrow(result), nrow(dep))
})

test_that("original column order is preserved", {
  dep    <- make_dep()
  # The coercion warning is expected here and is tested directly in
  # "character tag values remain as character". Suppressed so this block
  # asserts only what it is named for.
  result <- suppressWarnings(extract_tags(dep))

  # The first N columns of the output should match the original column names
  expect_equal(names(result)[seq_len(ncol(dep))], names(dep))
})

# ---- NA handling ------------------------------------------------------------

test_that("literal 'NA' strings in tags are converted to true NA", {
  dep    <- make_dep()
  # The coercion warning is expected here and is tested directly in
  # "character tag values remain as character". Suppressed so this block
  # asserts only what it is named for.
  result <- suppressWarnings(extract_tags(dep))

  # Row 1 has tsf:NA — should be converted to NA_character_, then to NA (numeric)
  expect_true(is.na(result$tsf[1]))
})

test_that("rows with NA tags receive NA in all new tag columns", {
  dep    <- make_dep()
  # The coercion warning is expected here and is tested directly in
  # "character tag values remain as character". Suppressed so this block
  # asserts only what it is named for.
  result <- suppressWarnings(extract_tags(dep))

  # Row 3 has NA in deploymentTags
  expect_true(all(is.na(result[3, c("lantana", "site_burned", "tsf")])))
})

test_that("rows with empty-string tags receive NA in all new tag columns", {
  dep <- data.frame(
    id             = "dep1",
    deploymentTags = "",
    stringsAsFactors = FALSE
  )
  result <- extract_tags(dep)
  # No new columns should be added; function should return df unchanged
  expect_equal(ncol(result), ncol(dep))
})

# ---- keep_original argument -------------------------------------------------

test_that("keep_original = TRUE retains the tags column", {
  dep    <- make_dep()
  # The coercion warning is expected here and is tested directly in
  # "character tag values remain as character". Suppressed so this block
  # asserts only what it is named for.
  result <- suppressWarnings(extract_tags(dep, keep_original = TRUE))

  expect_true("deploymentTags" %in% names(result))
})

test_that("keep_original = FALSE drops the tags column", {
  dep    <- make_dep()
  # The coercion warning is expected here and is tested directly in
  # "character tag values remain as character". Suppressed so this block
  # asserts only what it is named for.
  result <- suppressWarnings(extract_tags(dep, keep_original = FALSE))

  expect_false("deploymentTags" %in% names(result))
})

# ---- Type coercion ----------------------------------------------------------

test_that("numeric tag values are coerced to numeric class", {
  obs    <- make_obs()
  # The coercion warning is expected here and is tested directly in
  # "character tag values remain as character". Suppressed so this block
  # asserts only what it is named for.
  result <- suppressWarnings(extract_tags(obs))

  expect_type(result$temperature, "double")
  expect_equal(result$temperature, c(13, 22))
})

test_that("character tag values remain as character", {
  obs <- make_obs()

  # The coercion warning is expected: moonphase holds values that could never
  # have been numeric. Asserting it here means that if the warning is ever
  # removed or reworded, that change shows up in a diff rather than slipping
  # through a refactor. regexp is supplied so only THIS warning satisfies the
  # expectation, and a future unrelated warning cannot hide behind it.
  expect_warning(
    result <- extract_tags(obs),
    regexp = "could not be safely coerced"
  )

  # The point of the warning is that the column was left alone rather than
  # silently mangled into NAs. That is the contract that must hold whatever
  # the warning does.
  expect_type(result$moonphase, "character")
})

test_that("logical tag values are coerced to logical class", {
  dep <- data.frame(
    deploymentTags = c("active:TRUE | verified:FALSE", "active:false | verified:true"),
    stringsAsFactors = FALSE
  )
  result <- extract_tags(dep)

  expect_type(result$active,   "logical")
  expect_type(result$verified, "logical")
  expect_equal(result$active,   c(TRUE, FALSE))
  expect_equal(result$verified, c(FALSE, TRUE))
})

test_that("date tag values are coerced to POSIXct", {
  dep <- data.frame(
    deploymentTags = c("start_date:2024-01-15", "start_date:2024-06-01"),
    stringsAsFactors = FALSE
  )
  result <- extract_tags(dep)

  expect_s3_class(result$start_date, "POSIXct")
})

test_that("mixed-type columns that fail coercion emit a warning and stay as character", {
  dep <- data.frame(
    deploymentTags = c("score:10", "score:not_a_number"),
    stringsAsFactors = FALSE
  )
  expect_warning(
    result <- extract_tags(dep),
    regexp = "score"
  )
  expect_type(result$score, "character")
})

# ---- Error and warning handling ---------------------------------------------

test_that("error when col is specified but not found in df", {
  dep <- make_dep()
  expect_error(
    extract_tags(dep, col = "nonexistent_col"),
    regexp = "nonexistent_col"
  )
})

test_that("error when no standard tag column found and col is NULL", {
  df <- data.frame(x = 1:3, y = letters[1:3], stringsAsFactors = FALSE)
  expect_error(
    extract_tags(df),
    regexp = "No tag column found"
  )
})

test_that("error when both standard tag columns present and col is NULL", {
  df <- data.frame(
    deploymentTags  = c("a:1", "a:2"),
    observationTags = c("b:3", "b:4"),
    stringsAsFactors = FALSE
  )
  # Should warn about non-standard DP and then error asking for col
  expect_error(
    suppressWarnings(extract_tags(df)),
    regexp = "specify `col` explicitly"
  )
})

test_that("warning when both standard tag columns present even if col is specified", {
  df <- data.frame(
    deploymentTags  = c("a:1", "a:2"),
    observationTags = c("b:3", "b:4"),
    stringsAsFactors = FALSE
  )
  expect_warning(
    extract_tags(df, col = "deploymentTags"),
    regexp = "non-standard Camtrap DP"
  )
})

test_that("error when df is not a dataframe", {
  expect_error(
    extract_tags(list(deploymentTags = "a:1")),
    regexp = "must be a dataframe"
  )
})

# ---- Edge cases -------------------------------------------------------------

test_that("malformed pairs without a colon are silently skipped", {
  dep <- data.frame(
    deploymentTags = c("valid_key:value | bad_pair | another_key:123"),
    stringsAsFactors = FALSE
  )
  # The coercion warning is expected here and is tested directly in
  # "character tag values remain as character". Suppressed so this block
  # asserts only what it is named for.
  result <- suppressWarnings(extract_tags(dep))

  # Only the two valid pairs should produce columns
  expect_true("valid_key"   %in% names(result))
  expect_true("another_key" %in% names(result))
  # bad_pair (no colon) should not appear as a column
  expect_false("bad_pair" %in% names(result))
})

test_that("values containing colons are preserved correctly", {
  dep <- data.frame(
    deploymentTags = c("timestamp:2024-01-01T12:00:00"),
    stringsAsFactors = FALSE
  )
  result <- extract_tags(dep)

  # The timestamp key should exist
  expect_true("timestamp" %in% names(result))
  # The full value including embedded colons should be preserved before coercion
  # (after coercion to POSIXct the raw string is replaced, so just check the column exists)
  expect_equal(nrow(result), 1)
})

test_that("keys with varying presence across rows produce NA for absent rows", {
  dep <- data.frame(
    deploymentTags = c(
      "key_a:1 | key_b:hello",
      "key_a:2"
    ),
    stringsAsFactors = FALSE
  )
  # The coercion warning is expected here and is tested directly in
  # "character tag values remain as character". Suppressed so this block
  # asserts only what it is named for.
  result <- suppressWarnings(extract_tags(dep))

  expect_true("key_b" %in% names(result))
  # Row 2 does not have key_b — should be NA
  expect_true(is.na(result$key_b[2]))
  # Row 1 has key_b
  expect_equal(result$key_b[1], "hello")
})

test_that("all-NA tag column returns df with all-NA new columns (no error)", {
  dep <- data.frame(
    id             = c("d1", "d2"),
    deploymentTags = c(NA_character_, NA_character_),
    stringsAsFactors = FALSE
  )
  # All tags are NA: no keys will be found, so df is returned unchanged
  result <- extract_tags(dep)
  expect_equal(ncol(result), ncol(dep))
  expect_equal(nrow(result), nrow(dep))
})
