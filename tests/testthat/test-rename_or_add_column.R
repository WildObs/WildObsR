## Tests for rename_or_add_column() ----

## A small, predictable data frame reused across the tests below. Two columns is
## enough to prove a rename hits the right one and leaves the other alone.
make_test_df <- function() {
  data.frame(lat = c(-27.5, -28.1),
             lon = c(153.0, 152.8),
             stringsAsFactors = FALSE)
}


## The normal path: an existing column gets a new name ----

test_that("rename_or_add_column renames an existing column", {
  df <- make_test_df()

  # remember: new name is the 2nd argument, old name is the 3rd
  out <- rename_or_add_column(df, "latitude", "lat")

  # the old name is gone and the new one has taken its place
  expect_true("latitude" %in% names(out))
  expect_false("lat" %in% names(out))

  # the untouched column is still there, in its original position
  expect_equal(names(out), c("latitude", "lon"))
})

test_that("rename_or_add_column preserves the renamed column's values", {
  df <- make_test_df()

  out <- rename_or_add_column(df, "latitude", "lat")

  # renaming must move the data across unchanged, not blank it out
  expect_equal(out$latitude, df$lat)

  # and it must never add or drop rows
  expect_equal(nrow(out), nrow(df))
})


## The add path: no old name supplied, so a new NA column appears ----

test_that("rename_or_add_column adds an NA column when old_name is an empty string", {
  df <- make_test_df()

  out <- rename_or_add_column(df, "coordinateUncertainty", "")

  # the new column is appended rather than replacing anything
  expect_equal(names(out), c("lat", "lon", "coordinateUncertainty"))

  # and it is entirely NA, one value per existing row
  expect_true(all(is.na(out$coordinateUncertainty)))
  expect_equal(length(out$coordinateUncertainty), nrow(df))
})

test_that("rename_or_add_column adds an NA column when old_name is NA", {
  df <- make_test_df()

  # NA is treated the same as an empty string: nothing to rename, so add
  out <- rename_or_add_column(df, "coordinateUncertainty", NA)

  expect_true("coordinateUncertainty" %in% names(out))
  expect_true(all(is.na(out$coordinateUncertainty)))
})

test_that("rename_or_add_column adds an NA column when old_name has length zero", {
  df <- make_test_df()

  # character(0) is the third way of saying "there is no old column"
  out <- rename_or_add_column(df, "coordinateUncertainty", character(0))

  expect_true("coordinateUncertainty" %in% names(out))
  expect_true(all(is.na(out$coordinateUncertainty)))
})


## The no-match path: old_name names a column that isn't there ----

test_that("rename_or_add_column leaves the data frame unchanged when old_name matches nothing", {
  df <- make_test_df()

  # "elevation" is not a column, so there is nothing to rename
  out <- rename_or_add_column(df, "latitude", "elevation")

  # nothing is renamed, and crucially nothing is added either
  expect_equal(names(out), names(df))
  expect_equal(out, df)

  # the requested new name does NOT appear; this is the add path's opposite
  expect_false("latitude" %in% names(out))
})


## The collision path: new_name already belongs to a different column ----

test_that("rename_or_add_column produces duplicate names when new_name already exists", {
  df <- make_test_df()

  # renaming "lat" to "lon" when "lon" already exists does not error or merge
  out <- rename_or_add_column(df, "lon", "lat")

  # instead both columns end up sharing the same name, which callers must expect
  expect_equal(names(out), c("lon", "lon"))
  expect_true(anyDuplicated(names(out)) > 0)

  # the column count is unchanged, so no data was dropped in the collision
  expect_equal(ncol(out), ncol(df))
})

test_that("rename_or_add_column overwrites an existing column on the add path", {
  df <- make_test_df()

  # here new_name matches an existing column AND we take the add branch,
  # so the existing "lon" values are replaced with NA rather than duplicated
  out <- rename_or_add_column(df, "lon", "")

  expect_equal(names(out), c("lat", "lon"))
  expect_true(all(is.na(out$lon)))
})
