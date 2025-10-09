## Tests for verify_col_match() ----

test_that("verify_col_match detects perfect matches", {
  df1 <- data.frame(ID = c(1, 2, 3), Name = c("A", "B", "C"))
  df2 <- data.frame(ID = c(1, 2, 3), Value = c(10, 20, 30))

  # Capture printed output
  output <- capture.output(verify_col_match(df1, df2, "ID"))

  expect_true(any(grepl("All values from the column: ID match!", output)))
})

test_that("verify_col_match handles missing column error", {
  df1 <- data.frame(ID = c(1, 2, 3))
  df2 <- data.frame(Name = c("A", "B", "C"))

  expect_error(
    verify_col_match(df1, df2, "NonExistent"),
    "is not present in one or both dataframes"
  )
})

test_that("verify_col_match detects mismatches in first dataframe", {
  df1 <- data.frame(ID = c(1, 2, 3, 4, 5))
  df2 <- data.frame(ID = c(1, 2, 3))

  output <- capture.output(verify_col_match(df1, df2, "ID"))

  expect_true(any(grepl("first dataframe is greater than the second", output)))
  expect_true(any(grepl("4, 5", output)))
})

test_that("verify_col_match detects mismatches in second dataframe", {
  df1 <- data.frame(ID = c(1, 2, 3))
  df2 <- data.frame(ID = c(1, 2, 3, 4, 5))

  output <- capture.output(verify_col_match(df1, df2, "ID"))

  expect_true(any(grepl("second dataframe is greater than the first", output)))
  expect_true(any(grepl("4, 5", output)))
})

test_that("verify_col_match detects equal mismatches (potential typos)", {
  df1 <- data.frame(Species = c("Puma concolor", "Panthera leo", "Vulpes vulps"))
  df2 <- data.frame(Species = c("Puma concolor", "Panthera leo", "Vulpes vulpes"))

  output <- capture.output(verify_col_match(df1, df2, "Species"))

  # Function detects mismatches
  expect_true(any(grepl("mis-matched", output)))
  expect_true(any(grepl("Vulpes vulps", output)))
  expect_true(any(grepl("Vulpes vulpes", output)))
})

test_that("verify_col_match works with character columns", {
  df1 <- data.frame(Location = c("Site A", "Site B", "Site C"))
  df2 <- data.frame(Location = c("Site A", "Site B", "Site C"))

  output <- capture.output(verify_col_match(df1, df2, "Location"))

  expect_true(any(grepl("All values.*match", output)))
})

test_that("verify_col_match works with factor columns", {
  df1 <- data.frame(Category = factor(c("Low", "Medium", "High")))
  df2 <- data.frame(Category = factor(c("Low", "Medium", "High")))

  output <- capture.output(verify_col_match(df1, df2, "Category"))

  expect_true(any(grepl("All values.*match", output)))
})

test_that("verify_col_match handles tibbles correctly", {
  skip_if_not_installed("tibble")

  df1 <- tibble::tibble(ID = c(1, 2, 3))
  df2 <- tibble::tibble(ID = c(1, 2, 3))

  output <- capture.output(verify_col_match(df1, df2, "ID"))

  expect_true(any(grepl("All values.*match", output)))
})

test_that("verify_col_match handles empty dataframes", {
  df1 <- data.frame(ID = integer(0))
  df2 <- data.frame(ID = integer(0))

  output <- capture.output(verify_col_match(df1, df2, "ID"))

  expect_true(any(grepl("All values.*match", output)))
})

test_that("verify_col_match handles NA values in columns", {
  df1 <- data.frame(ID = c(1, 2, NA))
  df2 <- data.frame(ID = c(1, 2, NA))

  output <- capture.output(verify_col_match(df1, df2, "ID"))

  # Should match including NA
  expect_true(any(grepl("All values.*match", output)))
})

test_that("verify_col_match detects different NA patterns", {
  df1 <- data.frame(ID = c(1, 2, 3, NA))
  df2 <- data.frame(ID = c(1, 2, 3))

  output <- capture.output(verify_col_match(df1, df2, "ID"))

  # Should detect mismatch
  expect_true(any(grepl("mis-matched", output)))
})

test_that("verify_col_match works with numeric columns", {
  df1 <- data.frame(Value = c(1.5, 2.5, 3.5))
  df2 <- data.frame(Value = c(1.5, 2.5, 3.5))

  output <- capture.output(verify_col_match(df1, df2, "Value"))

  expect_true(any(grepl("All values.*match", output)))
})

test_that("verify_col_match detects partial overlaps", {
  df1 <- data.frame(ID = c(1, 2, 3, 4))
  df2 <- data.frame(ID = c(3, 4, 5, 6))

  output <- capture.output(verify_col_match(df1, df2, "ID"))

  expect_true(any(grepl("mis-matched", output)))
  # df1 has 1, 2 missing from df2
  expect_true(any(grepl("1, 2", output)))
  # df2 has 5, 6 missing from df1
  expect_true(any(grepl("5, 6", output)))
})

test_that("verify_col_match handles single row dataframes", {
  df1 <- data.frame(ID = 1)
  df2 <- data.frame(ID = 1)

  output <- capture.output(verify_col_match(df1, df2, "ID"))

  expect_true(any(grepl("All values.*match", output)))
})

test_that("verify_col_match case sensitive for character columns", {
  df1 <- data.frame(Name = c("alice", "bob"))
  df2 <- data.frame(Name = c("Alice", "Bob"))

  output <- capture.output(verify_col_match(df1, df2, "Name"))

  # Should detect case differences as mismatches
  expect_true(any(grepl("mis-matched", output)))
})
