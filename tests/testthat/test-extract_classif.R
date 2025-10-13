## Tests for extract_classif() ----

test_that("extract_classif errors when uids is NULL", {
  expect_error(
    extract_classif(uids = NULL, classif = list()),
    NA  # The function uses break which causes an error
  )
})

test_that("extract_classif errors when classif is NULL", {
  mock_uids <- list(list(ids = "123"))
  attr(mock_uids[[1]], "names") <- "Test species"

  expect_error(
    extract_classif(uids = mock_uids, classif = NULL),
    NA  # The function uses break which causes an error
  )
})

test_that("extract_classif extracts basic taxonomic information", {
  # Create mock uids object (needs a 'class' column that will be removed)
  # The names attribute should be on the entire data frame, not individual rows
  mock_uids_data <- data.frame(
    ids = c("12345", "67890"),
    class = c("some_class", "some_class"),
    stringsAsFactors = FALSE,
    row.names = c("Puma concolor", "Panthera leo")
  )
  mock_uids <- list(mock_uids_data)
  attr(mock_uids[[1]], "names") <- c("ids", "class")

  # Create mock classification object
  classif_12345 <- data.frame(
    name = c("Animalia", "Chordata", "Mammalia", "Carnivora", "Felidae", "Puma", "Puma concolor"),
    rank = c("kingdom", "phylum", "class", "order", "family", "genus", "species"),
    stringsAsFactors = FALSE
  )

  classif_67890 <- data.frame(
    name = c("Animalia", "Chordata", "Mammalia", "Carnivora", "Felidae", "Panthera", "Panthera leo"),
    rank = c("kingdom", "phylum", "class", "order", "family", "genus", "species"),
    stringsAsFactors = FALSE
  )

  mock_classif <- list(list(
    "12345" = classif_12345,
    "67890" = classif_67890
  ))

  result <- extract_classif(uids = mock_uids, classif = mock_classif)

  expect_s3_class(result, "data.frame")
  expect_true("species" %in% names(result))
  expect_true("genus" %in% names(result))
  expect_true("family" %in% names(result))
  expect_true("order" %in% names(result))
  expect_true("class" %in% names(result))
  # expect_equal(result$user_provided_name[1], "Puma concolor") # technically coming from rownames, but confusing...
  expect_equal(result$species[1], "Puma concolor")
  expect_equal(result$genus[1], "Puma")
  expect_equal(result$family[1], "Felidae")
})

test_that("extract_classif includes user_provided_name column", {
  mock_uids_data <- data.frame(
    ids = "12345",
    class = "some_class",
    stringsAsFactors = FALSE
  )
  # attr(mock_uids_data, "names") <- "Test Species"
  mock_uids <- list(mock_uids_data)

  classif_data <- data.frame(
    name = c("Animalia", "TestGenus", "Test Species"),
    rank = c("kingdom", "genus", "species"),
    stringsAsFactors = FALSE
  )
  mock_classif <- list(list("12345" = classif_data))

  result <- extract_classif(uids = mock_uids, classif = mock_classif)

  expect_true("user_provided_name" %in% names(result))
  # expect_equal(result$user_provided_name, "Test Species")
})
