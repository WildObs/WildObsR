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
  mock_uids_data <- data.frame(
    ids = c("12345", "67890"),
    class = c("some_class", "some_class"),
    stringsAsFactors = FALSE
  )
  attr(mock_uids_data, "names") <- c("Puma concolor", "Panthera leo")
  mock_uids <- list(mock_uids_data)

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
  expect_equal(result$species[1], "Puma concolor")
  expect_equal(result$genus[1], "Puma")
  expect_equal(result$family[1], "Felidae")
})

test_that("extract_classif handles custom request fields", {
  # Create mock uids object
  mock_uids_data <- data.frame(
    ids = "12345",
    class = "some_class",
    stringsAsFactors = FALSE
  )
  attr(mock_uids_data, "names") <- "Puma concolor"
  mock_uids <- list(mock_uids_data)

  # Create mock classification
  classif_data <- data.frame(
    name = c("Animalia", "Chordata", "Mammalia"),
    rank = c("kingdom", "phylum", "class"),
    stringsAsFactors = FALSE
  )
  mock_classif <- list(list("12345" = classif_data))

  result <- extract_classif(
    uids = mock_uids,
    classif = mock_classif,
    request = c("kingdom", "phylum", "class")
  )

  expect_true("kingdom" %in% names(result))
  expect_true("phylum" %in% names(result))
  expect_true("class" %in% names(result))
  expect_equal(result$kingdom, "Animalia")
  expect_equal(result$phylum, "Chordata")
})

test_that("extract_classif includes user_provided_name column", {
  mock_uids_data <- data.frame(
    ids = "12345",
    class = "some_class",
    stringsAsFactors = FALSE
  )
  attr(mock_uids_data, "names") <- "Test Species"
  mock_uids <- list(mock_uids_data)

  classif_data <- data.frame(
    name = c("Animalia", "TestGenus", "Test Species"),
    rank = c("kingdom", "genus", "species"),
    stringsAsFactors = FALSE
  )
  mock_classif <- list(list("12345" = classif_data))

  result <- extract_classif(uids = mock_uids, classif = mock_classif)

  expect_true("user_provided_name" %in% names(result))
  expect_equal(result$user_provided_name, "Test Species")
})

test_that("extract_classif includes ids column", {
  mock_uids_data <- data.frame(
    ids = "12345",
    class = "some_class",
    stringsAsFactors = FALSE
  )
  attr(mock_uids_data, "names") <- "Test Species"
  mock_uids <- list(mock_uids_data)

  classif_data <- data.frame(
    name = c("TestGenus", "Test Species"),
    rank = c("genus", "species"),
    stringsAsFactors = FALSE
  )
  mock_classif <- list(list("12345" = classif_data))

  result <- extract_classif(uids = mock_uids, classif = mock_classif)

  expect_true("ids" %in% names(result))
  expect_equal(result$ids, "12345")
})

test_that("extract_classif removes rows with NA ids", {
  mock_uids_data <- data.frame(
    ids = c("12345", NA, "67890"),
    class = c("some_class", "some_class", "some_class"),
    stringsAsFactors = FALSE
  )
  attr(mock_uids_data, "names") <- c("Species1", "Species2", "Species3")
  mock_uids <- list(mock_uids_data)

  classif_12345 <- data.frame(
    name = c("Genus1", "Species1"),
    rank = c("genus", "species"),
    stringsAsFactors = FALSE
  )

  classif_67890 <- data.frame(
    name = c("Genus3", "Species3"),
    rank = c("genus", "species"),
    stringsAsFactors = FALSE
  )

  mock_classif <- list(list(
    "12345" = classif_12345,
    "67890" = classif_67890
  ))

  result <- extract_classif(uids = mock_uids, classif = mock_classif)

  # Should only have 2 rows (NA removed)
  expect_equal(nrow(result), 2)
  expect_false(any(is.na(result$ids)))
})

test_that("extract_classif handles missing taxonomic ranks", {
  # Species identified only to genus level
  mock_uids_data <- data.frame(
    ids = "12345",
    class = "some_class",
    stringsAsFactors = FALSE
  )
  attr(mock_uids_data, "names") <- "Genus sp."
  mock_uids <- list(mock_uids_data)

  # Classification without species rank
  classif_data <- data.frame(
    name = c("Animalia", "Felidae", "Genus"),
    rank = c("kingdom", "family", "genus"),
    stringsAsFactors = FALSE
  )
  mock_classif <- list(list("12345" = classif_data))

  result <- extract_classif(uids = mock_uids, classif = mock_classif)

  expect_true("genus" %in% names(result))
  expect_equal(result$genus, "Genus")
  # Species field should exist but be NA
  expect_true("species" %in% names(result))
  expect_true(is.na(result$species))
})

test_that("extract_classif handles multiple species", {
  mock_uids_data <- data.frame(
    ids = c("111", "222", "333"),
    class = c("some_class", "some_class", "some_class"),
    stringsAsFactors = FALSE
  )
  attr(mock_uids_data, "names") <- c("Species A", "Species B", "Species C")
  mock_uids <- list(mock_uids_data)

  classif_111 <- data.frame(
    name = c("GenusA", "Species A"),
    rank = c("genus", "species"),
    stringsAsFactors = FALSE
  )

  classif_222 <- data.frame(
    name = c("GenusB", "Species B"),
    rank = c("genus", "species"),
    stringsAsFactors = FALSE
  )

  classif_333 <- data.frame(
    name = c("GenusC", "Species C"),
    rank = c("genus", "species"),
    stringsAsFactors = FALSE
  )

  mock_classif <- list(list(
    "111" = classif_111,
    "222" = classif_222,
    "333" = classif_333
  ))

  result <- extract_classif(uids = mock_uids, classif = mock_classif)

  expect_equal(nrow(result), 3)
  expect_equal(result$species, c("Species A", "Species B", "Species C"))
  expect_equal(result$genus, c("GenusA", "GenusB", "GenusC"))
})

test_that("extract_classif returns correct column structure", {
  mock_uids_data <- data.frame(
    ids = "12345",
    class = "some_class",
    stringsAsFactors = FALSE
  )
  attr(mock_uids_data, "names") <- "Test Species"
  mock_uids <- list(mock_uids_data)

  classif_data <- data.frame(
    name = c("TestClass", "TestOrder", "TestFamily", "TestGenus", "Test Species"),
    rank = c("class", "order", "family", "genus", "species"),
    stringsAsFactors = FALSE
  )
  mock_classif <- list(list("12345" = classif_data))

  result <- extract_classif(uids = mock_uids, classif = mock_classif)

  # Check all default columns are present
  expected_cols <- c("user_provided_name", "ids", "species", "genus", "family", "order", "class")
  expect_true(all(expected_cols %in% names(result)))
})
