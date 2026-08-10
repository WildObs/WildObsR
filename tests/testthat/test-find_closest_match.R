test_that("find_closest_match signals deprecation once per session", {
  # The warning is guarded by a once-per-session flag, so reset it first.
  .wildobsr_warned$find_closest_match <- NULL

  expect_warning(
    find_closest_match("Puma concolor", c("Puma concolor", "Panthera leo")),
    "deprecated"
  )

  # Second call in the same session must stay quiet.
  expect_silent(
    find_closest_match("Puma concolor", c("Puma concolor", "Panthera leo"))
  )
})

test_that("find_closest_match finds exact matches", {
  missing_species <- c("Puma concolor", "Panthera leo")
  verified_species <- c("Puma concolor", "Panthera leo", "Gorilla gorilla")

  result <- suppressWarnings(find_closest_match(missing_species, verified_species))

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2)
  expect_equal(result$original, missing_species)
  expect_equal(result$verified_match, c("Puma concolor", "Panthera leo"))
})

test_that("find_closest_match corrects typos", {
  # Test example from documentation
  missing_species <- c("Puma concorl", "Panthera leo", "Gorillla gorilla")
  verified_species <- c("Puma concolor", "Panthera leo", "Gorilla gorilla")

  result <- suppressWarnings(find_closest_match(missing_species, verified_species))

  expect_equal(result$original, missing_species)
  expect_equal(result$verified_match[1], "Puma concolor")  # Fixed typo
  expect_equal(result$verified_match[2], "Panthera leo")    # Exact match
  expect_equal(result$verified_match[3], "Gorilla gorilla") # Fixed extra 'l'
})

test_that("find_closest_match handles case differences", {
  missing_species <- c("puma concolor", "PANTHERA LEO")
  verified_species <- c("Puma concolor", "Panthera leo")

  result <- suppressWarnings(find_closest_match(missing_species, verified_species))

  # Should still match despite case differences (Jaro-Winkler is case-sensitive
  # but should have high similarity)
  expect_equal(nrow(result), 2)
  expect_equal(result$verified_match[1], "Puma concolor")
  expect_equal(result$verified_match[2], "Panthera leo")
})

test_that("find_closest_match handles single species", {
  missing_species <- "Vulpes vulpes"
  verified_species <- c("Vulpes vulpes", "Canis lupus", "Felis catus")

  result <- suppressWarnings(find_closest_match(missing_species, verified_species))

  expect_equal(nrow(result), 1)
  expect_equal(result$original, "Vulpes vulpes")
  expect_equal(result$verified_match, "Vulpes vulpes")
})

test_that("find_closest_match returns correct column names", {
  missing_species <- c("Test species")
  verified_species <- c("Test species")

  result <- suppressWarnings(find_closest_match(missing_species, verified_species))

  expect_true("original" %in% names(result))
  expect_true("verified_match" %in% names(result))
  expect_equal(ncol(result), 2)
})

test_that("find_closest_match handles multiple similar options", {
  missing_species <- "Macropus"
  verified_species <- c("Macropus rufus", "Macropus giganteus", "Macropus fuliginosus")

  result <- suppressWarnings(find_closest_match(missing_species, verified_species))

  expect_equal(nrow(result), 1)
  # Should match one of the Macropus species (whichever is closest)
  expect_true(result$verified_match %in% verified_species)
})

test_that("find_closest_match finds best match among multiple species", {
  missing_species <- "Wallabia bicolo"  # Typo in species name
  verified_species <- c("Wallabia bicolor", "Macropus rufus", "Vombatus ursinus")

  result <- suppressWarnings(find_closest_match(missing_species, verified_species))

  expect_equal(result$verified_match, "Wallabia bicolor")
})

test_that("find_closest_match handles spaces and formatting", {
  missing_species <- c("Canis  lupus", "Felis catus")  # Extra space
  verified_species <- c("Canis lupus", "Felis catus", "Vulpes vulpes")

  result <- suppressWarnings(find_closest_match(missing_species, verified_species))

  expect_equal(result$verified_match[1], "Canis lupus")
  expect_equal(result$verified_match[2], "Felis catus")
})

test_that("find_closest_match returns data frame with character columns", {
  missing_species <- c("Species A", "Species B")
  verified_species <- c("Species A", "Species B")

  result <- suppressWarnings(find_closest_match(missing_species, verified_species))

  # Check that result is a data frame with character columns
  expect_s3_class(result, "data.frame")
  expect_type(result$original, "character")
  expect_type(result$verified_match, "character")
})

test_that("find_closest_match handles Australian wildlife examples", {
  # Common Australian species with potential typos
  missing_species <- c("Macropus rufos", "Vombatus ursinus", "Tachyglossus aculeatu")
  verified_species <- c("Macropus rufus", "Vombatus ursinus", "Tachyglossus aculeatus",
                       "Ornithorhynchus anatinus", "Phascolarctos cinereus")

  result <- suppressWarnings(find_closest_match(missing_species, verified_species))

  expect_equal(nrow(result), 3)
  expect_equal(result$verified_match[1], "Macropus rufus")      # Fixed 'os' to 'us'
  expect_equal(result$verified_match[2], "Vombatus ursinus")    # Exact match
  expect_equal(result$verified_match[3], "Tachyglossus aculeatus")  # Fixed 'u' ending
})

test_that("find_closest_match preserves order of input", {
  missing_species <- c("Species C", "Species A", "Species B")
  verified_species <- c("Species A", "Species B", "Species C")

  result <- suppressWarnings(find_closest_match(missing_species, verified_species))

  expect_equal(result$original, missing_species)
  expect_equal(result$verified_match, c("Species C", "Species A", "Species B"))
})

test_that("find_closest_match handles common name typos", {
  # Testing with common names that might have typos
  missing_species <- c("Red Kangaro", "Koala Bear", "Echidna")
  verified_species <- c("Red Kangaroo", "Koala", "Short-beaked Echidna")

  result <- suppressWarnings(find_closest_match(missing_species, verified_species))

  expect_equal(nrow(result), 3)
  expect_equal(result$verified_match[1], "Red Kangaroo")
  # Note: "Koala Bear" is closer to "Koala" than other options
  expect_true(result$verified_match[2] %in% verified_species)
})
