## Tests for update_common_to_binomial() ----

test_that("update_common_to_binomial replaces common name with binomial", {
  data_caps <- data.frame(
    scientificName = c("Red Kangaroo", "Red Kangaroo", "Koala"),
    observation = c(1, 2, 3),
    stringsAsFactors = FALSE
  )

  data_verified <- data.frame(
    user_provided_name = c("Macropus rufus", "Phascolarctos cinereus"),
    Common_name = c("Red Kangaroo", "Koala"),
    binomial_verified = c("Macropus rufus", "Phascolarctos cinereus"),
    stringsAsFactors = FALSE
  )

  result <- update_common_to_binomial(data_caps, data_verified, interactive = FALSE)

  expect_equal(result$scientificName[1], "Macropus rufus")
  expect_equal(result$scientificName[2], "Macropus rufus")
  expect_equal(result$scientificName[3], "Phascolarctos cinereus")
})

test_that("update_common_to_binomial handles case insensitivity", {
  data_caps <- data.frame(
    scientificName = c("RED KANGAROO", "red kangaroo"),
    stringsAsFactors = FALSE
  )

  data_verified <- data.frame(
    user_provided_name = "Macropus rufus",
    Common_name = "Red Kangaroo",
    binomial_verified = "Macropus rufus",
    stringsAsFactors = FALSE
  )

  result <- update_common_to_binomial(data_caps, data_verified, interactive = FALSE)

  expect_equal(result$scientificName[1], "Macropus rufus")
  expect_equal(result$scientificName[2], "Macropus rufus")
})

test_that("update_common_to_binomial skips species with multiple binomial matches", {
  data_caps <- data.frame(
    scientificName = "Possum",
    stringsAsFactors = FALSE
  )

  data_verified <- data.frame(
    user_provided_name = c("Species1", "Species2"),
    Common_name = c("Possum", "Possum"),
    binomial_verified = c("Trichosurus vulpecula", "Pseudocheirus peregrinus"),
    stringsAsFactors = FALSE
  )

  # Capture output to check warning message
  output <- capture.output(
    result <- update_common_to_binomial(data_caps, data_verified, interactive = FALSE)
  )

  # Should not replace when multiple matches exist
  expect_equal(result$scientificName, "Possum")
  expect_true(any(grepl("more than one unique value", output)))
})

test_that("update_common_to_binomial handles Species column name", {
  data_caps <- data.frame(
    Species = c("Red Kangaroo"),
    stringsAsFactors = FALSE
  )

  data_verified <- data.frame(
    user_provided_name = "Macropus rufus",
    Common_name = "Red Kangaroo",
    binomial_verified = "Macropus rufus",
    stringsAsFactors = FALSE
  )

  result <- update_common_to_binomial(data_caps, data_verified, interactive = FALSE)

  # Should rename back to Species
  expect_true("Species" %in% names(result))
  expect_false("scientificName" %in% names(result))
  expect_equal(result$Species, "Macropus rufus")
})

test_that("update_common_to_binomial handles deployment_id column name", {
  data_caps <- data.frame(
    deployment_id = "dep1",
    scientificName = "Red Kangaroo",
    stringsAsFactors = FALSE
  )

  data_verified <- data.frame(
    user_provided_name = "Macropus rufus",
    Common_name = "Red Kangaroo",
    binomial_verified = "Macropus rufus",
    stringsAsFactors = FALSE
  )

  result <- update_common_to_binomial(data_caps, data_verified, interactive = FALSE)

  # Should rename back to deployment_id
  expect_true("deployment_id" %in% names(result))
  expect_false("deploymentID" %in% names(result))
})

test_that("update_common_to_binomial prints message for no matches", {
  data_caps <- data.frame(
    scientificName = "Unknown Animal",
    stringsAsFactors = FALSE
  )

  data_verified <- data.frame(
    user_provided_name = "Macropus rufus",
    Common_name = "Red Kangaroo",
    binomial_verified = "Macropus rufus",
    stringsAsFactors = FALSE
  )

  output <- capture.output(
    result <- update_common_to_binomial(data_caps, data_verified, interactive = FALSE)
  )

  expect_true(any(grepl("No matching Common_name found for", output)))
  expect_equal(result$scientificName, "Unknown Animal")
})

test_that("update_common_to_binomial preserves already verified species", {
  data_caps <- data.frame(
    scientificName = c("Macropus rufus", "Red Kangaroo"),
    stringsAsFactors = FALSE
  )

  data_verified <- data.frame(
    user_provided_name = "Macropus rufus",
    Common_name = "Red Kangaroo",
    binomial_verified = "Macropus rufus",
    stringsAsFactors = FALSE
  )

  result <- update_common_to_binomial(data_caps, data_verified, interactive = FALSE)

  # First row already verified, should remain unchanged
  expect_equal(result$scientificName[1], "Macropus rufus")
  # Second row should be updated
  expect_equal(result$scientificName[2], "Macropus rufus")
})

test_that("update_common_to_binomial preserves other columns", {
  data_caps <- data.frame(
    scientificName = "Red Kangaroo",
    location = "Site A",
    count = 5,
    stringsAsFactors = FALSE
  )

  data_verified <- data.frame(
    user_provided_name = "Macropus rufus",
    Common_name = "Red Kangaroo",
    binomial_verified = "Macropus rufus",
    stringsAsFactors = FALSE
  )

  result <- update_common_to_binomial(data_caps, data_verified, interactive = FALSE)

  expect_equal(result$location, "Site A")
  expect_equal(result$count, 5)
  expect_equal(ncol(result), ncol(data_caps))
})

test_that("update_common_to_binomial returns data frame", {
  data_caps <- data.frame(
    scientificName = "Red Kangaroo",
    stringsAsFactors = FALSE
  )

  data_verified <- data.frame(
    user_provided_name = "Macropus rufus",
    Common_name = "Red Kangaroo",
    binomial_verified = "Macropus rufus",
    stringsAsFactors = FALSE
  )

  result <- update_common_to_binomial(data_caps, data_verified, interactive = FALSE)

  expect_s3_class(result, "data.frame")
})

test_that("update_common_to_binomial handles empty data_caps", {
  data_caps <- data.frame(
    scientificName = character(0),
    stringsAsFactors = FALSE
  )

  data_verified <- data.frame(
    user_provided_name = "Macropus rufus",
    Common_name = "Red Kangaroo",
    binomial_verified = "Macropus rufus",
    stringsAsFactors = FALSE
  )

  result <- update_common_to_binomial(data_caps, data_verified, interactive = FALSE)

  expect_equal(nrow(result), 0)
  expect_s3_class(result, "data.frame")
})

test_that("update_common_to_binomial handles multiple different species", {
  data_caps <- data.frame(
    scientificName = c("Red Kangaroo", "Koala", "Wombat"),
    stringsAsFactors = FALSE
  )

  data_verified <- data.frame(
    user_provided_name = c("Macropus rufus", "Phascolarctos cinereus", "Vombatus ursinus"),
    Common_name = c("Red Kangaroo", "Koala", "Wombat"),
    binomial_verified = c("Macropus rufus", "Phascolarctos cinereus", "Vombatus ursinus"),
    stringsAsFactors = FALSE
  )

  result <- update_common_to_binomial(data_caps, data_verified, interactive = FALSE)

  expect_equal(result$scientificName[1], "Macropus rufus")
  expect_equal(result$scientificName[2], "Phascolarctos cinereus")
  expect_equal(result$scientificName[3], "Vombatus ursinus")
})

test_that("update_common_to_binomial handles mixed verified and common names", {
  data_caps <- data.frame(
    scientificName = c("Macropus rufus", "Red Kangaroo", "Phascolarctos cinereus"),
    stringsAsFactors = FALSE
  )

  data_verified <- data.frame(
    user_provided_name = c("Macropus rufus", "Phascolarctos cinereus"),
    Common_name = c("Red Kangaroo", "Koala"),
    binomial_verified = c("Macropus rufus", "Phascolarctos cinereus"),
    stringsAsFactors = FALSE
  )

  result <- update_common_to_binomial(data_caps, data_verified, interactive = FALSE)

  # All should be binomial names
  expect_equal(result$scientificName[1], "Macropus rufus")
  expect_equal(result$scientificName[2], "Macropus rufus")
  expect_equal(result$scientificName[3], "Phascolarctos cinereus")
})

test_that("update_common_to_binomial defaults interactive to FALSE", {
  data_caps <- data.frame(
    scientificName = "Red Kangaroo",
    stringsAsFactors = FALSE
  )

  data_verified <- data.frame(
    user_provided_name = "Macropus rufus",
    Common_name = "Red Kangaroo",
    binomial_verified = "Macropus rufus",
    stringsAsFactors = FALSE
  )

  # Should work without specifying interactive parameter
  result <- update_common_to_binomial(data_caps, data_verified)

  expect_equal(result$scientificName, "Macropus rufus")
})

test_that("update_common_to_binomial handles NA in Common_name", {
  data_caps <- data.frame(
    scientificName = "Red Kangaroo",
    stringsAsFactors = FALSE
  )

  data_verified <- data.frame(
    user_provided_name = c("Macropus rufus", "Other species"),
    Common_name = c("Red Kangaroo", NA),
    binomial_verified = c("Macropus rufus", "Other species"),
    stringsAsFactors = FALSE
  )

  result <- update_common_to_binomial(data_caps, data_verified, interactive = FALSE)

  expect_equal(result$scientificName, "Macropus rufus")
})

test_that("update_common_to_binomial preserves row order", {
  data_caps <- data.frame(
    scientificName = c("Wombat", "Red Kangaroo", "Koala"),
    obs_id = c(3, 1, 2),
    stringsAsFactors = FALSE
  )

  data_verified <- data.frame(
    user_provided_name = c("Macropus rufus", "Phascolarctos cinereus", "Vombatus ursinus"),
    Common_name = c("Red Kangaroo", "Koala", "Wombat"),
    binomial_verified = c("Macropus rufus", "Phascolarctos cinereus", "Vombatus ursinus"),
    stringsAsFactors = FALSE
  )

  result <- update_common_to_binomial(data_caps, data_verified, interactive = FALSE)

  # Row order should be preserved
  expect_equal(result$obs_id, c(3, 1, 2))
  expect_equal(result$scientificName[1], "Vombatus ursinus")
  expect_equal(result$scientificName[2], "Macropus rufus")
  expect_equal(result$scientificName[3], "Phascolarctos cinereus")
})
