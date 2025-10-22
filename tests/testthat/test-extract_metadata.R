## Tests for extract_metadata() ----

# Helper function to create minimal test data package
create_test_dp <- function(id = "test_dp_001") {
  list(
    id = id,
    contributors = list(
      list(title = "John Doe", email = "john@example.com", role = "author"),
      list(title = "Jane Smith", email = "jane@example.com", role = "contributor")
    ),
    sources = list(
      title = "Test Data Source",
      path = "https://example.com/data"
    ),
    licenses = list(
      name = "CC-BY-4.0",
      scope = "data",
      path = "https://creativecommons.org/licenses/by/4.0/"
    ),
    relatedIdentifiers = list(
      list(relationType = "IsDocumentedBy", relatedIdentifier = "doi:10.1234/example"),
      list(relationType = "IsCitedBy", relatedIdentifier = "doi:10.5678/paper")
    ),
    references = c(
      "Author et al. (2023) Title. Journal 1:1-10",
      "Smith et al. (2024) Another Title. Journal 2:20-30"
    ),
    project = list(
      id = "project_001",
      title = "Test Camera Trap Project",
      samplingDesign = "targeted detection"
    ),
    WildObsMetadata = list(
      tabularSharingPreference = "open",
      metadataContact = "contact@example.com",
      embargoMonths = 0
    ),
    spatial = list(
      type = "polygon",
      bbox = list(xmin = 145.0, xmax = 146.0, ymin = -28.0, ymax = -27.0)
    ),
    temporal = list(
      study_period = list(
        timeZone = "Australia/Brisbane",
        start = "2023-01-01",
        end = "2023-12-31"
      )
    ),
    taxonomic = list(
      list(
        scientificName = "Macropus rufus",
        vernacularNames = list(en = "Red Kangaroo"),
        taxonRank = "species"
      ),
      list(
        scientificName = "Sus scrofa",
        vernacularNames = list(en = "Wild Pig"),
        taxonRank = "species"
      )
    )
  )
}

## Input validation tests ----

test_that("extract_metadata errors when no supported elements provided", {
  dp <- create_test_dp()

  expect_error(
    extract_metadata(dp, elements = c("unsupported", "invalid")),
    "You have not provided metadata elements that match this functions supported elements"
  )
})

test_that("extract_metadata accepts all supported elements", {
  dp <- create_test_dp()
  supported <- c("contributors", "sources", "licenses", "relatedIdentifiers",
                 "references", "project", "WildObsMetadata", "spatial", "temporal", "taxonomic")

  # Should not error
  expect_no_error(extract_metadata(dp, elements = supported))
})

## Single data package tests ----

test_that("extract_metadata returns data frame for single element request", {
  dp <- create_test_dp()

  result <- extract_metadata(dp, elements = "contributors")

  expect_s3_class(result, "data.frame")
  expect_true("DPID" %in% names(result))
})

test_that("extract_metadata returns named list for multiple elements", {
  dp <- create_test_dp()

  result <- extract_metadata(dp, elements = c("contributors", "sources"))

  expect_type(result, "list")
  expect_named(result, c("contributors", "sources"))
  expect_s3_class(result$contributors, "data.frame")
  expect_s3_class(result$sources, "data.frame")
})

test_that("extract_metadata includes DPID column", {
  dp <- create_test_dp()

  result <- extract_metadata(dp, elements = "contributors")

  expect_true("DPID" %in% names(result))
  expect_equal(result$DPID[1], "test_dp_001")
})

## List of objects vs flat object handling ----

test_that("extract_metadata handles list of objects (contributors)", {
  dp <- create_test_dp()

  result <- extract_metadata(dp, elements = "contributors")

  # Should have one row per contributor
  expect_equal(nrow(result), 2)
  expect_true("title" %in% names(result))
  expect_true("email" %in% names(result))
  expect_true("role" %in% names(result))
})

test_that("extract_metadata handles flat object (sources)", {
  dp <- create_test_dp()

  result <- extract_metadata(dp, elements = "sources")

  # Should have one row for flat object
  expect_equal(nrow(result), 1)
  expect_true("title" %in% names(result))
  expect_true("path" %in% names(result))
})

test_that("extract_metadata handles NULL replacement with NA", {
  dp <- create_test_dp()
  # Add contributor with NULL value
  dp$contributors[[3]] <- list(title = "Test User", email = NULL, role = "tester")

  result <- extract_metadata(dp, elements = "contributors")

  # NULL should be replaced with NA
  expect_equal(nrow(result), 3)
  expect_true(is.na(result$email[3]))
})

## Multiple data packages tests ----

test_that("extract_metadata handles multiple data packages", {
  dp1 <- create_test_dp(id = "dp_001")
  dp2 <- create_test_dp(id = "dp_002")
  dp_list <- list(dp1, dp2)

  result <- extract_metadata(dp_list, elements = "contributors")

  # Should combine rows from both DPs
  expect_equal(nrow(result), 4) # 2 contributors per DP
  expect_true("dp_001" %in% result$DPID)
  expect_true("dp_002" %in% result$DPID)
})

test_that("extract_metadata handles list wrapping of single DP", {
  dp <- create_test_dp()

  # Function should handle both dp and list(dp) the same way
  result1 <- extract_metadata(dp, elements = "contributors")
  result2 <- extract_metadata(list(dp), elements = "contributors")

  expect_equal(nrow(result1), nrow(result2))
  expect_equal(names(result1), names(result2))
})

test_that("extract_metadata accumulates results across multiple DPs", {
  dp1 <- create_test_dp(id = "dp_001")
  dp2 <- create_test_dp(id = "dp_002")
  dp_list <- list(dp1, dp2)

  result <- extract_metadata(dp_list, elements = c("contributors", "sources"))

  expect_type(result, "list")
  expect_named(result, c("contributors", "sources"))

  # Contributors should have 4 rows (2 per DP)
  expect_equal(nrow(result$contributors), 4)

  # Sources should have 2 rows (1 per DP)
  expect_equal(nrow(result$sources), 2)
})

## Specific element tests ----

test_that("extract_metadata extracts licenses correctly", {
  dp <- create_test_dp()

  result <- extract_metadata(dp, elements = "licenses")

  expect_equal(nrow(result), 1)
  expect_true("name" %in% names(result))
  expect_equal(result$name[1], "CC-BY-4.0")
})

test_that("extract_metadata extracts relatedIdentifiers correctly", {
  dp <- create_test_dp()

  result <- extract_metadata(dp, elements = "relatedIdentifiers")

  # relatedIdentifiers is a list of objects
  expect_equal(nrow(result), 2)
  expect_true("relationType" %in% names(result))
  expect_true("relatedIdentifier" %in% names(result))
})

test_that("extract_metadata extracts references correctly", {
  dp <- create_test_dp()

  result <- extract_metadata(dp, elements = "references")

  # references is a character vector (flat)
  expect_equal(nrow(result), 1)
  expect_true(any(grepl("Author et al", result)))
})

test_that("extract_metadata extracts project correctly", {
  dp <- create_test_dp()

  result <- extract_metadata(dp, elements = "project")

  expect_equal(nrow(result), 1)
  expect_true("id" %in% names(result))
  expect_true("title" %in% names(result))
  expect_equal(result$samplingDesign[1], "targeted detection")
})

test_that("extract_metadata extracts WildObsMetadata correctly", {
  dp <- create_test_dp()

  result <- extract_metadata(dp, elements = "WildObsMetadata")

  expect_equal(nrow(result), 1)
  expect_true("tabularSharingPreference" %in% names(result))
  expect_equal(result$tabularSharingPreference[1], "open")
})

test_that("extract_metadata extracts spatial correctly", {
  dp <- create_test_dp()

  result <- extract_metadata(dp, elements = "spatial")

  expect_equal(nrow(result), 1)
  expect_true("type" %in% names(result))
  # bbox should be flattened into columns
  expect_true(any(grepl("bbox", names(result), ignore.case = TRUE)))
})

test_that("extract_metadata extracts temporal correctly", {
  dp <- create_test_dp()

  result <- extract_metadata(dp, elements = "temporal")

  expect_equal(nrow(result), 1)
  # Should have columns from the nested structure
  expect_true(ncol(result) >= 2) # At least study_period and DPID
})

test_that("extract_metadata extracts taxonomic correctly", {
  dp <- create_test_dp()

  result <- extract_metadata(dp, elements = "taxonomic")

  # taxonomic is a list of objects (one per species)
  expect_equal(nrow(result), 2)
  expect_true("scientificName" %in% names(result))
  expect_true("taxonRank" %in% names(result))
})

## Edge cases and error handling ----

test_that("extract_metadata handles missing element in DP", {
  dp <- create_test_dp()
  dp$contributors <- NULL  # Remove contributors

  result <- extract_metadata(dp, elements = c("contributors", "sources"))

  # Should only return sources since contributors is missing
  expect_type(result, "list")
  expect_named(result, "sources")
  expect_false("contributors" %in% names(result))
})

test_that("extract_metadata handles empty element in DP", {
  dp <- create_test_dp()
  dp$contributors <- list()  # Empty list

  result <- extract_metadata(dp, elements = c("contributors", "sources"))

  # Should skip empty contributors
  expect_type(result, "list")
  expect_named(result, "sources")
})

test_that("extract_metadata returns single element when only one found", {
  dp <- create_test_dp()
  dp$sources <- NULL  # Remove sources

  result <- extract_metadata(dp, elements = c("contributors", "sources"))

  # Should return data frame (not list) when only one element found
  expect_s3_class(result, "data.frame")
  expect_true("DPID" %in% names(result))
})

test_that("extract_metadata handles all elements missing", {
  dp <- list(id = "test_dp_001")  # Minimal DP with only ID

  result <- extract_metadata(dp, elements = "contributors")

  # Should return empty list (Filter removes NULL)
  expect_type(result, "list")
  expect_equal(length(result), 0)
})

test_that("extract_metadata uses default elements when not specified", {
  dp <- create_test_dp()

  # Default is all supported elements
  result <- extract_metadata(dp)

  expect_type(result, "list")
  # Should have most/all default elements present
  expect_true(length(result) >= 5)
})

## Data type and structure tests ----

test_that("extract_metadata preserves column names", {
  dp <- create_test_dp()

  result <- extract_metadata(dp, elements = "contributors")

  # Should have original column names from list elements
  expect_true("title" %in% names(result))
  expect_true("email" %in% names(result))
  expect_true("role" %in% names(result))
})

test_that("extract_metadata handles mixed data types in contributors", {
  dp <- create_test_dp()
  # Add contributor with different structure
  dp$contributors[[3]] <- list(
    title = "Mixed User",
    email = "mixed@test.com",
    role = "reviewer",
    orcid = "0000-0001-2345-6789"
  )

  result <- extract_metadata(dp, elements = "contributors")

  # Should handle extra columns gracefully
  expect_equal(nrow(result), 3)
  expect_true("orcid" %in% names(result))
  # First two rows should have NA for orcid
  expect_true(is.na(result$orcid[1]))
  expect_true(is.na(result$orcid[2]))
  expect_false(is.na(result$orcid[3]))
})

test_that("extract_metadata handles character vectors (references)", {
  dp <- create_test_dp()

  result <- extract_metadata(dp, elements = "references")

  # references is character vector, should be converted to data frame
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
})

## Integration test ----

test_that("extract_metadata works end-to-end with multiple DPs and elements", {
  # Create multiple diverse DPs
  dp1 <- create_test_dp(id = "project_A")
  dp2 <- create_test_dp(id = "project_B")

  # Modify dp2 to have different structure
  dp2$contributors[[3]] <- list(title = "Extra Person", email = "extra@test.com", role = "analyst")
  dp2$taxonomic[[3]] <- list(
    scientificName = "Vulpes vulpes",
    vernacularNames = list(en = "Red Fox"),
    taxonRank = "species"
  )

  dp_list <- list(dp1, dp2)

  # Extract multiple elements
  result <- extract_metadata(dp_list, elements = c("contributors", "taxonomic", "project"))

  # Verify structure
  expect_type(result, "list")
  expect_equal(length(result), 3)
  expect_named(result, c("contributors", "taxonomic", "project"))

  # Verify contributors combined correctly
  expect_equal(nrow(result$contributors), 5) # 2 + 3
  expect_true("project_A" %in% result$contributors$DPID)
  expect_true("project_B" %in% result$contributors$DPID)

  # Verify taxonomic combined correctly
  expect_equal(nrow(result$taxonomic), 5) # 2 + 3

  # Verify project combined correctly
  expect_equal(nrow(result$project), 2) # 1 per DP
})
