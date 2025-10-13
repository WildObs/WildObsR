# WildObsR

[![R Package](https://img.shields.io/badge/R-package-blue.svg)](https://www.r-project.org/)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

> Professional tools for camera trap data management and analysis in R

WildObsR provides a comprehensive suite of functions for processing, standardizing, and analyzing wildlife camera trap data. Built to support the [Camera Trap Data Package (Camtrap DP)](https://camtrap-dp.tdwg.org/) standard and [Frictionless Data](https://specs.frictionlessdata.io/data-package/) specifications, this package streamlines workflows from raw observations to publication-ready analyses.

---

## Key Features

### Data Access & Management
- **MongoDB Integration**: Query and download camera trap data from WildObs database via API or direct connection
- **Schema Validation**: Enforce data quality with Camtrap DP schema checking and type coercion
- **Flexible Filtering**: Query projects by spatial bounds, temporal ranges, species, contributors, and sampling design

### Spatial Analysis
- **Location Enrichment**: Automatically assign protected area names (CAPAD), bioregions (IBRA), and Australian states
- **UTM Conversion**: Generate UTM coordinates with automatic zone detection
- **Spatial Buffers**: Create location buffers and identify overlapping protected areas
- **Coordinate Precision**: Handle coordinate rounding for data privacy

### Data Processing
- **Temporal Wrangling**: Detect and resolve overlapping deployments, resample observations to custom intervals
- **Taxonomic Validation**: Fuzzy matching for species names, taxonomy extraction from taxize objects
- **Survey Generation**: Create deployment and survey records from raw observation data
- **Matrix Generation**: Build detection/non-detection matrices for occupancy modeling

### Quality Control
- **Date Correction**: Interactive tools for manual date correction
- **Distance Calculations**: Nearest neighbor analysis for spatial validation
- **Column Verification**: Automated checking for required fields and valid formats

---

## Installation

```r
# Install from GitHub
# install.packages("devtools")
devtools::install_github("ecological-conservancy/WildObsR")
```

---

## Quick Start

### Query WildObs Database

```r
library(WildObsR)

# Use general API key
api_key <- "f4b9126e87c44da98c0d1e29a671bb4ff39adcc65c8b92a0e7f4317a2b95de83"

# Query projects in Queensland
spatial_query <- list(xmin = 145.0, xmax = 154.0, ymin = -29.0, ymax = -10.0)
temporal_query <- list(minDate = as.Date("2020-01-01"),
                       maxDate = as.Date("2024-12-31"))

project_ids <- wildobs_mongo_query(
  api_key = api_key,
  spatial = spatial_query,
  temporal = temporal_query,
  taxonomic = c("Phascolarctos cinereus"),  # Koalas
  tabularSharingPreference = "open"
)

# Download data packages
dp_list <- wildobs_dp_download(
  api_key = api_key,
  project_ids = project_ids,
  media = FALSE  # Set TRUE to include media files
)

# Access deployments
deployments <- dp_list[[1]]$resources$deployments$data
observations <- dp_list[[1]]$resources$observations$data
```

### Enrich Location Data

```r
# Add Australian state information
deps <- AUS_state_locator(deployments)

# Add IBRA bioregions
deps <- ibra_classification(deps,
                            lat_col = "latitude",
                            long_col = "longitude")

# Add protected area names
deps <- locationName_verification_CAPAD(deps)

# Generate UTM coordinates
deps <- UTM_coord_generator(deps)
```

### Create Analysis-Ready Data

```r
# Generate survey-level summaries
surveys <- survey_and_deployment_generator(observations, deployments)

# Build detection matrix
detection_matrix <- matrix_generator(
  observations = observations,
  deployments = deployments,
  time_interval_hours = 24,
  independent_detection_minutes = 30
)

# Resample to custom intervals
resampled <- resample_covariates_and_observations(
  observations = observations,
  deployments = deployments,
  time_interval_hours = 168  # Weekly
)
```

---

## Core Functions

| Category | Functions |
|----------|-----------|
| **Data Access** | `wildobs_mongo_query()`, `wildobs_dp_download()` |
| **Spatial** | `AUS_state_locator()`, `ibra_classification()`, `locationName_verification_CAPAD()`, `locationName_buffer_CAPAD()`, `UTM_coord_generator()` |
| **Data Wrangling** | `survey_and_deployment_generator()`, `matrix_generator()`, `update_temporally_overlapping_deployments()`, `resample_covariates_and_observations()` |
| **Quality Control** | `check_schema()`, `apply_schema_types()`, `find_closest_match()`, `verify_col_match()` |
| **Taxonomy** | `update_common_to_binomial()`, `extract_classif()` |
| **Utilities** | `long_to_UTM_zone()`, `find_nearest_neighbor_distance()`, `correct_dates_manually()` |

---

## Data Standards

WildObsR adheres to community standards for reproducible wildlife research:

- **[Camera Trap Data Package (Camtrap DP)](https://camtrap-dp.tdwg.org/)**: TDWG standard for camera trap data exchange
- **[Frictionless Data Package](https://specs.frictionlessdata.io/data-package/)**: Specification for self-describing data
- **[Darwin Core](https://dwc.tdwg.org/)**: Biodiversity data standard for taxonomic and occurrence information

Data packages downloaded from WildObs include:
- **Project metadata**: Contributors, licenses, spatial/temporal coverage, taxonomic scope
- **Deployments**: Camera locations, setup dates, habitat covariates
- **Observations**: Species detections with timestamps and classifications
- **Media**: Image/video metadata (optional)
- **Schemas**: Field definitions, types, constraints for validation

---

## Spatial Data Sources

Location enrichment functions utilize authoritative Australian spatial datasets:

- **[CAPAD (2022)](https://www.environment.gov.au/land/native-vegetation/capad)**: Collaborative Australian Protected Areas Database
- **[IBRA7](https://www.dcceew.gov.au/environment/land/nrs/science/ibra)**: Interim Biogeographic Regionalisation for Australia
- **Australian State Boundaries**: Official administrative boundaries

**Note**: Spatial functions require local copies of shapefiles. Default paths assume Dropbox storage at `~/Dropbox/ECL spatial layers repository/`. Adjust `capad_file_path` and `ibra_file_path` parameters for custom locations.

---

## WildObs Platform

WildObsR interfaces with the [WildObs](https://wildobs.org.au) camera trap database, a collaborative platform for Australian wildlife monitoring data. The database supports:

- **Open Science**: Data sharing with flexible privacy controls (open/partial/closed)
- **Standardization**: Automated validation against Camtrap DP schemas
- **Discoverability**: Spatial/temporal/taxonomic queries across contributed datasets
- **Embargo Periods**: Configurable data release schedules for research protection

Access requires an API key. Contact [wildobs-support@qcif.edu.au](mailto:wildobs-support@qcif.edu.au) for keys with appropriate permissions.

---

## Documentation

Comprehensive documentation for all functions:

```r
# View function help
?wildobs_mongo_query
?survey_and_deployment_generator
?matrix_generator

# Browse package overview
help(package = "WildObsR")
```

Each function includes:
- Detailed parameter descriptions
- Return value specifications
- Working examples with sample data
- References to relevant standards and methods

---

## Contributing

We welcome contributions! Please:

1. Fork the repository
2. Create a feature branch (`git checkout -b feature/your-feature`)
3. Write tests for new functionality
4. Ensure `devtools::check()` passes with no errors
5. Submit a pull request

### Development Guidelines

- Follow [tidyverse style guide](https://style.tidyverse.org/)
- Document functions with roxygen2
- Add unit tests with testthat
- Update NEWS.md for user-facing changes

---

## Testing

WildObsR uses comprehensive unit testing:

```r
# Run all tests
devtools::test()

# Run specific test file
testthat::test_file("tests/testthat/test-matrix_generator.R")

# Check package
devtools::check()
```

Current test coverage: 114+ tests across spatial, data wrangling, and database functions.

---

## Citation

If you use WildObsR in your research, please cite:

```
Amir, Z., Bruce, T., & Contributors. (2024). WildObsR: Tools for camera trap
data management and analysis in R. R package version 0.1.0.
https://github.com/ecological-conservancy/WildObsR
```

---

## License

MIT License - see [LICENSE](LICENSE) file for details.

---

## Support

- **Issues**: [GitHub Issues](https://github.com/ecological-conservancy/WildObsR/issues)
- **WildObs Support**: wildobs-support@qcif.edu.au
- **Maintainer**: Zachary Amir (z.amir@uq.edu.au)

---

## Acknowledgments

WildObsR development is supported by:

- **Queensland Cyber Infrastructure Foundation (QCIF)**
- **University of Queensland, School of the Environment**
- **The Ecological Conservancy Ltd**

Spatial data provided by the Australian Government Department of Climate Change, Energy, the Environment and Water.

---

**Built with ❤️ for the Australian wildlife research community**
