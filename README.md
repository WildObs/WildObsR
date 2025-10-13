<div style="display: flex; align-items: center;">
  <img src="man/figures/WildObsLogo_green.svg" style="width: 60%; height: auto;" alt="WildObsR logo"/>
  <img src="man/figures/wildobs.svg" style="width: 20%; height: auto; margin-left: auto;" alt="WildObsR icon"/>
</div>

# WildObsR

<!-- badges: start -->
[![R Package](https://img.shields.io/badge/R-package-blue.svg)](https://www.r-project.org/)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Codecov test coverage](https://codecov.io/gh/WildObs/WildObsR/graph/badge.svg)](https://app.codecov.io/gh/WildObs/WildObsR)
<!-- badges: end -->

> Professional tools for camera trap data management and analysis in R

WildObsR provides a comprehensive suite of functions for processing, standardizing, and analyzing wildlife camera trap data. Built to support the [Camera Trap Data Package (Camtrap DP)](https://camtrap-dp.tdwg.org/) data standard and [Frictionless Data](https://specs.frictionlessdata.io/data-package/) specifications, this package streamlines workflows from raw observations to publication-ready analyses.

---

## Key Features

### Data Access & Management
- **MongoDB Integration**: Query and download camera trap data from WildObs internal database via API
- **Schema Validation**: Enforce data quality with Camtrap DP schema checking and type coercion
- **Flexible Filtering**: Query projects by spatial bounds, temporal ranges, species, contributors, and sampling design

### Spatial Analysis
- **Location Enrichment**: Automatically assign protected area names using [CAPAD](https://www.dcceew.gov.au/environment/land/nrs/science/capad), bioregions using [IBRA](https://www.dcceew.gov.au/environment/land/nrs/science/ibra), and Australian states using [ozmaps](https://mdsumner.github.io/ozmaps/)
- **UTM Conversion**: Generate UTM coordinates with automatic zone detection
- **Spatial Buffers**: Create location buffers and identify overlapping camera deployments
- **Coordinate Precision**: Handle coordinate rounding for data privacy

### Data Processing
- **Temporal Wrangling**: Detect and resolve overlapping deployments, spatially resample observations and deployments to custom spatial scales
- **Taxonomic Validation**: Fuzzy matching for species names, taxonomy extraction from `taxize` objects
- **Survey Generation**: Create spatially and temporally defined camera deployment and surveys from camtrap DP data.
- **Matrix Generation**: Build detection/non-detection matrices, and site- and observation-level covariates for occupancy & abundance modeling formatted for the [`unmarked` R library](https://rbchan.github.io/unmarked/)

---

## Installation

```r
# Install from GitHub
# install.packages("devtools")
devtools::install_github("WildObs/WildObsR")
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
deployments <- frictionless::read_resource(dp_list[[1]], "deployments")
# Access observations
observations <- frictionless::read_resource(dp_list[[1]] "observations")
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

---

## Core Functions

| Category | Functions |
|----------|-----------|
| **Data Access** | `wildobs_mongo_query()`, `wildobs_dp_download()` |
| **Spatial** | `AUS_state_locator()`, `ibra_classification()`, `locationName_buffer_CAPAD()`
| **Data Wrangling** | `survey_and_deployment_generator()`, `matrix_generator()`, `resample_covariates_and_observations()` |
| **Quality Control** | `check_schema()`, `apply_schema_types()`, `verify_col_match()` |
| **Taxonomy** | `update_common_to_binomial()`, `extract_classif()` |

---

## Data Standards

WildObsR adheres to community standards for reproducible wildlife research:

- **[Camera Trap Data Package (Camtrap DP)](https://camtrap-dp.tdwg.org/)**: TDWG standard for camera trap data exchange
- **[Frictionless Data Package](https://specs.frictionlessdata.io/data-package/)**: Specification for self-describing data
- **[Darwin Core](https://dwc.tdwg.org/)**: Biodiversity data standard for taxonomic and occurrence information

Data packages downloaded from WildObs include:
- **Project metadata**: Contributors, licenses, spatial/temporal coverage, taxonomic scope
- **Schemas**: Field definitions, types, constraints for validation
- **Deployments**: Camera locations, setup dates, habitat covariates
- **Observations**: Species detections with timestamps and classifications
- **Media**: Image/video metadata (optional)
- **Covariates**: Environmental values derived from shape files for each coordinate (*e.g.*, elevation)

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
5. Submit a pull request and we will review your new feature! 

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
Amir, Z., Bruce, T., & Contributors. (2024). WildObsR: Professional tools for camera trap
data management and analysis in R. R package version 0.1.0.
https://github.com/ecological-conservancy/WildObsR
```

---

## License

MIT License - see [LICENSE](LICENSE) file for details.

---

## Support

- **Issues**: [GitHub Issues](https://github.com/WildObs/WildObsR/issues)
- **WildObs Support**: wildobs-support@qcif.edu.au
- **Maintainer**: Zachary Amir (z.amir@uq.edu.au)

---

## Acknowledgments

WildObsR development is supported by:

- **Queensland Cyber Infrastructure Foundation (QCIF)**
- **Terrestrial Ecosystem Research Network (TERN)**
- **Australian Research Data Commons (ARDC)**
- **Atlas of Living Australia (ALA)**
- **University of Queensland, School of the Environment**
