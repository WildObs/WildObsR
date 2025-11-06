#' WildObsR: Professional tools for camera trap data access, management, and analysis in R
#'
#' WildObsR provides professional tools for accessing, managing, and analyzing
#' wildlife camera trap data following the [Camera Trap Data Package (Camtrap DP)](https://camtrap-dp.tdwg.org/)
#' standard. It enables reproducible workflows for querying, downloading, validating,
#' and processing camera trap datasets from [WildObs](https://wildobs.org.au).
#'
#' Built around the [Frictionless Data Package](https://specs.frictionlessdata.io/data-package/)
#' specification, WildObsR ensures that camera trap data remain
#' [FAIR](https://ardc.edu.au/resource-hub/making-data-fair/)—Findable, Accessible,
#' Interoperable, and Reusable. The package also includes tools for spatial
#' resampling, matrix generation for hierarchical models, and schema validation.
#'
#' @section Key features:
#' * Query and download camera trap data packages via API
#' * Validate datasets against schemas
#' * Spatially resample deployments and observations to enhance hierarchical modeling
#' * Build detection and abundance matrices formatted for the \pkg{unmarked} package
#'
#' @section Main functions:
#' * `wildobs_mongo_query()` – Search WildObs database by spatial, temporal, or project filters
#' * `wildobs_dp_download()` – Download full or partial Camtrap DP data packages
#' * `resample_covariates_and_observations()` – Aggregate data spatially and temporally
#' * `matrix_generator()` – Generate occupancy or abundance matrices
#'
#' @section Learn more:
#' Visit the project repository: \url{https://github.com/WildObs/WildObsR}
#'
#' @docType package
#' @name WildObsR
#' @keywords package
"_PACKAGE"
