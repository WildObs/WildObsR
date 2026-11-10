# Package index

## Discover

Search the WildObs database and read what each project describes.

- [`wildobs_mongo_query()`](https://wildobs.github.io/WildObsR/reference/wildobs_mongo_query.md)
  : Query WildObs MongoDB for Relevant Project IDs
- [`extract_metadata()`](https://wildobs.github.io/WildObsR/reference/extract_metadata.md)
  : Extract Metadata Elements from Data Packages

## Access

Download data packages and images, and cite them.

- [`wildobs_dp_download()`](https://wildobs.github.io/WildObsR/reference/wildobs_dp_download.md)
  : Download Data from WildObs' MongoDB and Format as Frictionless Data
  Packages
- [`wildobs_media_download()`](https://wildobs.github.io/WildObsR/reference/wildobs_media_download.md)
  : Download the Media Files Listed in a Camtrap DP Media Table
- [`cite_datapackages()`](https://wildobs.github.io/WildObsR/reference/cite_datapackages.md)
  : Cite the Camera Trap Data Packages Used in an Analysis
- [`example_datapackages()`](https://wildobs.github.io/WildObsR/reference/example_datapackages.md)
  : Example WildObs Data Packages

## Interoperate

Move between WildObs and canonical Camtrap DP.

- [`as_camtrapdp()`](https://wildobs.github.io/WildObsR/reference/as_camtrapdp.md)
  : Convert a WildObs Data Package to Canonical Camtrap DP
- [`apply_schema_types()`](https://wildobs.github.io/WildObsR/reference/apply_schema_types.md)
  : Apply camtrapDP Schemas to Data
- [`extract_tags()`](https://wildobs.github.io/WildObsR/reference/extract_tags.md)
  : Extract Tag Key:Value Pairs into Individual Columns

## Harmonise

Resample many projects to shared spatial and temporal units.

- [`spatial_hexagon_generator()`](https://wildobs.github.io/WildObsR/reference/spatial_hexagon_generator.md)
  : Generate spatially distinct sampling units
- [`resample_covariates_and_observations()`](https://wildobs.github.io/WildObsR/reference/resample_covariates_and_observations.md)
  : Spatially resample the covariates and observations resources
  Spatially Resample Covariates and Observations Across Spatial Scales
- [`update_temporally_overlapping_deployments()`](https://wildobs.github.io/WildObsR/reference/update_temporally_overlapping_deployments.md)
  : Group Temporally Overlapping Deployment IDs
- [`recombine_deployment_groups()`](https://wildobs.github.io/WildObsR/reference/recombine_deployment_groups.md)
  : Recombine 100-Day Deployments Into Continuous Long-Term Deployments
- [`reclassify_eventID()`](https://wildobs.github.io/WildObsR/reference/reclassify_eventID.md)
  : Reclassify eventID Values Using a New Temporal Threshold
- [`survey_and_deployment_generator()`](https://wildobs.github.io/WildObsR/reference/survey_and_deployment_generator.md)
  : Determine Single-Season Sampling Efforts from Camera Trap Images

## Model

Build sampling effort and detection histories, and format them for
modelling packages.

- [`effort_matrix()`](https://wildobs.github.io/WildObsR/reference/effort_matrix.md)
  : Build a Sampling Effort Matrix from Camera Deployments
- [`detection_matrix()`](https://wildobs.github.io/WildObsR/reference/detection_matrix.md)
  : Build Detection Histories that Match a Sampling Effort Matrix
- [`model_matrix_input()`](https://wildobs.github.io/WildObsR/reference/model_matrix_input.md)
  : Format Detection Histories and Covariates for Occupancy and
  Abundance Models

### Legacy

Kept for existing workflows.

- [`matrix_generator()`](https://wildobs.github.io/WildObsR/reference/matrix_generator.md)
  : Generate Detection History and Covariate Matrices from camtrapDP
  Formatted Data

## Spatial context

Place cameras in their Australian setting.

- [`AUS_state_locator()`](https://wildobs.github.io/WildObsR/reference/AUS_state_locator.md)
  : Determine Australian State from Coordinates
- [`ibra_classification()`](https://wildobs.github.io/WildObsR/reference/ibra_classification.md)
  : Determine Interim Biogeographic Regionalisation for Australia (IBRA)
  bio-regions and subregions
- [`locationName_verification_CAPAD()`](https://wildobs.github.io/WildObsR/reference/locationName_verification_CAPAD.md)
  : Verify Location Names Using the Collaborative Australian Protected
  Areas Database
- [`locationName_buffer_CAPAD()`](https://wildobs.github.io/WildObsR/reference/locationName_buffer_CAPAD.md)
  : Generate Location Buffers and Assign CAPAD Protected Area Names
- [`UTM_coord_generator()`](https://wildobs.github.io/WildObsR/reference/UTM_coord_generator.md)
  : Generate UTMs when latitude,longitude,and UTM_Zone have been
  provided in a dataframe
- [`find_nearest_neighbor_distance()`](https://wildobs.github.io/WildObsR/reference/find_nearest_neighbor_distance.md)
  : Calculate Nearest Neighbor Distances for Camera Trap Locations

## Utilities

- [`WildObsR-package`](https://wildobs.github.io/WildObsR/reference/WildObsR.md)
  [`WildObsR`](https://wildobs.github.io/WildObsR/reference/WildObsR.md)
  : WildObsR: Professional tools for camera trap data access,
  management, and analysis in R
- [`install_claude_skill()`](https://wildobs.github.io/WildObsR/reference/install_claude_skill.md)
  : Install a WildObsR Skill for Claude
- [`update_common_to_binomial()`](https://wildobs.github.io/WildObsR/reference/update_common_to_binomial.md)
  : Update common names to binomial nomenclature
- [`verify_col_match()`](https://wildobs.github.io/WildObsR/reference/verify_col_match.md)
  : Verify Column Matches Between Two Dataframes
- [`rename_or_add_column()`](https://wildobs.github.io/WildObsR/reference/rename_or_add_column.md)
  : Rename or Add a Column
- [`get_decimal_places()`](https://wildobs.github.io/WildObsR/reference/get_decimal_places.md)
  : Get Decimal Places from Numeric Values
- [`` `%>%` ``](https://wildobs.github.io/WildObsR/reference/pipe.md) :
  Pipe operator

## Data

- [`capad`](https://wildobs.github.io/WildObsR/reference/capad.md) :
  Collaborative Australian Protected Areas Database (CAPAD) 2022,
  Terrestrial
- [`ibra`](https://wildobs.github.io/WildObsR/reference/ibra.md) :
  Interim Biogeographic Regionalisation for Australia (IBRA7) Subregions
- [`species_traits`](https://wildobs.github.io/WildObsR/reference/species_traits.md)
  : Species Trait Data

## Deprecated

- [`check_schema()`](https://wildobs.github.io/WildObsR/reference/check_schema.md)
  : Validate Data Against CamTrap DP Schemas
