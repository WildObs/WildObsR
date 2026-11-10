# WildObsR: Professional tools for camera trap data access, management, and analysis in R

WildObsR provides professional tools for accessing, managing, and
analyzing wildlife camera trap data following the [Camera Trap Data
Package (Camtrap DP)](https://camtrap-dp.tdwg.org/) standard. It enables
reproducible workflows for querying, downloading, validating, and
processing camera trap datasets from [WildObs](https://wildobs.org.au).

## Details

Built around the [Frictionless Data
Package](https://specs.frictionlessdata.io/data-package/) specification,
WildObsR ensures that camera trap data remain
[FAIR](https://ardc.edu.au/resource-hub/making-data-fair/)—Findable,
Accessible, Interoperable, and Reusable. The package also includes tools
for spatial resampling, matrix generation for hierarchical models, and
schema validation.

## Key features

- Query and download camera trap data packages via API

- Validate datasets against schemas

- Spatially resample deployments and observations to enhance
  hierarchical modeling

- Build detection and abundance matrices formatted for the unmarked
  package

## Main functions

- [`wildobs_mongo_query()`](https://wildobs.github.io/WildObsR/reference/wildobs_mongo_query.md)
  – Search WildObs database by spatial, temporal, or project filters

- [`wildobs_dp_download()`](https://wildobs.github.io/WildObsR/reference/wildobs_dp_download.md)
  – Download full or partial Camtrap DP data packages

- [`resample_covariates_and_observations()`](https://wildobs.github.io/WildObsR/reference/resample_covariates_and_observations.md)
  – Aggregate data spatially and temporally

- [`matrix_generator()`](https://wildobs.github.io/WildObsR/reference/matrix_generator.md)
  – Generate occupancy or abundance matrices

## Learn more

Visit the project repository: <https://github.com/WildObs/WildObsR>

## See also

Useful links:

- <https://wildobs.github.io/WildObsR/>

- <https://github.com/WildObs/WildObsR>

- Report bugs at <https://github.com/WildObs/WildObsR/issues>

## Author

**Maintainer**: Zachary Amir <z.amir@uq.edu.au>
([ORCID](https://orcid.org/0000-0002-8398-2059))
