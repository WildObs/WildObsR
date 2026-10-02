#' Species Trait Data
#'
#' A .csv file containing species trait information used within the WildObs framework.
#' This dataset provides cross-referenced trait data to facilitate consistent ecological analyses.
#'
#' A data.frame of species-level traits for verified taxa. Each row corresponds to a verified species.
#'
#' @format A data frame with the following columns:
#' \describe{
#'   \item{binomial_verified}{Verified scientific name (species level).}
#'   \item{uri}{Unique Resource Identifier corresponding to the taxonomic record.}
#'   \item{phylum, class, order, family, genus, species}{Standard taxonomic ranks.}
#'   \item{home_range_km2}{Estimated home range (km²) for mammals, primarily from \code{HomeRange::GetHomeRangeData()} or, where unavailable, \code{traitdata::pantheria()}.}
#'   \item{home_range_source}{DOI of the source home range dataset.}
#'   \item{AdultBodyMass_g}{Mean adult body mass (grams) for mammals and birds, from \code{traitdata::pantheria()} or \code{traitdata::australian_birds()}.}
#'   \item{bodyMass_source}{DOI of the source body mass dataset.}
#'   \item{epbc_category}{EPBC Act threat status category for Australian fauna. One of: "conservation_dependent", "critically_endangered", "endangered", "extinct", "extinct_in_the_wild", "vulnerable", or "not_listed".}
#'   \item{epbc_location}{The state level acronym where the EPBC Act threat status category is applicable, with several states concatenated together as needed, or NA values if not applicable.}
#' }
#'
#' @details
#' Species trait data helps enable comparative and functional ecological
#' analyses and is used to obscure threatend species location information.
#'
#' @source
#' Data were compiled and verified against:
#' \itemize{
#'   \item HomeRange Database (\doi{10.6084/m9.figshare.16698184})
#'   \item Pantheria Database (\doi{10.1890/08-1494.1})
#'   \item Australian Birds Trait Database (\doi{10.1038/s41597-022-01372-2})
#'   \item Environmental Protection and Biodiversity Conservation Act 1999 (\url{https://www.legislation.gov.au/Series/C2004A00485})
#' }
#'
#' @examples
#' # Load data explicitly
#' data(species_traits)
#' species_traits
#'
#' @source Home range data from HomeRange package;
#' body mass data from traitdata package;
#' EPBC status from Australian Department of Climate Change, Energy, the Environment and Water.
#'
#' @keywords datasets
"species_traits"



#' Interim Biogeographic Regionalisation for Australia (IBRA7) Subregions
#'
#' The 419 IBRA7 subregions of Australia, with their parent bioregions, as an `sf`
#' polygon layer. This is the layer \code{\link{ibra_classification}} uses to assign
#' coordinates to an IBRA subregion and bioregion.
#'
#' @format An `sf` data frame with 419 rows (one MULTIPOLYGON per subregion) and 5
#'   attribute columns, in WGS84 longitude/latitude (EPSG:4326):
#' \describe{
#'   \item{SUB_CODE_7}{IBRA7 subregion code.}
#'   \item{SUB_NAME_7}{IBRA7 subregion name.}
#'   \item{REG_CODE_7}{IBRA7 bioregion code.}
#'   \item{REG_NAME_7}{IBRA7 bioregion name.}
#'   \item{HECTARES}{Area of the subregion in hectares, from the source data.}
#' }
#'
#' @details
#' Derived from the official IBRA7 subregions shapefile (GDA94, EPSG:4283) by keeping only
#' the columns above, repairing invalid polygons, simplifying boundaries with a 25 m
#' tolerance, and reprojecting to EPSG:4326. Simplification was checked against the full
#' resolution layer: all 19,736 WildObs deployments fall in the same subregion with either.
#' The build is recorded in `dev/add_data_to_package.R`.
#'
#' Convert to a `terra` SpatVector with `terra::vect(ibra)` if you prefer `terra`.
#'
#' @source
#' Australian Government Department of Climate Change, Energy, the Environment and Water
#' (DCCEEW). *Interim Biogeographic Regionalisation for Australia (IBRA), Version 7
#' (Subregions)*. Licensed under Creative Commons Attribution.
#' \url{https://www.dcceew.gov.au/environment/land/nrs/science/ibra}
#'
#' @seealso \code{\link{ibra_classification}}, \code{\link{capad}}
#'
#' @examples
#' # The attribute table, without the polygons
#' head(sf::st_drop_geometry(ibra))
#'
#' # Map the subregion outlines
#' plot(sf::st_geometry(ibra))
#'
#' @keywords datasets
"ibra"


#' Collaborative Australian Protected Areas Database (CAPAD) 2022, Terrestrial
#'
#' The 14,234 terrestrial protected areas in CAPAD 2022, as an `sf` polygon layer. This
#' is the layer \code{\link{locationName_verification_CAPAD}} and
#' \code{\link{locationName_buffer_CAPAD}} use to name the protected area around each
#' camera.
#'
#' @format An `sf` data frame with 14,234 rows (one POLYGON or MULTIPOLYGON per protected
#'   area) and 3 attribute columns, in WGS84 longitude/latitude (EPSG:4326):
#' \describe{
#'   \item{NAME}{Protected area name.}
#'   \item{TYPE_ABBR}{Abbreviated protected area type, e.g. `NP` (national park),
#'     `NR` (nature reserve), `IPA` (Indigenous Protected Area).}
#'   \item{IUCN}{IUCN protected area management category: `Ia`, `Ib`, `II`, `III`, `IV`,
#'     `V`, `VI`, `NR` (not reported), `NAS` (not assigned), or `NA` (not applicable).}
#' }
#'
#' @details
#' Derived from the CAPAD 2022 terrestrial shapefile (web Mercator, EPSG:3857) by keeping
#' only the columns above, repairing invalid polygons, simplifying boundaries with a 5 m
#' tolerance, and reprojecting to EPSG:4326. Simplification was checked against the full
#' resolution layer for all 19,736 WildObs deployments: 4 changed between "inside a
#' protected area" and "outside", and none switched from one protected area to another.
#' The build is recorded in `dev/add_data_to_package.R`.
#'
#' @source
#' Australian Government Department of Climate Change, Energy, the Environment and Water
#' (DCCEEW). *Collaborative Australian Protected Areas Database (CAPAD) 2022 -
#' Terrestrial*. Commonwealth of Australia. Licensed under CC BY 4.0.
#' \url{https://www.dcceew.gov.au/environment/land/nrs/science/capad}
#'
#' @seealso \code{\link{locationName_verification_CAPAD}},
#'   \code{\link{locationName_buffer_CAPAD}}, \code{\link{ibra}}
#'
#' @examples
#' # The attribute table, without the polygons
#' head(sf::st_drop_geometry(capad))
#'
#' # How many protected areas of each IUCN category
#' table(capad$IUCN)
#'
#' @keywords datasets
"capad"
