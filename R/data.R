#' Verified Taxonomy and Species Trait Data Package
#'
#' A Frictionless Data Package (`taxonomy_dp`) containing verified species taxonomy
#' and associated trait information used within the WildObs framework.
#' This dataset standardises taxonomic names across all CamtrapDP datasets and
#' provides cross-referenced trait data to facilitate consistent ecological analyses.
#'
#' @format A Frictionless Data Package with two tabular resources:
#'
#' \describe{
#'   \item{\strong{verified_taxonomy}}{
#'     A table of verified species names and their taxonomic classifications.
#'     Each row corresponds to one verified name used across WildObs data packages.
#'     \describe{
#'       \item{binomial_verified}{The verified scientific name used in CamtrapDP data packages. Verified using the Australian Faunal Directory (AFD), Global Biodiversity Information Facility (GBIF), and National Center for Biotechnology Information (NCBI).}
#'       \item{user_provided_name}{The unverified name supplied by data providers, later checked against reference databases.}
#'       \item{Common_name}{Common or vernacular name from the AFD or via `taxize::sci2comm()`.}
#'       \item{uri}{Unique Resource Identifier linking to the preferred taxonomy database (AFD, GBIF, or NCBI).}
#'       \item{taxonomic_level}{The verified taxonomic level (e.g., species, genus) corresponding to the URI.}
#'       \item{match_type}{The type of match between user-provided and verified names, as returned by `galah::search_taxa()`: one of “exactMatch”, “fuzzyMatch”, or “higherMatch”.}
#'       \item{phylum, class, order, family, genus, species}{Standard taxonomic ranks describing each verified name.}
#'     }
#'   }
#'
#'   \item{\strong{species_traits}}{
#'     A table of species-level traits for verified taxa. Each row corresponds to a verified species.
#'     \describe{
#'       \item{binomial_verified}{Verified scientific name (species level).}
#'       \item{uri}{Unique Resource Identifier corresponding to the taxonomic record.}
#'       \item{phylum, class, order, family, genus, species}{Standard taxonomic ranks.}
#'       \item{home_range_km2}{Estimated home range (km²) for mammals, primarily from `HomeRange::GetHomeRangeData()` or, where unavailable, `traitdata::pantheria()`.}
#'       \item{home_range_source}{DOI of the source home range dataset.}
#'       \item{AdultBodyMass_g}{Mean adult body mass (grams) for mammals and birds, from `traitdata::pantheria()` or `traitdata::australian_birds()`.}
#'       \item{bodyMass_source}{DOI of the source body mass dataset.}
#'       \item{epbc_category}{EPBC Act threat status category for Australian fauna. One of: “conservation_dependent”, “critically_endangered”, “endangered”, “extinct”, “extinct_in_the_wild”, “vulnerable”, or “not_listed”.}
#'     }
#'   }
#' }
#'
#' @details
#' This dataset forms the taxonomic and trait backbone of the WildObsR package.
#' It ensures harmonised species naming across diverse camera-trap datasets
#' and integrates external authoritative sources (AFD, GBIF, NCBI) to maintain
#' consistent taxonomic resolution. Trait fields were added to enable
#' comparative and functional ecological analyses.
#'
#' @source
#' Data were compiled and verified against:
#' \itemize{
#'   \item Australian Faunal Directory (\url{https://biodiversity.org.au/afd})
#'   \item Global Biodiversity Information Facility (GBIF, \url{https://www.gbif.org})
#'   \item National Center for Biotechnology Information (NCBI, \url{https://www.ncbi.nlm.nih.gov/taxonomy})
#'   \item HomeRange Database (\doi{10.6084/m9.figshare.16698184})
#'   \item Pantheria Database (\doi{10.1890/08-1494.1})
#'   \item Australian Birds Trait Database (\doi{10.1038/s41597-022-01372-2})
#' }
#'
#' @examples
#' # Load data
#' data(taxonomy_dp)
#'
#' # List resources
#' frictionless::resources(taxonomy_dp)
#'
#' # Preview verified taxonomy
#' verified_taxonomy <- frictionless::read_resource(taxonomy_dp, "verified_taxonomy")
#' head(verified_taxonomy)
#'
#' @keywords datasets
"taxonomy_dp"



#' Interim Biogeographic Regionalisation for Australia (IBRA7) Subregions
#'
#' A spatial dataset (`SpatVector`) representing the *Interim Biogeographic Regionalisation for Australia* (IBRA7) bioregions and subregions.
#' This shapefile provides the spatial foundation for the \code{\link{ibra_classification}} function, which assigns geographic coordinates to their
#' corresponding IBRA bio-region and sub-region. It is based on the official IBRA7 dataset maintained by the Australian Government Department of Climate Change,
#' Energy, the Environment and Water (DCCEEW).
#'
#' @format A `SpatVector` object (from the \pkg{terra} package) with 419 polygons and 16 attributes:
#' \describe{
#'   \item{SUB_CODE_7}{IBRA7 subregion code.}
#'   \item{SUB_NAME_7}{IBRA7 subregion name.}
#'   \item{REG_CODE_7}{IBRA7 bioregion code.}
#'   \item{REG_NAME_7}{IBRA7 bioregion name.}
#'   \item{HECTARES}{Area of the subregion in hectares.}
#'   \item{SQ_KM}{Area of the subregion in square kilometres.}
#'   \item{REC_ID}{Internal record identifier.}
#'   \item{SUB_CODE_6}{IBRA6 subregion code (legacy).}
#'   \item{SUB_NAME_6}{IBRA6 subregion name (legacy).}
#'   \item{SUB_NO_61_}{Numeric subregion identifier.}
#'   \item{REG_CODE_6}{IBRA6 bioregion code (legacy).}
#'   \item{REG_NAME_6}{IBRA6 bioregion name (legacy).}
#'   \item{REG_NO_61}{Numeric bioregion identifier.}
#'   \item{FEAT_ID}{Unique feature ID.}
#'   \item{Shape_Leng}{Perimeter length of the polygon feature (metres).}
#'   \item{Shape_Area}{Area of the polygon feature (square metres).}
#' }
#'
#' @details
#' The IBRA framework divides Australia into distinct biogeographic regions based on shared ecological, geological,
#' and climatic characteristics. This dataset corresponds to the seventh version (IBRA7) and is provided in
#' GDA94 geographic coordinates (EPSG:4283).
#'
#' The dataset is primarily used by the \code{\link{ibra_classification}} function in this package to assign
#' camera-trap deployments or other georeferenced observations to their respective IBRA subregion and bioregion.
#' Users can also employ the dataset directly for spatial analysis or visualisation of Australia's ecological regions.
#'
#' @source
#' Australian Government Department of Climate Change, Energy, the Environment and Water (DCCEEW).
#' *Interim Biogeographic Regionalisation for Australia (IBRA), Version 7 (Regions and Subregions)*.
#' \url{https://www.dcceew.gov.au/environment/land/nrs/science/ibra}
#'
#' @seealso
#' \code{\link{ibra_classification}} for assigning latitude–longitude points to IBRA subregions.
#' \pkg{terra} for reading and manipulating `SpatVector` data.
#'
#' @examples
#' data(ibra)
#'
#' # Inspect structure
#' ibra
#'
#' # Plot subregions
#' if (requireNamespace("terra", quietly = TRUE)) {
#'   terra::plot(ibra, col = "lightgreen", border = "darkgrey")
#' }
#'
#' # Example use with ibra_classification()
#' \dontrun{
#' data <- data.frame(
#'   deploymentID = 1:3,
#'   lat = c(-15.5, -23.2, -17.1),
#'   lon = c(145.7, 133.5, 141.8)
#' )
#' result <- ibra_classification(data, lat_col = "lat", long_col = "lon")
#' head(result)
#' }
#'
#' @keywords datasets
"ibra"
