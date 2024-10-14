#' Check overlap with GBIF species ranges
#'
#' This function will carry out a GBIF range check to flag for species that might be incorrectly identified.
#' It does this by checking against polygons predicting species ranges generated from GBIF records.
#' This is not perfect, but it provides a quick flag to check for anything that might not make sense
#' e.g. gorillas in the centre of Australia.
#'
#'
#' @param deps The deployment file with camera coordinates which will take the average these must be labelled as Latitude and Longitude or X and Y, the code will check for both.
#' @param obs The observations file which will generate the species list to check. There must be a column called Species or scientificName
#' @param tif_folder The folder containing the GBIF files to be checked this can be the path it self or a vector.
#'
#' @return A data frame containing the following columns:
#' \enumerate{
#'   \item Species: The binomial nomenclature of the species name.
#'   \item Overlapping: A logical value indicating whether the species overlaps with the GBIF raster.
#'   \item File_Not_Found: Indicates whether the GBIF file for the species could not be found.
#' }
#'
#' @details The GBIF raster layers that this function relies upon do not live in this R package (for now). These are maintined in the WildObs DropBox account and can be granted access with permission.
#' @examples
#' gbif_folder = "path/to/GBIF/files"
#' gbif_overlap_result = gbif_check(dep, caps, gbif_folder)
#' print(gbif_overlap_result)
#' @import terra
#' @importFrom dplyr select starts_with
#' @importFrom tidyselect all_of
#' @author Tom Bruce
#' @export
gbif_check <- function(deps, obs, tif_folder) {
  # Check for Latitude and Longitude columns; if not present, use X and Y
  #This builds in flexibility based on when in the pipeline you use this function, it usually comes at the species naming phase so before coordinates are properly formatted so be cautions.

  #First capitals can cause problems address this.
  # Check if "Latitude" or "Longitude" exists, and rename them to lowercase if found
  if ("Latitude" %in% colnames(deps)) {
    colnames(deps)[colnames(deps) == "Latitude"] <- "latitude"
  }

  if ("Longitude" %in% colnames(deps)) {
    colnames(deps)[colnames(deps) == "Longitude"] <- "longitude"
  }

  if ("latitude" %in% colnames(deps) && "longitude" %in% colnames(deps)) {#If the dep frame has latittude and longitude they are the coordiantes
    lat_col <- "latitude"
    lon_col <- "longitude"
  } else if ("X" %in% colnames(deps) && "Y" %in% colnames(deps)) {#If there are X and Y use them for coordinates
    lat_col <- "X"
    lon_col <- "Y"
  } else { #If there are no coordinates tell the user
    stop("Cannot find coordinates in the 'deps' data frame.")
  }

  # Filter deployments with complete cases (non-missing latitude/longitude)
  # Need to subset dep to only include deployments with proper coordinates to prevent NAs from tripping it up
  #Now using starts_with to account for different column names
  dep <- deps[complete.cases(dplyr::select(deps, dplyr::starts_with("deployment"), all_of(lat_col), all_of(lon_col))), ]


  # Check if 'scientificName' or 'Species' exists in the 'obs' data frame
  if ("scientificName" %in% colnames(obs)) {
    species_list <- sort(unique(obs$scientificName))
    species_col_name <- "scientificName"  # Store the column name for later use
  } else if ("Species" %in% colnames(obs)) {
    species_list <- sort(unique(obs$Species))
    species_col_name <- "Species"  # Store the column name for later use
  } else {
    stop("Neither 'scientificName' nor 'Species' column found in the 'obs' data frame.")
  }

  # Initialize result dataframe with the correct species column name dynamically
  gbif_overlap_result <- data.frame(
    setNames(list(character(0), character(0), character(0)),
             c(species_col_name, "Overlapping", "File_Not_Found")),
    stringsAsFactors = FALSE
  )

  # Loop through each species
  for (species in species_list) {
    # Construct file path for the species-specific tif file
    tif_file <- file.path(tif_folder, paste0(species, ".tif"))

    if (file.exists(tif_file)) {
      # Calculate the average latitude and longitude for the species from the 'dep' dataframe
      avg_lat <- mean(dep[[lat_col]])
      avg_lon <- mean(dep[[lon_col]])

      # Create a SpatVector with the average location
      species_point <- terra::vect(matrix(c(avg_lon, avg_lat), ncol = 2), crs = "EPSG:4326")

      # Load the raster file using terra
      raster_data <- terra::rast(tif_file)

      # Transform species point to the CRS of the raster
      species_point <- terra::project(species_point, crs(raster_data))

      # Extract pixel value for the point
      pixel_value <- terra::extract(raster_data, species_point)

      # Check if pixel value is NA or valid
      if (is.na(pixel_value$layer)) { #Needed to define layer here to make sure it's registering properly.
        gbif_overlap_result <- rbind(gbif_overlap_result,
                                     data.frame(setNames(list(species, "No overlap", ""),
                                                         c(species_col_name, "Overlapping", "File_Not_Found")),
                                                stringsAsFactors = FALSE))
      } else {
        gbif_overlap_result <- rbind(gbif_overlap_result,
                                     data.frame(setNames(list(species, "Overlap", ""),
                                                         c(species_col_name, "Overlapping", "File_Not_Found")),
                                                stringsAsFactors = FALSE))
      }
    } else {
      # If the TIF file doesn't exist, record "No GBIF file" for that species
      gbif_overlap_result <- rbind(gbif_overlap_result,
                                   data.frame(setNames(list(species, "", "No GBIF file"),
                                                       c(species_col_name, "Overlapping", "File_Not_Found")),
                                              stringsAsFactors = FALSE))
    }
  }

  return(gbif_overlap_result)
}

