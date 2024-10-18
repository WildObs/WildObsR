#' Verify Location Names Using State-Specific Landscape Shapefiles
#'
#' This function verifies camera deployment locations by intersecting provided coordinates with landscape shapefiles specific to Australian states.
#' It works at multiple levels, starting by checking National Parks and State Forests based on the state provided, and then fills in any missing values based on proximity to known landscapes.
#' Optionally, the function can also visualize results using a Leaflet map.
#'
#' @details
#' The function operates in a stepwise fashion:
#' \enumerate{
#'   \item Users provide the state (e.g., "QLD", "NSW") to search for relevant shapefiles in a specified directory.
#'   \item The function works through National Parks, State Forests, and other land types for each state.
#'   \item If there are missing values, users are prompted to use distance to the nearest known landscape feature to fill in the gaps.
#' }
#' The function handles shapefiles differently for each state, automatically renaming columns to standardized formats to facilitate further analysis.
#'
#' @param dep The deployments tabular data containing information about each camera deployment, including latitude and longitude columns (either `Latitude`/`Longitude` or `latitude`/`longitude`).
#' @param Landscapes_path The path to the directory containing the landscape shapefiles, which should be organized by state.
#' @param dist A logical value (`TRUE` or `FALSE`). If `TRUE`, the function uses Euclidean distance to find the nearest known landscape feature for deployments with missing values.
#' @param state A character string representing the Australian state where the study is located (e.g., "QLD", "NSW", "VIC"). If `NULL`, the user will be prompted to enter a state.
#'
#' @return A new column appended to the deployments data frame with an updated `locationName` column, indicating the verified landscape (e.g., National Park, State Forest) for each deployment.
#'
#' @importFrom terra vect project extract makeValid distance crs
#' @importFrom stringr str_to_title str_extract
#' @examples
#' # Example usage:
#' dep <- data.frame(
#'   deploymentID = c("dep1", "dep2"),
#'   Latitude = c(-33.8688, -27.4698),
#'   Longitude = c(151.2093, 153.0251)
#' )
#' Landscapes_path <- "~/Dropbox/ECL spatial layers repository/Australian spatial layers GIS data/AUS/Landscapes/"
#' dep$locationName <- locationName_verification(dep, Landscapes_path, dist = TRUE, state = "NSW")
#' table(deps$locationName)
#'
#' @author Tom Bruce
#' @export
locationName_verification <- function(dep, Landscapes_path, dist, state = NULL) {
  # Step 1: Check if 'state' variable exists in dep
  if (is.null(state)) {
    cat("State not provided. Please provide the state (QLD, NSW, VIC, SA, WA, NT,TAS).\n")

    # Step 1a: Ask the user for the state
    state <- readline("Enter the state: ")

    # Step 1b: Validate the entered state
    valid_states <- c("QLD", "NSW", "VIC", "SA", "WA", "NT", "TAS")
    if (!(state %in% valid_states)) {
      stop("Invalid state entered. Please provide a valid state.")
    }
  } else {
    # Step 1c: Validate the provided state
    valid_states <- c("QLD", "NSW", "VIC", "SA", "WA", "NT", "TAS")
    if (!(state %in% valid_states)) {
      stop("Invalid state provided. Please provide a valid state.")
    }
  }

  # Step 2: Construct the full path for the selected state
  state_path <- file.path(Landscapes_path, state)

  # Step 3: Work through National Parks and State Forests based on the selected state
  if (state == "QLD") { #User has entered Queensland
    # Step 2a: Process for QLD
    # 1. Protected_areas = National parks and state forests
    # 2. Rural_properties = Rural properties like cattle stations etc.


    # Define the desired order of layer names; this should be something like National Parks, State Forests, Rural properties but will vary from state to state.
    desired_order <- c("Protected_areas", "Rural_Properties")

    # Initialize an empty list
    shapefile_list <- list()

    # Loop through each layer in the desired order
    for (layer_name in desired_order) {
      # Open the layer using terra::vect
      layer_data <- vect(file.path(state_path, paste0(layer_name, ".shp")))  # Assuming shapefile format

      # Perform column renaming based on layer_name as we need the same name column to extract later to keep code succinct and workable.
      if (grepl("Protected", layer_name)) {  # Then for protected areas rename columns
        # Rename columns for layers containing "Protected"
        layer_data$name_extract <- layer_data$NAMEABBREV
        # Remove spaces and replace with underscores
        layer_data$name_extract <- gsub(" ", "_", layer_data$name_extract)

      } else if (grepl("Rural", layer_name)) {  # Then for rural properties
        # Rename columns for layers containing "Rural"
        layer_data$name_extract <- layer_data$NAME
        # Remove spaces and replace with underscores and add RP after for rural property
        layer_data$name_extract <- gsub(" ", "_", layer_data$name_extract)
        layer_data$name_extract <- paste0(layer_data$name_extract, "_RP")
      }

      # Add the layer to the list
      shapefile_list[[layer_name]] <- layer_data
    }  # Close out QLD

  } else if (state == "NSW") { #User has entered NSW
    # Step 2b: Process for NSW

    desired_order <- c("NPWS_reserves", "State_forest")

    # Initialize an empty list
    shapefile_list <- list()

    # Loop through each layer in the desired order
    for (layer_name in desired_order) {  # Enter layer name loop

      # Open the layer using terra::vect
      layer_data <- vect(file.path(state_path, paste0(layer_name, ".shp")))  # Assuming shapefile format

      # Perform column renaming based on layer_name as we need the same name column to extract later to keep code succinct and workable.
      if (grepl("reserves", layer_name)) {  # Deal with reserves
        # Rename columns for layers containing "reserves"
        layer_data$name_extract <- layer_data$reservenam
        # Then convert names to 'title' i.e. only one capital
        layer_data$name_extract <- str_to_title(layer_data$name_extract)
        # Then remove any spaces and replace with underscores
        layer_data$name_extract <- gsub(" ", "_", layer_data$name_extract)
        # Then paste the reserve type at the end.
        layer_data$name_extract <- paste(layer_data$name_extract, layer_data$reservetyp, sep = "_")

      } else if (grepl("State", layer_name)) {  # Deal with state forest
        # Rename columns for layers containing "State"
        layer_data$name_extract <- layer_data$statefores
        # Then convert names to 'title' i.e. only one capital
        layer_data$name_extract <- str_to_title(layer_data$name_extract)
        # Then remove any spaces and replace with underscores
        layer_data$name_extract <- gsub(" ", "_", layer_data$name_extract)
        # Then paste the letters SF at the end to denote State Forest
        layer_data$name_extract <- paste0(layer_data$name_extract, "_SF")
      }  # Close out our renaming conditional

      # Add the layer to the list
      shapefile_list[[layer_name]] <- layer_data
    }  # Exit NSW


  } else if (state == "VIC") {#User has enteterd VIC
    # Define the desired order of layer names this should be something like National Parks, State Forests, Rural proerties, but will vary from state to state.
    desired_order <- c("PARKRES","PLM25")

    # Initialize an empty list
    shapefile_list <- list()

    # Loop through each layer in the desired order
    for (layer_name in desired_order) {
      # Open the layer using terra::vect
      layer_data <- vect(file.path(state_path, paste0(layer_name, ".shp")))  # Assuming shapefile format

      # Perform column renaming based on layer_name as we need the same name column to extract later to keep code succinct and workable.
      if (grepl("PARK", layer_name)) {  # Rename columns for layers containing "PARK"
        layer_data$name_extract <- layer_data$NAME_SHORT
        # Then in name_extract remove spaces, and replace with underscores
        layer_data$name_extract <- gsub(" ", "_", layer_data$name_extract)
        # These are good to go at this point

      } else if (grepl("PLM25", layer_name)) {  # Rename columns for layers containing "PLM"
        layer_data$name_extract <- layer_data$LABELSHORT
        # Then in name_extract remove spaces, and replace with underscores
        layer_data$name_extract <- gsub(" ", "_", layer_data$name_extract)
        # These are good to go at this point
      }

      # Add the layer to the list
      shapefile_list[[layer_name]] <- layer_data
    }  # Exit VIC

  } else if (state == "SA") {#User has entered SA
    # Step 2d: Process for SA
    desired_order <- c("CONSERVATION_NpwsaReserves_GDA94")

    # Initialize an empty list
    shapefile_list <- list()

    # Loop through each layer in the desired order
    for (layer_name in desired_order) {  # Enter desired order process
      # Open the layer using terra::vect
      layer_data <- vect(file.path(state_path, paste0(layer_name, ".shp")))  # Assuming shapefile format

      # Perform column renaming based on layer_name as we need the same name column to extract later to keep code succinct and workable.
      if (grepl("CONSERVATION", layer_name)) {  # Enter layer name renaming
        # Rename columns for layers containing "CONSERVATION"
        layer_data$name_extract <- layer_data$RESNAME
        # Now paste the type at the end of the name
        layer_data$name_extract <- paste(layer_data$name_extract, layer_data$RESTYPE, sep = "_")
      }  # Exit layer renaming

      # Add the layer to the list
      shapefile_list[[layer_name]] <- layer_data
    }  # Exit SA
  }

  else if (state == "TAS") {#User has entered TAS
    # Step 2d: Process for TAS
    desired_order <- c("Tas_reserves_estate_mainland","list_land_tenure_statewide")

    # Initialize an empty list
    shapefile_list <- list()

    # Loop through each layer in the desired order
    for (layer_name in desired_order) {  # Enter desired order process
      # Open the layer using terra::vect
      layer_data <- vect(file.path(state_path, paste0(layer_name, ".shp")))  # Assuming shapefile format

      # Perform column renaming based on layer_name as we need the same name column to extract later to keep code succinct and workable.
      if (grepl("Tas_reserves", layer_name)) {  # Enter layer name renaming
        # Rename columns for layers containing "CONSERVATION"
        layer_data$name_extract <- layer_data$FEAT_NAME
        # Now paste the type at the end of the name
        layer_data$name_extract <- paste(layer_data$name_extract, layer_data$TEN_CLASS, sep = "_")
        # Replace spaces with underscores
        layer_data$name_extract <- gsub(" ", "_", layer_data$name_extract)
      }  else if (grepl("tenure_statewide", layer_name)) {  # Enter renaming for the third layer
        # Rename columns for layers containing "Pastoral"
        layer_data$name_extract <- layer_data$RES_NAME
        # Replace spaces with underscores
        layer_data$name_extract <- gsub(" ", "_", layer_data$name_extract)
      }  # Exit renaming for each layer

      # Add the layer to the list
      shapefile_list[[layer_name]] <- layer_data
    }  # Exit TAS
  }

  else if (state == "WA") { #User has entered WA
    # Step 2e: Process for WA
    # Define the desired order of layer names; this should be something like National Parks, State Forests, Rural properties, but will vary from state to state.
    desired_order <- c("Legislated_Lands_and_Waters_DBCA_011","ipa_dedicated","Pastoral_Stations_DPLH_083")

    # Initialize an empty list
    shapefile_list <- list()

    # Loop through each layer in the desired order
    for (layer_name in desired_order) {  # Enter layer name conditional
      # Open the layer using terra::vect
      layer_data <- vect(file.path(state_path, paste0(layer_name, ".shp")))  # Assuming shapefile format

      # Perform column renaming based on layer_name as we need the same name column to extract later to keep code succinct and workable.
      if (grepl("Legislated", layer_name)) {  # Enter renaming for the first layer
        # Rename columns for layers containing "Legislated"
        layer_data$name_extract <- layer_data$leg_name
        # Remove any spaces and replace with underscore
        layer_data$name_extract <- gsub(" ", "_", layer_data$name_extract)

        # Then take the Legislative categories and convert them to letter-based abbreviations
        category_abbrev_map <- c(
          "Crown Freehold - Dept Managed" = "CFDM",
          "Nature Reserve" = "NR",
          "National Park" = "NP",
          "State Forest" = "SF",
          "Section 34A Freehold" = "S34AF",
          "Timber Reserve" = "TR",
          "SCRM Act - River Reserve" = "RR",
          "UCL - Section 33(2)" = "UCL33(2)",
          "Section 5(1)(h) Reserve" = "S5(1)(h)R",
          "Marine Park" = "MP",
          "Section 5(1)(g) Reserve" = "S5(1)(g)R",
          "BGPA - Reserve" = "BGR",
          "Conservation Park" = "CP",
          "ZPA - Reserve" = "ZPAR",
          "RIA - Reserve" = "RIAR",
          "Marine Nature Reserve" = "MNR",
          "Marine Management Area" = "MMA"
        )

        # Create the new column leg_abbrev based on leg_catego using the mapping
        layer_data$leg_abbrev <- category_abbrev_map[layer_data$leg_catego]

        # Then add the abbreviation to the end of the name_extract
        layer_data$name_extract <- paste(layer_data$name_extract, layer_data$leg_abbrev, sep = "_")

      } else if (grepl("dedicated", layer_name)) {  # Enter renaming for the second layer
        # Rename columns for layers containing "Dedicated"
        layer_data$name_extract <- layer_data$NAME
        # Remove spaces and replace with underscores
        layer_data$name_extract <- gsub(" ", "_", layer_data$name_extract)
        # Add the three-letter abbreviation "IPA" for Indigenous Protected Area
        layer_data$name_extract <- paste0(layer_data$name_extract, "_IPA")

      } else if (grepl("Pastoral", layer_name)) {  # Enter renaming for the third layer
        # Rename columns for layers containing "Pastoral"
        layer_data$name_extract <- layer_data$stn_name
        # Convert names to title case
        layer_data$name_extract <- str_to_title(layer_data$name_extract)
        # Replace spaces with underscores
        layer_data$name_extract <- gsub(" ", "_", layer_data$name_extract)
        # Then add _RP to the end for rural property
        layer_data$name_extract <- paste0(layer_data$name_extract, "_RP")
      }  # Exit renaming for each layer

      # Add the layer to the list
      shapefile_list[[layer_name]] <- layer_data
    }  # Exit the loop
  }# Exit WA

  else if (state == "NT") { #User has entered NT
    # Step 2f: Process for NT
    desired_order <- c("NT_Parks_g94")

    # Initialize an empty list
    shapefile_list <- list()

    # Loop through each layer in the desired order
    for (layer_name in desired_order) {
      # Read the shapefile using terra's vect function
      layer_data <- terra::vect(file.path(state_path, paste0(layer_name, ".shp")))

      # Perform column renaming based on layer_name
      if (grepl("NT_Parks", layer_name)) {
        # Rename columns for layers containing "PARK"
        layer_data$name_extract <- layer_data$NAME

        # Replace spaces with underscores
        layer_data$name_extract <- gsub(" ", "_", layer_data$name_extract)
      }

      # Add the layer to the list
      shapefile_list[[layer_name]] <- layer_data
    }

  }# Exit NT

  else {
    stop("Invalid state entered. Please choose from QLD, NSW, VIC, SA, WA, NT.")
  }

  # Step 4: Extract any points that lay within the shapefiles with our order of National Park, State Forests, Rural property
  {
    results_list <- list()

    dep$area_name = NA

    #layer_name = "Rural_Properties"
    # Iterate through each layer in shapefile_list
    for (layer_name in names(shapefile_list)) {
      # Read the current layer from the shapefile_list
      gdb_data <- shapefile_list[[layer_name]]

      # Extract the CRS information for the layer using terra's crs() function
      crs_info <- terra::crs(gdb_data)

      # Check for correct capitalization of Latitude and Longitude
      lat_col <- if ("Latitude" %in% names(dep)) "Latitude" else "latitude"
      lon_col <- if ("Longitude" %in% names(dep)) "Longitude" else "longitude"

      # Convert dep data.frame to a SpatVector using the appropriate lat/lon column names
      dep_sp <- vect(dep, geom = c(lon_col, lat_col), crs = crs_info)  # crs_info is the projection you want to use (e.g. "EPSG:4326")

      # Reproject dep_sp to match the CRS of gdb_data using spTransform
      dep_sp <- project(dep_sp, crs_info)  # Ensure crs_info is the desired CRS for reprojecting

      # Convert gdb_data to a terra SpatVector obect to let terra functions work with it
      #gdb_data = terra::vect(gdb_data)

      # Clean gdb_data geometries using terra's makeValid function
      gdb_data <- terra::makeValid(gdb_data)

      # Perform the spatial join using terra's `intersect` or `extract`
      result <- terra::extract(gdb_data, dep_sp)

      # Extract the column you need for the result and modify it if necessary
      result_column <- ifelse(is.na(result$name_extract), NA, gsub(" ", "_", paste0(result$name_extract, "")))

      # Store the result in the results_list
      results_list[[layer_name]] <- result_column

      # Check if dep$area_name is NA
      na_rows <- is.na(dep$area_name)

      # Update NA values with the results of the current layer
      dep$area_name[na_rows] <- result_column[na_rows]

      # Clean up to avoid carrying unwanted objects to the next iteration
      rm(gdb_data, crs_info, dep_sp, result, result_column)}

  }

  # Step 5: Then if there are still NA's take the nearest distance...
  ## Now we will use Euclidean Distance to determine the nearest National Park or Rural property
  # check if NA is present
  if(dist) { #If the user has requested to use nearest neighbour open
    # Check if there are any missing values in area_name
    if (any(is.na(dep$area_name))) {

    crs_info = terra::crs("EPSG:4326") #Lat long as terra distances automatically outputs meters
  
    # Identify the rows with missing values
     missing_rows <- which(is.na(dep$area_name))
  
    # Create a SpatVector for points with non-NA area_name
    non_na_points <- vect(dep[!is.na(dep$area_name), ], geom = c(lon_col, lat_col), crs = crs_info)

      # Loop over each missing row
      for (missing_row in missing_rows) {
    # Extract coordinates of the point with missing area_name
    missing_coords <- c(dep[missing_row, lon_col], dep[missing_row, lat_col])
    #Extract the missing placename
    missing_placename <- dep[missing_row, "placename"]

    # Create a data frame with appropriate column names
    missing_point_df <- data.frame(longitude = missing_coords[1], latitude = missing_coords[2])

    # Create a SpatVector for the point with the missing area_name
    # Ensure crs_info is defined with the correct CRS (Coordinate Reference System)
    missing_point <- vect(missing_point_df, geom = c("longitude","latitude"), crs = crs_info)

    # Calculate distances to non-NA points
    distances <- terra::distance(missing_point, non_na_points)  # Calculates distance in the units of the CRS
    
    # Add the distance as a new attribute/column to the non_na_points object
    non_na_points$distance <- as.vector(distances)  # Add the distance vector directly
    
    
    # Now you can easily sort non_na_points by the distance column
    sorted_points <- non_na_points[order(non_na_points$distance), ]
    
    # Convert the SpatVector (sorted_points) to a data frame
    sorted_points_df <- as.data.frame(sorted_points)
    
    # Display the closest point with all relevant attributes
    closest_match <- sorted_points[1, ]
    closest_match = as.data.frame(closest_match)
    
    # Find the index of the minimum distance
    min_index <- (closest_match$distance)
    
    # Check if the minimum distance is greater than 500 meters
     if (min_index > 5000) {
    cat("Note: Distance to nearest non NA neighbor is greater than 5km for placename", missing_placename, "To", closest_match$placename, "\n")
     }

    # Update the missing value with the nearest neighbor's area_name
    dep$area_name[missing_row] <- closest_match$area_name
      }
    }
  } #Close nearest neighbour NA filling brace

  #Step 6: User defined mapping
  #If the user has indicated they want to plot a map in leaflet.
  # if (vis) {
  #   ## inspect reservenames via leaflet map
  #   # set colors
  #   color_palette <- rainbow(length(unique(dep$area_name)))
  #   # Assign a color to each unique area
  #   landscape_colors <- setNames(color_palette, sort(unique(dep$area_name)))
  #   # Add color information to the data frame
  #   dep$circle_color <- landscape_colors[dep$area_name]
  #
  #   # Visualize with leaflet
  #   leaf_map =
  #     leaflet() %>%
  #     addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
  #     addCircleMarkers(
  #       lng = dep$longitude,
  #       lat = dep$latitude,
  #       label = paste(dep$placename, dep$area_name, sep = " & "),
  #       color = dep$circle_color,  # Use the color column here
  #       group = "Circles"
  #     )
  #   ## show the map
  #   print(leaf_map)
  #
  # } else {
  #   cat("Map not requested by user\n")
  # }
  ### ZDA hashed this out because the map will not generate.

  # Return the modified dep$area_name column
  return(dep$area_name) #This should always be last.

} # Closing brace for the overall function

