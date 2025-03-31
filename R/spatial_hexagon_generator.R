#' Generate spatially distinct sampling units
#'
#' This function generates spatially distinct hexagonal sampling units to group nearby camera trap deployment.
#' It accounts for spatial and temporal overlap between deployments across different contributors to ensure that sampling units are truly independent.
#'
#' @details
#' The function works by overlaying a hexagonal grid of scales determined by the user to group deploymentIDs per `locationName`.
#' Each deploymentID is then assigned a unique `cellID` (temporally distinct) and `polygon` ID (spatially distinct) across all provided scales.
#' If multiple contributors sample the same cell in the same time window, their deployments are grouped into a common code.
#'
#'
#' @param data A dataframe of covariates that includes at minimum: `deploymentID`, `locationName`,`deploymentGroups`, `source`, `latitude`, and `longitude`. These columns are found in WildObs' camtrapDP-formatted Frictionless Data Packages.
#' @param scales A named numeric vector specifying the apothem of hexagonal grid cells (in meters). The names of the vector define the spatial scale (e.g., `c("1km" = 1074.6, "3km" = 1861.2)`).
#'
#' @return A dataframe including the original data plus additional columns for each scale:
#' - `polygon_<scale>`: spatially distinct cell assignments.
#' - `cellID_<scale>`: temporally and spatially distinct cell assignments (appending contributor info).
#'
#' @importFrom magrittr %>%
#' @importFrom sf st_as_sf st_transform st_make_grid st_sf st_join st_set_geometry
#' @importFrom tibble rowid_to_column
#' @importFrom dplyr group_by summarise distinct select full_join mutate filter
#' @importFrom purrr reduce
#' @importFrom stringr str_match
#'
#' @examples
#' # load data
#' dp = frictionless::read_package("/Users/zachary_amir/Dropbox/WildObs master folder/WildObs GitHub Data Storage/data_clean/Step 4 DataPackages/ZAmir_QLD_Wet_Tropics_2022/datapackage.json")
#' # extract covariates resource
#' data = frictionless::read_resource(dp, "covariates")
#' ## assign spatial scales
#' scales = c("1km" = 1074.6, "3km" = 1861.2, "5km" = 2403, "10km" = 3398) # number refer to the apothem of the hexagonal cell, name refers to the area covered by the cell
#' ## add dataSource to the covarites
#' data$source = dp$contributors[[1]]$tag
#'
#' @author Zachary Amir
#'
#' @export
spatial_hexagon_generator = function(data, scales) {

  ### Implement some data checks to make sure the provided data is appropriate
  ## is locationName present?
  if(!any(grepl("locationName", names(data)))){
    stop("The data provided does not contain a column for 'locationName' and must be provided prior to generating hexagonal cells.\n The locationName column defines spatially locations containing distinct deploymentGroups (i.e, spatially distinct camera trap surveys).\n Please ensure locationName is added to the covariates before proceeding with this function.\n You can use the function WildObsR::locationName_buffer_CAPAD() to generate locationNames based on Australian protected areas with a buffer around camera deployments.")
  }
  ## deploymentGroups
  if(!any(grepl("deploymentGroups", names(data)))){
    stop("The data provided does not contain a column for 'deploymentGroups' and must be provided prior to generating hexagonal cells.\n The deploymentGroups column defines temporally distinct sampling efforts in distinct locationNames (i.e, temporally distinct camera trap surveys).\n Please ensure deploymentGroups is added to the covariates before proceeding with this function.\n You can use the function WildObsR::survey_and_deployment_generator() to generate deploymentGroups based on the sampling duration of your deployments and locationNames.")
  }
  ## data source
  if(!any(grepl("source", names(data)))){
    stop("The data provided does not contain a column for 'source' and must be provided prior to generating hexagonal cells.\n The source column simply defines what contributor is responsible for this camera trap project and is necessary for locating spatial-temporal overlap in sampling effort.\n Please ensure source is added to the covariates before proceeding with this function.\n You can access the contributor's information from the project-level metadata in camtrap DP fricitonless data package using this code: dp$contributors[[1]]$tag, where dp is your data package.")
  }
  ## need lats and longs
  if(!any(
    any(grepl("latitude", names(data))) &
    any(grepl("longitude", names(data))))){
    stop("The data provided does not contain columns for 'latitude' and/or 'longitude', but these must be provided to ensure hexagonal cells are spatially distinct.\n Please add 'latitude' and 'longitude' to your data before proceeding with this function.")
  }
  ## ensure date/time are in as.posixct format
  if(!any(inherits(data[["deploymentStart"]], "POSIXct") & inherits(data[["deploymentEnd"]], "POSIXct"))){
    stop("The data provided contains deploymentStart and/or deploymentEnd columns that are not formatted as a proper datetime class POSIXct.\n Please use the as.posixct() function to format your deploymentStart and deploymentEnd columns before using this function.")
  }

  #
  ##
  ### First generate hexagonal cells per locationName
  land_list = list() # temporary store results here
  for(i in 1:length(unique(data$locationName))){ #for each unique Landscape

    #select a single Landscape and subset the data
    l = unique(data$locationName)[i]
    dat = data[data$locationName == l,]

    ## Convert finaldata to a spatial object using the coordinates
    shape = sf::st_as_sf(dat, coords = c("longitude", "latitude"),
                         crs = "+proj=longlat +datum=WGS84")

    # Ensure EPSG is 4087 via transformation
    shape = sf::st_transform(shape, 4087)

    ## produce hexagons at each scale
    hex_list = list()
    for(s in 1:length(scales)){
      # create a new column name
      colname = paste("cellID", names(scales)[s], sep = "_")
      # make the hex
      hex = sf::st_make_grid(shape, cellsize = scales[s], square = FALSE) %>%
        sf::st_sf() %>%
        tibble::rowid_to_column(colname)
      # Add locationName name to cell_id
      hex[[colname]] = paste(unique(shape$locationName), hex[[colname]],
                             "cellID", names(scales)[s], sep = "_")
      # combine the hex to the shape
      temp = sf::st_join(shape, hex, join = sf::st_intersects)%>%
        sf::st_set_geometry(NULL) # dont need geom anymore
      #only interested in deploymentID and cell_id
      temp = dplyr::select(temp, deploymentID, paste("cellID", names(scales)[s], sep = "_"))
      # save in a list
      hex_list[[s]] = temp
    } # end per scale for hexagon generation

    ## Merge all together on the basis of deploymentID
    t = purrr::reduce(hex_list, dplyr::full_join, by = "deploymentID")

    #add conditional statement for cams that intersect two cells
    if(length(unique(duplicated(t$deploymentID))) > 1){

      ## Isolate the cams w/ duplicated cells
      du = t$deploymentID[duplicated(t$deploymentID)==T]

      ## Create empty df to fill in new (single) cell_id's
      r = t[0,]

      for(u in 1:length(du)){ # Repeat for each duplicated cell id

        # select a single unique cam
        dup = t[t$deploymentID == du[u],]

        #just take the first cell_id, for simplicity sake.
        dupl = dup[1,]

        #save it
        r = rbind(r, dupl)

      } # end du

      ## remove duplicated cams from t
      t = t[!t$deploymentID %in% r$deploymentID,]

      ## and bind new single cell id
      t = rbind(r, t)

    } # end conditional

    ## run distinct to verify there are no repeats
    t = dplyr::distinct(t)

    ## ensure there is a safe merge
    if(length(setdiff(t$deploymentID, dat$deploymentID)) +
       length(setdiff(dat$deploymentID, t$deploymentID)) != 0){
      stop("A problem occurred when generating hexagonal grid cells for the provided data. Please ensure coordinate information is accurate for all data and try the function again after the problem is resolved.")
    }else{
      ## Merge w/ the provided data
      dat = merge(t, dat, by = "deploymentID")
    }

    ## and save it in the list
    land_list[[i]] = dat
  } # end per locationName
  # clean up for testing
  rm(s,l,i,t,r, temp, dat, hex, hex_list, shape, du, dup, dupl, colname)

  ## combine results into one DF
  meta = do.call(rbind, land_list)

  ## First save Hex ID's so we can look at spatially close cams later
  for(s in 1:length(scales)){
    meta[, paste("polygon", names(scales)[s], sep = "_")] = meta[, paste("cellID", names(scales)[s], sep = "_")]
  }

  ### Create a code to affix to sampling unit to include time and collaborator
  meta$match = stringr::str_match(meta$deploymentGroups, ".*([0-9]{4})[_]?([a-z]*).*")
  # Extract year and letter separately
  meta$year <- meta$match[, 2]
  meta$letter <- meta$match[, 3]

  # Combine year and letter
  meta$code <- paste0(ifelse(meta$letter != "", paste0(meta$year, "_", meta$letter),
                             meta$year), "_", meta$source)
  # Remove the trailing underscore from the values
  meta$code <- sub("_$", "", meta$code)

  ## find the column name with the largest scale
  largest_col = paste("polygon", names(scales)[scales == max(scales)], sep = "_")

  ## TESTING
  # meta$deploymentGroups[meta$cellID_10km == "Eacham_Curtain_Fig_NPs_6_cellID_10km"][1:5] = "test_2022"
  # meta$source[meta$cellID_10km == "Eacham_Curtain_Fig_NPs_6_cellID_10km"][1:5] = "test"

  ### Before we combine, verify no collaborators sampled in the same cell at the same time
  ## for each unique cell, check how many unique sources occurred.
  # it should only be one!
  a <- meta %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(c(largest_col, "code")))) %>%
    dplyr::summarise(
      num_surv = dplyr::n_distinct(deploymentGroups),
      num_source = dplyr::n_distinct(source),
      .groups = "drop"
    )

  ## grab all polygons w/ more than one survey & data source
  poly_check = a[a$num_source >1 & a$num_surv > 1, largest_col]

  ## if there are problematic polygons,
  if(nrow(poly_check) > 0){
    # bring in old chat GPT function to check for date range overlaps within a data frame
    check_overlaps <- function(df) {
      overlaps <- function(start1, end1, start2, end2) {
        return((start1 <= end2) & (end1 >= start2))
      }

      overlaps_matrix <- outer(
        seq_len(nrow(df)), seq_len(nrow(df)),
        Vectorize(function(i, j) overlaps(df$min_date[i], df$max_date[i], df$min_date[j], df$max_date[j]))
      )

      diag(overlaps_matrix) <- FALSE
      return(overlaps_matrix)
    } # end check overlaps function

    # check start/end dates for each to assess overlap
    for(i in 1:nrow(poly_check)){
      ## grab the relevant polygon
      p = poly_check[[largest_col]][i]

      ## grab the relevant code combo too
      yl = a[a[[largest_col]] == p & a$num_source >1 & a$num_surv > 1, ][["code"]]

      ## subset the data
      m = meta[meta[[largest_col]] == p & meta$code == yl,]

      ## summarize sampling duration
      b = m %>%
        dplyr::group_by(source) %>%
        dplyr::summarise(
          min_date = min(deploymentStart, na.rm = TRUE),
          max_date = max(deploymentEnd, na.rm = TRUE),
          .groups = "drop"
        )
      ## This assumes date-times are already formatted, which is true for frictionless DPs, but not true for non-camtrap DP data!

      ## Use custom function to check if there are any temporal overlaps
      if(any(check_overlaps(b))){

        ## If there are overlaps, modify the source code to include both sources.
        # grab both sources
        sources = paste(unique(meta$source[meta[[largest_col]] == p &
                                             meta$code == yl]), collapse = "_&_")
        meta$code[meta[[largest_col]] == p &
                    meta$code == yl] = paste(yl, sources, sep = "_")

        ## print a message as well
        warning(paste(paste("A spatial-temporal overlap was detected at the", names(scales)[scales == max(scales)], "scale from the following data sources:", gsub("_", " ", sources)),
                      "The deployment information from these sources has been grouped into one spatially distinct hexagonal grid cell.",  sep = "\n"))

      } # end overlap condition

    } # end per poly_check

  } # end nrow poly_check condition
  # # clean up testing
  # rm(sources, yl, p, i, m, b,  u, poly_check, a, check_overlaps)

  ### Now that code has been verified for multiple sources sampling in the same cell at the same time,
  ## append it to each cellID to keep cells temporally separated
  for(s in 1:length(scales)){
   meta[[paste("cellID", names(scales)[s], sep = "_")]] = paste(meta[[paste("polygon", names(scales)[s], sep = "_")]],
                                                                meta[["code"]], sep = "_")
  }

  ## Grab the new cols
  new_cols = setdiff(names(meta), names(data))
  # but remove fluff
  new_cols = new_cols[! new_cols %in% c("match","year","letter","code")]
  ## and totally remove

  ## and verify we didnt lose and deployments (idk how we would??)
  if(length(setdiff(meta$deploymentID, data$deploymentID)) +
     length(setdiff(data$deploymentID, meta$deploymentID)) != 0){
    stop("There was a mysterious error in this function that removed deploymentIDs from the data containing hexagonal cells. Please carefully inspect your deploymentIDs to ensure all are spatially and temporally distint before using this function.")
  }

  ## and return the updated dataframe with polygon (spatial only) and cellID (spatial and temporal)
  return(meta[, names(meta) %in% c(names(data), new_cols)])

} # end function
