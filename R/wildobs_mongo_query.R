#' Query WildObs MongoDB for Relevant Project IDs
#'
#' This function queries the WildObs MongoDB database for projects matching specified spatial, temporal, taxonomic, contributor, and data-sharing criteria.
#' It extracts metadata from the database and filters projects based on bounding box overlaps, temporal overlaps, species detected, contributors associated, and data sharing preferences. The function also ensures that only projects that have past their embargo date are shared. Only admin credentials in the db_url parameter will allow users to access data with 'closed' sharing agreements or projects not past their embargo date.
#'
#' The character vector of project IDs that is returned from this function is then used in the function @seealso \code{\link{wildobs_dp_download}} for extracting data packages from WildObs' MongoDB.
#'
#' @param db_url A character string specifying the MongoDB connection URI. This should follow the format:
#'   `'mongodb://username:password@host:port/database'`. If `NULL`, the function will stop with an error.
#'   This parameter allows users to specify their own connection string to the WildObs MongoDB instance.
#' @param api_key A character string specifying the API key used for authenticated access to the WildObs
#'  public API. If provided, the function will query the API instead of connecting directly to the MongoDB
#'  instance with `mongolite`. API keys grant read-only access to specific endpoints and should be kept
#'  confidential (e.g., stored in an `.Renviron` file or other secure environment variable).
#'  Defaults to `NULL`, in which case the function expects a valid `db_url` to connect directly to MongoDB.
#'
#' @param spatial A named list specifying spatial query parameters, including:
#'   \describe{
#'     \item{xmin}{Minimum longitude value.}
#'     \item{xmax}{Maximum longitude value.}
#'     \item{ymin}{Minimum latitude value.}
#'     \item{ymax}{Maximum latitude value.}
#'   }
#' @param temporal A named list specifying temporal query parameters, including:
#'   \describe{
#'     \item{minDate}{Earliest allowable date as a `Date` object.}
#'     \item{maxDate}{Latest allowable date as a `Date` object.}
#'   }
#'  @param taxonomic A vector of species names in binomial nomenclature (i,e., Latin names), and all projects that detect any of the species listed will be returned.
#'  @param samplingDesing A vector of enumerated sampling design values, and projects with the specific sampling designs will be returned. The enumerated values are:"simpleRandom", "systematicRandom", "clusteredRandom", "experimental", "targeted", & "opportunistic"
#'  @param contributor A vector of first and last names of people associated with any projects. Regardless of their role in the project, if the name is found in the metadata, the relevant projects will be returned.
#' @param tabularSharingPreference A character vector specifying accepted sharing preferences.
#'   Defaults to `c("open")`, but the user can also specify 'partial' for limited metadata of the project. If the user provides admin DB credentials, the user can access 'closed' data, but if admin credentials have not been provided, 'closed' data will be removed from the projects list.  Only projects with these preferences are returned.
#' @return A character vector of project IDs matching the specified criteria.
#' @examples
#' \dontrun{
#' # Load the general use WildObs API key
#' api_key <- "f4b9126e87c44da98c0d1e29a671bb4ff39adcc65c8b92a0e7f4317a2b95de83"
#'
#' # Define spatial query: extract projects in a specific bounding box
#' spatial_query <- list(xmin = 145.0, xmax = 147.0, ymin = -20.0, ymax = -16.0)
#'
#' # Define temporal query: select projects active in 2022-2025
#' temporal_query <- list(minDate = as.Date("2022-01-01"), maxDate = as.Date("2025-01-01"))
#'
#' # Define taxonomic query: want all koalas and echidnas
#' taxa_query = c("Phascolarctos cinereus", "Tachyglossus aculeatus")
#'
#' # Define sampling design query: opportunistic and random datasets
#' sample_query = c("simpleRandom", "opportunistic", "systematicRandom")
#'
#' # Define contributor query: only want data from the WildObsR maintainer
#' contributor_query = c("Zachary Amir")
#'
#' # Query the WildObs database for matching projects
#' relevant_projects <- wildobs_mongo_query(db_url, spatial = spatial_query, temporal = temporal_query, taxonomic = taxa_query, samplingDesign = sample_query, contributors = contributor_query, tabularSharingPreference = "open")
#'
#' # display the matching projects
#' print(relevant_projects)
#' }
#' @seealso
#' - [mongolite::mongo()] for database queries
#' @importFrom magrittr %>%
#' @importFrom lubridate add_with_rollback
#' @importFrom httr status_code content GET add_headers
#' @importFrom jsonlite fromJSON
#' @importFrom mongolite mongo
#'
#' @export
wildobs_mongo_query = function(db_url = NULL, api_key = NULL,
                               spatial = NULL, temporal = NULL,
                               taxonomic = NULL, samplingDesign = NULL,
                               contributors = NULL,
                               tabularSharingPreference = c("open", "closed")){
  # create an empty vector to store project IDs
  proj_ids = c()

  ## read in environment file with confidential DB access info
  # readRenviron("inst/config/.Renviron.local.ro") # local version
  # readRenviron("inst/config/.Renviron.prod.ro") # remote version

  # ## load information from enviromnet
  # HOST <- Sys.getenv("HOST")
  # PORT <- Sys.getenv("PORT")
  # DATABASE <- Sys.getenv("DATABASE")
  # USER <- Sys.getenv("USER")
  # PASS <- Sys.getenv("PASS")
  #
  # ## combine all the information into a database-url to enable access
  # db_url <- sprintf("mongodb://%s:%s@%s:%s/%s", USER, PASS, HOST, PORT, DATABASE)
  # rm(USER, PASS, HOST, PORT, DATABASE)

  ### Determine if we will use the API key or the DB url to access data
  if(!is.null(api_key) && is.null(db_url)){
    # if API key is supplied and db url is still null, use the API
    use_api = TRUE
  }else{
    # but if there is a db_url supplied, prioritize that over the API key
    if(!is.null(db_url)){
      use_api = FALSE
    } # end non-null DB condition
  } # end api key present condition

  ## But if neither an API key or db_url was provied
  if(is.null(api_key) && is.null(db_url)){
    # stop the function and tell them to get more info!
    stop("You have not provided an API key or a database URL to access MongoDB.\nPlease provide an appropriate API key or URL if you want to access the database. \nIf you do not know how to access an appropriate API key or database URL, please contact the WildObs team at wildobs-support@qcif.edu.au")
  } # end double null condition

  # inspect the DB url that was provided to make sure its legit if we are NOT using an API
  if(use_api == FALSE){
    # if the db_url is null
    if(is.null(db_url)){
      # stop the function and give an error.
      stop("You have not provided a URL to access MongoDB.\nPlease provide an appropriate URL if you want to access the database.")
    } # end null check
    ## Make sure the db URL they provide matches the basic pattern
    pattern <- "^mongodb:\\/\\/[^:@]+:[^:@]+@[^\\/]+:\\d+(\\/[a-zA-Z0-9._-]+)?(\\/\\?.*)?$"
    if (!grepl(pattern, db_url)) {
      stop("The URL to access the database must be a valid MongoDB URI of the follwoing format: \n'mongodb://user:password@host:port/dbname'")
    } # end pattern check
  } # end API check

  ## Access the metadata from the DB, but do it via API key, or not
  if(use_api){
    # Send a GET request using the URL, API key, and only query for the metadata
    response <- httr::GET(
      "https://camdbapi.wildobs.org.au/find", # hard code API url
      httr::add_headers("X-API-Key" = api_key),
      query =     list(
        collection = "metadata"
      )
    )
    ## inspect status code, only 200 means success.
    if (httr::status_code(response) != 200) {
      stop("Failed to retrieve metadata from API. Status code: ", httr::status_code(response))
    }

    ## Extract the raw text from the response
    raw_text <- httr::content(response, "text", encoding = "UTF-8")
    ## then parse it from JSON
    parsed <- jsonlite::fromJSON(raw_text)

    # API appears to be structured where data is the first element in the returned list
    # and the 1st nested element is the number of rows, and the 2nd nested element is the data
    # 2nd element of the list appears to be the status code.
    metadata <- parsed[[1]][[2]]  # Extract the data

  }else{
    ## access the metadata from the DB
    metadata = mongolite::mongo(db = "wildobs_camdb", collection = "metadata", url = db_url)$find()
  } # end else use_api


  ## double check for closed in sharing preference AND admin credentials
  if("closed" %in% tabularSharingPreference){
    # if true, verify admin status via API keys
    if(use_api){
      if(! grepl("e95f47130dd589ca84d8f0b0a94c7d3f223d7", api_key)){
        # only checking middle chunk of api key^^^, not full thing
        # if not, remove closed from the preferences w/ a warning
        tabularSharingPreference = tabularSharingPreference[tabularSharingPreference != "closed"]
        # and give an update
        warning("You have requested data with closed data sharing agreements but have not provided admin credentials to access this data, so these projects have been removed from your query")
      } # end api key grepl
      # but if were not using an API key,
      }else{
        # check the db_url for admin info
        if(! grepl("admin", db_url)){
          # if not, remove closed from the preferences w/ a warning
          tabularSharingPreference = tabularSharingPreference[tabularSharingPreference != "closed"]
          # and give an update
          warning("You have requested data with closed data sharing agreements but have not provided admin credentials to access this data, so these projects have been removed from your query")
        } # end db_url grepl
      } # end else admin api key check
    } # end use api

  ## and immediately thin metadata to include the specific sharing preferences
  metadata = metadata[metadata$WildObsMetadata$tabularSharingPreference %in% tabularSharingPreference, ]

  #
  ##
  ### Create a true/false column in the metadata to determine if the embargo period has passed
  # First, convert unknown embargo periods based on sharing preference
  # open data has no embargo
  metadata$WildObsMetadata$embargoPeriodMonths[metadata$WildObsMetadata$embargoPeriodMonths == "unknown" &
                                                 metadata$WildObsMetadata$tabularSharingPreference == "open"] = 0
  # closed and partial has longest embargo
  metadata$WildObsMetadata$embargoPeriodMonths[metadata$WildObsMetadata$embargoPeriodMonths == "unknown" &
                                                 metadata$WildObsMetadata$tabularSharingPreference %in% c("partial", "closed")] = 48

  #now calculate when the date the emabrgo is done
  metadata$embargo_end <- lubridate::add_with_rollback(
    # take the created date
    as.POSIXct(metadata$created),
    # and add the embargo months
    months(as.integer(metadata$WildObsMetadata$embargoPeriodMonths)),
    # while keeping valid last day (e.g., Jan 31 -> Feb 28/29)
    roll_to_first = FALSE,
    # and we only care about dates, not time.
    preserve_hms = FALSE
  )
  # and create a new T/F column if the embargo date is past today
  metadata$embargo_pass = metadata$embargo_end < Sys.time()

  ## update metadata to onclude only data ready to be shared,
  # but allow an exception for admin access!
  if(use_api){
    if(grepl("e95f47130dd589ca84d8f0b0a94c7d3f223d7", api_key)){
      # if admin, convert all embargo pass to true
      metadata$embargo_pass = TRUE
    } # end admin api
  }else{
    if(grepl("admin", db_url)){
      # if admin, convert all embargo pass to true
      metadata$embargo_pass = TRUE
    } # end admin db url
  } # end else use_api condition

  ## now thin
  metadata = metadata[metadata$embargo_pass == TRUE, ]

  #
  ##
  ### Spatial query ----

  # for testing
  # spatial = list(xmin = 145.0, xmax = 147.0, ymin = -16.0,  ymax = -20.0)

  ## Make sure there is spatial information!
  if(exists("spatial", inherits = FALSE) && !is.null(spatial) && length(spatial) > 0){
    # Extract the bounding box data frame
    bbox_df <- metadata$spatial$bbox %>%
      tidyr::pivot_longer(cols = everything(), names_to = "locationName", values_to = "bbox") %>%
      dplyr::mutate(row_id = rep(seq_len(nrow(metadata)), each = ncol(metadata$spatial$bbox))) %>%  # Track original row ID
      dplyr::filter(purrr::map_lgl(bbox, ~ !is.null(.x) && !all(is.na(.x)))) %>%  # Remove NULL or all-NA values
      dplyr::mutate(
        xmin = purrr::map_dbl(bbox, ~ .x$xmin[1]),
        ymin = purrr::map_dbl(bbox, ~ .x$ymin[1]),
        xmax = purrr::map_dbl(bbox, ~ .x$xmax[1]),
        ymax = purrr::map_dbl(bbox, ~ .x$ymax[1])
      ) %>%
      dplyr::select(locationName, row_id, xmin, ymin, xmax, ymax)  # Keep only relevant columns

    # grab relevant project IDs
    bbox_df <- bbox_df %>% dplyr::mutate(id = metadata$id[row_id])

    # Filter bbox_df to remove any non-overlapping boxes from the spatial extent
    bbox_df_filtered <- bbox_df[
      !(bbox_df$xmax < spatial$xmin |   # bbox entirely west of xmin
          bbox_df$xmin > spatial$xmax |   # bbox entirely east of xmax
          bbox_df$ymax < spatial$ymin |   # bbox entirely south of ymin
          bbox_df$ymin > spatial$ymax),   # bbox entirely north of ymax
    ]

    # save the relevant project ids
    proj_ids_spatial <- unique(bbox_df_filtered$id)

    ## clean up for testing
    # rm(bbox_df, bbox_df_filtered, spatial)
  }else{
    # but if not present, leave it as blank
    proj_ids_spatial = ""
  } # end spatial condition

  #
  ##
  ### Temporal query ----

  # for testing
  # temporal = list(minDate = as.Date("2022-01-01"), maxDate = as.Date("2025-01-01"))

  if(!missing(temporal) && !is.null(temporal) && length(temporal) > 0){
    # extract the data frame from the meta
    temporal_df = metadata$temporal
    temporal_df$timeZone = NULL # dont want this rn

    # Track row indices to associate with project ID
    temporal_df <- temporal_df %>%
      dplyr::mutate(row_id = seq_len(nrow(.))) %>%  # Create row IDs before pivoting
      tidyr::pivot_longer(cols = -row_id, names_to = "name", values_to = "dates") %>%
      tidyr::unnest_wider(dates) %>%
      dplyr::filter(!is.na(start) & !is.na(end)) %>% # Keep only non-NA values
      dplyr::mutate(start = as.Date(start), end = as.Date(end)) %>% # make sure dates are dates.
      dplyr::mutate(id = metadata$id[row_id]) %>%
      dplyr::select(id, name, start, end)

    # Filter projects that fall within the temporal range
    temporal_df_filtered <- temporal_df %>%
      dplyr::filter(
        (start >= temporal$minDate & start <= temporal$maxDate) |    # Start is within the range
          (end >= temporal$minDate & end <= temporal$maxDate) |      # End is within the range
          (start <= temporal$minDate & end >= temporal$maxDate)      # Project fully overlaps the range
      )

    # save the relevant project ids
    proj_ids_temporal <- unique(temporal_df_filtered$id)
  }else{
    # but if not present, leave it as blank
    proj_ids_temporal = ""
  } # end temporal condition

  #
  ##
  ### Taxonomic ----

  # # for testing
  # taxonomic = c("Phascolarctos cinereus", "Tachyglossus aculeatus")

  if(!missing(taxonomic) && !is.null(taxonomic) && length(taxonomic) > 0){
    # extract the data frame from metadata
    taxa = metadata$taxonomic
    # create a list to store results
    taxa_df = list()
    for(i in 1:length(taxa)){
      # extract a dataframe
      t = purrr::map_dfr(taxa[i], as.data.frame)
      # and add the id
      t$id = metadata$id[i]
      # save in the list
      taxa_df[[i]] = t
    } # end per length taxa
    rm(i, t)

    # combine into one df
    taxa_df = do.call(rbind, taxa_df)

    # subset taxa_df to the relevant species
    taxa_df_subset = taxa_df[taxa_df$scientificName %in% taxonomic, ]

    # extract project IDs for the relevant species
    proj_ids_taxa = unique(taxa_df_subset$id)
  }else{
    # but if not present, leave it as blank
    proj_ids_taxa = ""
  } # end taxonomic condition

  #
  ##
  ### Contributors ----

  # # for testing
  # contributors = c("Zachary Amir", "Grant Linley")

  if(!missing(contributors) && !is.null(contributors) && length(contributors) > 0){

    ## COME HERE, and try to incorproate ORCIDs, names, and/or emails!! currently only running w/ names
    # could detect which column to check in the contributors DF later.

    # extract the information from metadata
    cont = metadata$contributors
    # create a list to store results
    cont_df = list()
    for(i in 1:length(cont)){
      # extract a dataframe
      t = purrr::map_dfr(cont[i], as.data.frame)
      # and add the id
      t$id = metadata$id[i]
      # save in the list
      cont_df[[i]] = t
    } # end per length taxa
    rm(i, t)

    # combine into one df
    cont_df = dplyr::distinct(do.call(rbind, cont_df))

    # subset taxa_df to the relevant species
    cont_df_subset = cont_df[cont_df$title %in% contributors, ] # come here and update w/ orcids and emails too!

    # extract project IDs for the relevant species
    proj_ids_contributors = unique(cont_df_subset$id)

  }else{
    # but if not present, leave it as blank
    proj_ids_contributors = ""
  } # end per contributors

  #
  ##
  ### samplingDesign ----

  ## remember, these are the enumerated values: "simpleRandom", "systematicRandom", "clusteredRandom", "experimental", "targeted", "opportunistic"

  # # for testing
  # samplingDesign = c("simpleRandom", "opportunistic")

  if(!missing(samplingDesign) && !is.null(samplingDesign) && length(samplingDesign) > 0){
    # extract project info
    proj = metadata$project
    # and subset based on specific sampling design
    proj_sub = proj[proj$samplingDesign %in% samplingDesign, ]
    # then pull out the relevant IDs
    proj_ids_SD = unique(proj_sub$id)
  }else{
    # but if not present, leave it as blank
    proj_ids_SD = ""
  } # end sampling design condition


  #
  ##
  ### Return the updated vector of project IDs -----

  ## make a list of all relevant project IDs
  id_lists <- list(proj_ids_spatial, proj_ids_temporal, proj_ids_taxa,
                   proj_ids_contributors, proj_ids_SD)  # come here and add more as they arise!

  ## filter out empties for both length == 0 and ""
  id_lists <- Filter(function(x) length(x) > 0 && !all(x == ""), id_lists)
  ## Take the intersection of all queries
  proj_ids <- Reduce(intersect, id_lists)

  # but if there are no conditions met, provide all open and partial options
  # accommodate NO returns and NO intersections
  # if(any(proj_ids == "" | length(proj_ids) == 0)){
  if(length(proj_ids) == 0 || any(proj_ids == "")){
    # print a message
    warning("There were no matches in our database of the specific parameters provided in your function. \nThis will return an empty vector instead of any projectIDs.")
    # Make it empty
    proj_ids = "" #metadata$id
  }

  # return the vector
  proj_ids

} # end function

