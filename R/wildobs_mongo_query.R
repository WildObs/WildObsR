#' Query WildObs MongoDB for Relevant Project IDs
#'
#' This function queries the WildObs MongoDB database for projects matching specified spatial, temporal, taxonomic, contributor, and data-sharing criteria.
#' It extracts metadata from the database and filters projects based on bounding
#' box overlaps, temporal overlaps, species detected, contributors associated,
#' and data sharing preferences. The function also ensures that only projects that
#' have past their embargo date are shared. Only admin credentials in the db_url
#' parameter will allow users to access data with 'closed' sharing agreements
#' or projects not past their embargo date.
#'
#' The character vector of project IDs that is returned from this function is
#' then used in the function @seealso \code{\link{wildobs_dp_download}} for
#' extracting data packages from WildObs' MongoDB.
#'
#' @param db_url A character string specifying the MongoDB connection URI. This
#'  should follow the format:`'mongodb://username:password@host:port/database'`.
#'  If `NULL`, the function will check for an API key, and if the `api_key`
#'  parameter is `NULL`, the function stop with an error. This parameter allows
#'  users to specify their own connection string to the WildObs MongoDB instance.
#'  Defaults to `NULL`, in which case the function expects a valid `api_key`
#'  to connect directly to MongoDB.
#' @param api_key A character string specifying the API key used for authenticated
#'  access to the WildObspublic API. If provided, the function will query the API
#'  instead of connecting directly to the MongoDB instance with `mongolite`.
#'  API keys grant read-only access to specific endpoints and should be kept
#'  confidential (e.g., stored in an `.Renviron` file or other secure
#'  environment variable).
#'  Defaults to `NULL`, in which case the function expects a valid `db_url`
#'  to connect directly to MongoDB.
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
#' @param taxonomic A vector of species names in binomial nomenclature (i,e., Latin names),
#'  and all projects that detect any of the species listed will be returned.
#' @param samplingDesign A vector of enumerated sampling design values, and projects
#'  with the specific sampling designs will be returned. The enumerated values are:
#'  "simpleRandom", "systematicRandom", "clusteredRandom", "experimental", "targeted", & "opportunistic"
#' @param contributors A vector of identifiers for people associated with any projects. Each
#'   element can be a full name (first and last, e.g. "Zachary Amir"), an email address
#'   (e.g. "z.amir(at)uq.edu.au"), or an ORCID (e.g. "0000-0002-0113-xxxx"). The type of each
#'   value is auto-detected, so the vector can mix all three. Regardless of a person's role in
#'   a project, if any supplied identifier is found in the metadata, the relevant projects will
#'   be returned.
#' @param tabularSharingPreference A character vector specifying accepted sharing preferences.
#'  Defaults to `c("open")`, but the user can also specify 'partial' for
#'  metadata of the project. If the user provides admin DB credentials, the user
#'  can access 'closed' data, but if admin credentials have not been provided,
#'  'closed' data will be removed from the projects list.  Only projects with
#'  these preferences are returned.
#' @return A character vector of project IDs matching the specified criteria.
#' @examples
#' \dontrun{
#' # Load API key from .Renviron
#' api_key <- Sys.getenv("MY_WILDOBS_API_KEY")
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
#' relevant_projects <- wildobs_mongo_query(db_url, spatial = spatial_query,
#' temporal = temporal_query, taxonomic = taxa_query, samplingDesign = sample_query,
#' contributors = contributor_query, tabularSharingPreference = "open")
#'
#' # display the matching projects
#' print(relevant_projects)
#' }
#' @seealso
#' - [mongolite::mongo()] for database queries
#' @importFrom magrittr %>%
#' @importFrom lubridate add_with_rollback
#' @importFrom httr status_code content POST add_headers
#' @importFrom jsonlite fromJSON
#' @importFrom mongolite mongo
#'
#' @export
wildobs_mongo_query = function(db_url = NULL, api_key = NULL,
                               spatial = NULL, temporal = NULL,
                               taxonomic = NULL, samplingDesign = NULL,
                               contributors = NULL,
                               tabularSharingPreference = c("open")){
  # create an empty vector to store project IDs
  proj_ids = c()

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
    stop("You have not provided an API key or a database URL to access MongoDB.",
         "\nPlease provide an appropriate API key or URL if you want to access",
         "the database. \nIf you require a user-specific API key, please contact",
         "the WildObs team at support@wildobs.org.au")
  } # end double null condition

  # inspect the DB url that was provided to make sure its legit if we are NOT using an API
  if(!use_api){
    # if the db_url is null
    if(is.null(db_url)){
      # stop the function and give an error.
      stop("You have not provided a URL to access MongoDB.\nPlease provide an",
           "appropriate URL if you want to access the database.")
    } # end null check
    ## Make sure the db URL they provide matches the basic pattern
    pattern <- "^mongodb:\\/\\/[^:@]+:[^:@]+@[^\\/]+:\\d+(\\/[a-zA-Z0-9._-]+)?(\\/\\?.*)?$"
    if (!grepl(pattern, db_url)) {
      stop("The URL to access the database must be a valid MongoDB URI of the",
           " follwoing format: \n'mongodb://user:password@host:port/dbname'")
    } # end pattern check

    ## if we survived the pattern check, grab the db name, since it could vary
    db <- sub("^mongodb(\\+srv)?://(.*@)?[^/?]+/([^?]*).*$", "\\3", db_url)

    ### now determine which host we are using
    # extract the host
    host <-  sub("^mongodb(\\+srv)?://(.*@)?([^:/?]+).*$", "\\3", db_url)
    # if the PROD host is present,
    if(host == "REDACTED_HOST"){
      ## run a test to check if were connected to the VPN
      # first craft a quick ping query
      sep <- if (grepl("\\?", db_url)) "&" else "?" # carefully extract the separator
      uri_test <- paste0(db_url, sep, "serverSelectionTimeoutMS=", 3000) # quick time out

      ## wrap in a try-catch so whole function doesn't fail
      ok <- tryCatch({
        # form the connection
        con <- mongolite::mongo(collection = "metadata", db = db, url = uri_test)
        # run a ping as the cheapest round-trip to the server
        con$run('{"ping": 1}')
        # then disconnect
        con$disconnect()
        # and return a TRUE result
        TRUE
      },
      # but save the error if we dont get a connection
      error = function(e) FALSE)

      ## if we did NOT get a successful connection, stop the function
      if(!ok){
        stop("You have provided the production MongoDB URL, but the function ",
             "cannot form a connection.\nPlease ensure you have logged into the ",
             "WildObs VPN before connecting to the database. \nIf you require ",
             "help connecting to the WildObs WireGuard VPN, please contact us at",
             " support@wildobs.org.au")
      }else{
        ## but if this test connection is ok, then we have admin rights
        use_admin <- TRUE
      } # end else test_conn results
    }else{
      ## but if they are NOT providing the PROD URL, then we dont have admin rights
      use_admin = FALSE
    } # end else host condition
  } # end API check
  ### COME HERE, will there be any conditions where an API key will result in admin use?
  ### currently not, but that is subject to change.

  ## Access the metadata from the DB, but do it via API key, or not
  if(use_api){
    # Send a POST request to API URL w/ the key, and query for the metadata
    response <- httr::POST(
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
    metadata = mongolite::mongo(db = db, collection = "metadata", url = db_url)$find()
  } # end else use_api

  ## double check for closed in sharing preference AND admin
  if("closed" %in% tabularSharingPreference){
    # if true, check if we have admin status
    if(!use_admin){
      # if not, remove closed from the preferences w/ a warning
      tabularSharingPreference = tabularSharingPreference[tabularSharingPreference != "closed"]
      # and give an update
      warning("You have requested data with closed data sharing agreements but",
              "have not provided admin credentials to access this data, so these",
              "projects have been removed from your query")
    } # end use_admin condition
  } # end closed data check

  ## and immediately thin metadata to include the specific sharing preferences
  metadata = metadata[metadata$WildObsMetadata$tabularSharingPreference %in% tabularSharingPreference, ]

  #
  ##
  ### Spatial query ----

  # # for testing
  # spatial = list(xmin = 112.9, ymin = -43.7, xmax = 153.6, ymax = -9.1) # all of AUS

  ## Make sure there is spatial information!
  if(exists("spatial", inherits = FALSE) && !is.null(spatial) && length(spatial) > 0){
    # Extract the bounding box data frame using custom utils.R function
    bbox_df <- WildObsR:::extract_spatial_bboxes(metadata) # handles flexible spatial data formats.

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
  # contributors = c("0000-0002-8398-2059", "Grant Linley")

  if(!missing(contributors) && !is.null(contributors) && length(contributors) > 0){

    ## COME HERE, and try to incorproate ORCIDs, names, and/or emails!! currently only running w/ names
    # could detect which column to check in the contributors DF later.

    # Helper: coerce any column (list, data.frame, empty, nested) into a clean
    # character vector. Empty/NULL entries become NA so row counts stay consistent.
    # Written by Claude Opus 4.8
    clean_chr <- function(x) {
      # if it's a list-column, flatten each element to a single string (or NA)
      if (is.list(x)) {
        x <- vapply(x, function(el) {
          if (length(el) == 0 || is.null(el)) {
            NA_character_
          } else {
            # collapse multi-value entries into one string; adjust separator as needed
            paste(unlist(el), collapse = "; ")
          }
        }, character(1))
      }
      # if it came through with zero length, return a single NA
      if (length(x) == 0) return(NA_character_)
      # final coercion to character
      as.character(x)
    }

    # extract the information from metadata
    cont = metadata$contributors
    # create a list to store results
    cont_df = list()
    for(i in seq_along(cont)){
      # extract a dataframe
      t = purrr::map_dfr(cont[i], as.data.frame)
      # and add the id
      t$id = metadata$id[i]
      # standardize the critical columns to clean character vectors
      # (handles list-columns, nested values, and zero-length/empty fields)
      t$title <- clean_chr(t$title)
      t$email <- clean_chr(t$email)
      t$path  <- clean_chr(t$path)
      # thin to CRITICAL columns
      t = dplyr::select(t, title, email, path, id)
      # save in the list
      cont_df[[i]] = t
    } # end per length taxa
    rm(i, t, clean_chr)

    # combine into one df
    cont_df = dplyr::distinct(dplyr::bind_rows(cont_df))

    # Classify each user-supplied contributor value by its format
    # - ORCID: 16 digits in groups of 4, separated by hyphens, last char may be X
    # - email: contains an @ with text either side
    # - otherwise: treat as a name (matched against title)
    # Author: Claude Opus 4.8
    classify_contributor <- function(x) {
      dplyr::case_when(
        grepl("^\\d{4}-\\d{4}-\\d{4}-\\d{3}[0-9X]$", x) ~ "orcid",
        grepl("^[^@\\s]+@[^@\\s]+\\.[^@\\s]+$",      x) ~ "email",
        TRUE                                            ~ "name"
      )
    }
    # normalize: trim whitespace, and lowercase emails/names for forgiving matching
    # (ORCIDs are already canonical, but lowercasing the X is harmless to guard against)
    norm <- function(x) tolower(trimws(x))

    # tag each requested contributor with its type
    types <- classify_contributor(contributors)

    # build a logical index: a normalized row matches if its title, email, OR ORCID
    # appears in the correspondingly-typed subset of the requested values
    match_idx <-
      norm(cont_df$title) %in% norm(contributors[types == "name"]) |
      norm(cont_df$email) %in% norm(contributors[types == "email"]) |
      norm(cont_df$path)  %in% norm(contributors[types == "orcid"])

    # subset to matching contributors
    cont_df_subset <- cont_df[match_idx, ]

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
    warning("There were no matches in our database of the specific parameters",
            "provided in your function. \nThis will return an empty vector",
            "instead of any projectIDs.")
    # Make it empty
    proj_ids = "" #metadata$id
  }

  # return the vector
  proj_ids
} # end function

