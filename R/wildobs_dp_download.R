#' Download Data from WildObs' MongoDB and Format as Frictionless Data Packages
#'
#' This function connects to the WildObs MongoDB server, retrieves project-specific
#' metadata and data resources, and bundles them into Frictionless Data Packages formatted using the camtrap DP data standard.
#'
#' @param db_url A character string specifying the MongoDB connection URI. This should follow the format:
#'   `'mongodb://username:password@host:port/database'`. If `NULL`, the function will stop with an error.
#'   This parameter allows users to specify their own connection string to the WildObs MongoDB instance.
#' @param api_key A character string specifying the API key used for authenticated access to the WildObs
#'  public API. If provided, the function will query the API instead of connecting directly to the MongoDB
#'  instance with `mongolite`. API keys grant read-only access to specific endpoints and should be kept
#'  confidential (e.g., stored in an `.Renviron` file or other secure environment variable).
#'  Defaults to `NULL`, in which case the function expects a valid `db_url` to connect directly to MongoDB.
#' @param project_ids A character vector of project IDs to retrieve from MongoDB, which is generated from from a query to WildObs' MongoDB.
#' @param media A logical (TRUE/FALSE) value to include the media resource in your data package download. This is the largest spreadsheet and significantly slows down the download process, so this value defaults to FALSE.
#' @return A named list of Frictionless Data Packages formatted using camtrap DP, where each element corresponds to a project. To learn more about [camtrapDP, click here](https://camtrap-dp.tdwg.org/), and to learn more about [Frictionless Data Packages, click here](https://specs.frictionlessdata.io/data-package/). Note that only data with an 'open' data sharing agreement will return tabular data, while data shared with a  'partial' data sharing agreement will return only the project-level metadata.
#' @details
#' The function performs the following steps:
#' 1. Connects to MongoDB using read-only credentials.
#' 2. Retrieves project-level metadata and formats it to match the Frictionless Data Package structure.
#' 3. Queries the `deployments`, `observations`, and `media` collections for the requested projects.
#' 4. Applies schemas to ensure data integrity.
#' 5. Creates Frictionless Data Packages and returns them as a list.
#'
#'  @seealso
#'   \code{\link{convert_df_to_list}} for formatting metadata,
#'   \code{\link{clean_list_recursive}} for removing NULL and empty elements,
#'   \code{\link{reformat_fields}}, \code{\link{reformat_schema}}, and \code{\link{apply_schema_types}} for processing and applying schema fields,
#'   \code{\link{is_empty_spatial}} and \code{\link{is_empty_temporal}} for removing empty spatio-temporal fields.
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' # Load the general use WildObs API key
#' api_key <- "f4b9126e87c44da98c0d1e29a671bb4ff39adcc65c8b92a0e7f4317a2b95de83"
#'
#' # Load relevant project ids
#' project_ids <- c("QLD_Kgari_BIOL2015_2023-24_WildObsID_0004",
#'                  "QLD_Kgari_potoroos_Amir_2022_WildObsID_0003")
#'
#' dp_list <- wildobs_dp_download(project_ids)
#'
#' # Access the data for a specific project
#' dp_list[["QLD_Kgari_BIOL2015_2023-24_WildObsID_0004"]]
#' }
#'
#' @importFrom mongolite mongo
#' @importFrom frictionless add_resource create_package
#' @importFrom jsonlite toJSON
#' @importFrom purrr map keep
#' @importFrom lutz tz_lookup_coords
#' @importFrom httr2 req_perform req_body_json req_method request
#'
#' @author Zachary Amir
#'
#' @export
wildobs_dp_download = function(db_url = NULL, api_key = NULL, project_ids, media = FALSE) {

  ## read in environment file with confidential DB access info
  # readRenviron("config_private/.Renviron.prod.ro") # remote version
  # readRenviron("config_private/.Renviron.admin.api") # admin api key
  ### NOTE: need to provide a read-only to open/partial shared projects here!!
  # This MUST be done before going public!

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
  #
  # # grab the api key
  # api_key = Sys.getenv("API_KEY")

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
    response <- GET(
      "https://camdbapi.wildobs.org.au/find", # hard code API url
      add_headers("X-API-Key" = api_key),
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


  #
  ##
  ### Define a chatGPT helper function to better extract spatial information
  format_spatial_to_geojson <- function(spatial_obj) {
    # Validate input
    if (!is.data.frame(spatial_obj) || !"bbox" %in% names(spatial_obj))
      return(NULL)

    # extract bounding box as a data frame
    bbox_df <- spatial_obj$bbox
    if (!is.data.frame(bbox_df)) return(NULL)

    # Prepare an empty bbox container
    bbox_out <- list()

    # Iterate over each location column (each is a bbox entry)
    for (loc_name in names(bbox_df)) {
      val <- bbox_df[[loc_name]][[1]]  # extract first (and only) cell value

      # Skip if missing or NULL
      if (is.null(val) || (is.character(val) && val == "NULL")) next

      # --- CASE 1: already a data.frame with xmin/ymin/xmax/ymax
      if (is.data.frame(val) && all(c("xmin", "ymin", "xmax", "ymax") %in% names(val))) {
        bbox_out[[loc_name]] <- list(list(
          xmin = val$xmin[1],
          ymin = val$ymin[1],
          xmax = val$xmax[1],
          ymax = val$ymax[1]
        ))
        next
      }

      # --- CASE 2: character string like "152.0796, -27.7047, 152.0934, -27.6972"
      if (is.character(val)) {
        nums <- suppressWarnings(as.numeric(strsplit(val, "\\s*,\\s*")[[1]]))
        if (length(nums) == 4 && all(is.finite(nums))) {
          bbox_out[[loc_name]] <- list(list(
            xmin = nums[1],
            ymin = nums[2],
            xmax = nums[3],
            ymax = nums[4]
          ))
        }
        next
      }

      # --- CASE 3: numeric vector already (rare case)
      if (is.numeric(val) && length(val) == 4) {
        bbox_out[[loc_name]] <- list(list(
          xmin = val[1],
          ymin = val[2],
          xmax = val[3],
          ymax = val[4]
        ))
        next
      }
    }

    # If we collected nothing valid, return NULL
    if (length(bbox_out) == 0) return(NULL)

    # Construct final geoJSON-style list
    list(
      type = "polygon",
      bbox = bbox_out
    )
  } # end function

  #
  ##
  ###
  #### Begin to reformat the metadata to fit back into datapackage.json format
  formatted_metadata = list() # store results here!
  for(i in 1:length(project_ids)){

    ## grab one name
    name = project_ids[i]
    ## and the matching metadata
    meta = metadata[metadata$id == name,] # must produce one row
    ## verify there is only one row
    if(nrow(meta)>1){print(paste("The project name:", name, "has multiple rows of metadata. This is not good and means repeated project names which violates our database rules. Please inspect manually!"))}

    ## convert to a list
    meta_list = as.list(meta)

    #
    ##
    ###
    #### Extract project-level metadata ####

    ## extract relevant project-level metadata
    proj_meta = meta_list[names(meta_list) %in% c("profile","name","created","title",
                                                  "contributors","description","version",
                                                  "keywords","image","homepage","sources",
                                                  "licenses","bibliographicCitation", "directory",
                                                  "coordinatePrecision","relatedIdentifiers",
                                                  "references","id","project","WildObsMetadata")]

    # use custom function where needed
    proj_meta$contributors = convert_df_to_list(proj_meta$contributors)
    proj_meta$licenses = convert_df_to_list(proj_meta$licenses)
    proj_meta$project = convert_df_to_list(proj_meta$project)
    proj_meta$WildObsMetadata = convert_df_to_list(proj_meta$WildObsMetadata)

    ## apply a few quick fixes to unlist or list things
    proj_meta$keywords = unlist(proj_meta$keywords)
    proj_meta$homepage = unlist(proj_meta$homepage)
    proj_meta$relatedIdentifiers = unlist(proj_meta$relatedIdentifiers)
    proj_meta$references = proj_meta$references[[1]] # come here, this might change!
    proj_meta$sources = as.list(proj_meta$sources[1, ])

    # "temporal","spatial", and "taxonomic" need special attention!
    ## resources is the specific schema for each resource (figured out below).

    #
    ##
    ###
    #### Extract spaital ####

    # # first grab spatial df
    # s = as.list(meta_list$spatial)
    #
    # # Keep only non-null bounding boxes
    # s_clean <- purrr:::keep(s$bbox, ~ !is_empty_spatial(.x))
    #
    # # convert clean list (no null) into nested structure to match camtrapDP
    # s_nested = purrr:::map(convert_df_to_list(s_clean), function(bbox) list(bbox))[[1]]
    #
    # # Save cleaned spatial data
    # proj_meta$spatial = list(
    #   type = s$type,                # Preserve type ("polygon")
    #   bbox = s_nested[[1]]          # Store formatted bounding boxes
    # )

    ## Use new chatGPT helper function to handle geoJSON + bboxes + flat bbox
    proj_meta$spatial <- format_spatial_to_geojson(meta_list$spatial)

    #
    ##
    ###
    #### Extract temporal ####

    # Grab temporal DF as a list
    t = as.list(meta_list$temporal)

    # apply the function, only keeping non-NA values
    t_clean = purrr::keep(t, ~ !is_empty_temporal(.x))
    # verify timezone is present
    if(is.null(t_clean$timeZone) || t_clean$timeZone == ""){
      # assume timezone is NA then
      tz = NA
      # grab timezone from spatial informaiton
      if (!is.null(proj_meta$spatial) && !is.null(proj_meta$spatial$bbox)) {
        # Extract all numeric lat/lon pairs from nested bbox list
        bbox_vals <- unlist(proj_meta$spatial$bbox, recursive = TRUE, use.names = FALSE)
        bbox_nums <- suppressWarnings(as.numeric(bbox_vals))
        bbox_nums <- bbox_nums[is.finite(bbox_nums)]

        ## verify there are at least 4 bbox coordinates
        if (length(bbox_nums) >= 4) {
          xmin <- bbox_nums[1]; ymin <- bbox_nums[2]
          xmax <- bbox_nums[3]; ymax <- bbox_nums[4]
          # take the average
          lat <- mean(c(ymin, ymax), na.rm = TRUE)
          lon <- mean(c(xmin, xmax), na.rm = TRUE)
          # and safely feed it into coord tz look up
          tz <- tryCatch(
            lutz::tz_lookup_coords(lat, lon, method = "accurate"),
            error = function(e) NA
          )
        } # end length 4 condition

      } # end spatial check conditon

      # then save the Tz
      t_clean$timeZone = tz
    } #end timeZone presence conditon
    # extract tzone
    tz = t_clean$timeZone

    # Convert dataframe format into nested named lists
    t_nested = purrr::map(t_clean[names(t_clean) != "timeZone"], function(df) {
      list(
        timeZone = tz,                      # save timezone as a character
        start = as.character(df$start[1]),  # Extract first row start date
        end = as.character(df$end[1])       # Extract first row end date
      )
    })

    # save it in the project metadata
    proj_meta$temporal = t_nested

    #
    ##
    ###
    #### Extract taxonomic ####

    ## First, grab taxonomy DF
    tax = meta_list$taxonomic[[1]]

    tax_list = list() # store formatted taxonomy here
    ## repeat for each species
    for(s in 1:nrow(tax)){

      # select one species
      b = tax[s, ]

      # store results per sp here
      b_tax = list()
      ## extract information to make a new taxonomy list
      for(n in 1:length(names(b))){
        # grab one column
        na = names(b)[n]
        # and add unique values to the list
        b_tax[[n]] = unique(b[,na][[1]])
        names(b_tax)[n] = na
      } # end per names b

      # save b_tax in the larger list
      tax_list[[s]] = b_tax

    } # end per tax row

    ## now save the full taxonomy in the project_metadata
    proj_meta$taxonomic = tax_list

    #
    ##
    ###
    #### Extract schema per resource ####

    ## First, grab the resources from the overall list
    resources = meta_list$resources[[1]]

    ## and repeat per resource
    res_list = list() # store results here
    for(r in 1:nrow(resources)){

      # begin extracting the schema per resource
      schema = reformat_schema(resources[r, "schema"])
      ## and create a new field for projectName, which must be present in all datasets
      schema$fields[[length(schema$fields) + 1]] = list(name = "projectName",
                                                        description = "This is the persistent identifier used to describe the overall datapackage, stored in dataPackage$id. This identifier is used to manage and track many different dataPackages. This value is a short url-usable and preferably human-readable name of the package. The name should be invariant, meaning that it should not change when a data package is updated.",
                                                        `skos:exactMatch` = "https://specs.frictionlessdata.io/data-package/#id",
                                                        constraits = list(required = "TRUE"),
                                                        example = "QLD_Kgari_BIOL2015_2023-24_WildObsID_0004",
                                                        type = "string")

      ### BUG IN THE DOWNLOADED DATA! COME HERE AND FIX
      ## not sure where this got in, but "dataSource","UTM_zone","X","Y","state" are present in schema
      # but they should be deleted/removed!
      if(resources[["name"]][r] == "deployments"){
        schema$fields = purrr::discard(schema$fields, ~ .x$name %in% c("dataSource","UTM_zone","X","Y","state"))
      }

      # and save in the res_list
      res_list[[r]] =  schema # res
      names(res_list)[r] = unique(resources[r, ]$name)
    } # end per r resources

    ## bundle it all up into a clear list, including media or not
    if(media){
      final_meta = list("project_level_metadata" = proj_meta,
                        "deployments_schema" = res_list$deployments,
                        "observations_schema" = res_list$observations,
                        "media_schema" = res_list$media,
                        "covariates_schema" = res_list$covariates)
    }else{
      final_meta = list("project_level_metadata" = proj_meta,
                        "deployments_schema" = res_list$deployments,
                        "observations_schema" = res_list$observations,
                        # "media_schema" = res_list$media,
                        "covariates_schema" = res_list$covariates)
    }

    # and save all the metadata in the final list
    formatted_metadata[[i]] = final_meta
    names(formatted_metadata)[i] = name


  } # end per project name
  # for testing
  # rm(b, b_tax, meta_list, proj_meta, res_list, resources, meta, xmax, ymax, xmin, ymin,
  #    s_clean, s_nested, schema,t_clean, t_nested, tax, tax_list,
  #    t, i,  n, na, name, r, s, final_meta, tz, lat, lon, pattern, bbox)


  #
  ##
  ###
  #### Extract key resources, apply schemas, and bundle together

  ### Extract project_ids that have an open data sharing preference
  ## BUT if were admin, all projects can be included
  # First check if we are checking API or DB url for admin access
  if(use_api){
    # check if admin string is present in the api key
    if(grepl("e95f47130dd589ca84d8f0b0a94c7d3f223d7", api_key)){
      # only checking middle chunk of api key^^^, not full thing
      # all projects can be accessed by admin
      project_ids_query = project_ids
    }else{
      ## But if not admin api, check for open projects
      project_ids_query = c() # store them here
      for(i in 1:length(formatted_metadata)){
        x = formatted_metadata[[i]]
        val = x$project_level_metadata$WildObsMetadata$tabularSharingPreference
        if(val == "open"){
          project_ids_query = c(project_ids_query, x$project_level_metadata$id)
        } # end open condition
      } # end per metadata
    } # end else admin api check
  }else{
    ## But if not using api keys, check for admin in the db_url
    if(grepl("admin", db_url)){
      # all projects can be accessed by admin
      project_ids_query = project_ids
    }else{
      # check for open projects only if not admin
      project_ids_query = c() # store them here
      for(i in 1:length(formatted_metadata)){
        x = formatted_metadata[[i]]
        val = x$project_level_metadata$WildObsMetadata$tabularSharingPreference
        if(val == "open"){
          project_ids_query = c(project_ids_query, x$project_level_metadata$id)
        } # end open condition
      } # end per metadata
    } # end else admin condition
  } # end use api

  ### Construct an API query or DB_url query and download the data
  if(use_api){
    ## assign the API URL
    url <- "https://camdbapi.wildobs.org.au/find"
    # first determine if we have one or multiple project_ids
    if(length(project_ids_query) == 1){
      ## specify deployments parameters
      dep_body <- list(
        collection = "deployments",
        filter = list(projectName = project_ids_query))
      ## Observations
      obs_body = list(
        collection = "observations",
        filter = list(projectName = project_ids_query))
      ## Covariates
      cov_body = list(
        collection = "covariates",
        filter = list(projectName = project_ids_query))
      ## Media
      media_body = list(
        collection = "media",
        filter = list(projectName = project_ids_query))
    }else{
      ## Specify deployments parameters
      dep_body <- list(
        collection = "deployments",
        filter = list(
          projectName = list(`$in` = project_ids_query)))
      ## Observations
      obs_body <- list(
        collection = "observations",
        filter = list(
          projectName = list(`$in` = project_ids_query)))
      ## covariates
      cov_body <- list(
        collection = "covariates",
        filter = list(
          projectName = list(`$in` = project_ids_query)))
      ## Media
      media_body <- list(
        collection = "media",
        filter = list(
          projectName = list(`$in` = project_ids_query)))
    } # end else proj_id = 1 condition

    ## Use httr2 functions to create a query for the url for each resource
    # deployments
    dep_req <- httr2::request(url) |>
      # Ensure the endpoint is GET-only
      httr2::req_method("GET") |>
      # assign headers
      httr2::req_headers("X-API-Key" = api_key) |>
      # put collection + filter in JSON body
      httr2::req_body_json(dep_body)
    # observations
    obs_req <- httr2::request(url) |>
      # Ensure the endpoint is GET-only
      httr2::req_method("GET") |>
      # assign headers
      httr2::req_headers("X-API-Key" = api_key) |>
      # put collection + filter in JSON body
      httr2::req_body_json(obs_body)
    # Covariates
    cov_req <- httr2::request(url) |>
      # Ensure the endpoint is GET-only
      httr2::req_method("GET") |>
      # assign headers
      httr2::req_headers("X-API-Key" = api_key) |>
      # put collection + filter in JSON body
      httr2::req_body_json(cov_body)
    # media
    media_req <- httr2::request(url) |>
      # Ensure the endpoint is GET-only
      httr2::req_method("GET") |>
      # assign headers
      httr2::req_headers("X-API-Key" = api_key) |>
      # put collection + filter in JSON body
      httr2::req_body_json(media_body)

    ## Now preform requests to access the data for each resource
    # deployments
    dep_resp <- httr2::req_perform(dep_req)
    # extract the body text string and then convert from JSON
    data_dep <- jsonlite::fromJSON(httr2::resp_body_string(dep_resp, encoding = "UTF-8"),
                                   simplifyVector = TRUE)
    # then grab the data.frame from the API data
    deps = data_dep[[1]][[2]]

    # observations
    obs_resp <- httr2::req_perform(obs_req)
    # extract the body text string and then convert from JSON
    data_obs <- jsonlite::fromJSON(httr2::resp_body_string(obs_resp, encoding = "UTF-8"),
                                   simplifyVector = TRUE)
    # then grab the data.frame from the API data
    obs = data_obs[[1]][[2]]
    # covariates
    cov_resp <- httr2::req_perform(cov_req)
    # extract the body text string and then convert from JSON
    data_cov <- jsonlite::fromJSON(httr2::resp_body_string(cov_resp, encoding = "UTF-8"),
                                   simplifyVector = TRUE)
    # then grab the data.frame from the API data
    covs = data_cov[[1]][[2]]
    # media, but only if true!
    if(media){
      # make the request
      media_resp = httr2::req_perform(media_req)
      # extract the body
      data_media <- jsonlite::fromJSON(httr2::resp_body_string(media_resp, encoding = "UTF-8"),
                                       simplifyVector = TRUE)
      # take data.frame
      media_df = data_media[[1]][[2]]
      } # end media condition

  }else{
    ## if not using API, stick with the old method
    # construct the project_id based query in json format
    query <- if(length(project_ids_query) == 1) {
      jsonlite::toJSON(list(projectName = project_ids_query), auto_unbox = TRUE) # Single ID
    } else {
      jsonlite::toJSON(list(projectName = list("$in" = project_ids_query)), auto_unbox = TRUE) # Multiple IDs
    }

    # grab observations
    obs = mongolite::mongo(db = "wildobs_camdb", collection = "observations", url = db_url)$find(query) # filtering w/ query
    # deployments
    deps = mongolite::mongo(db = "wildobs_camdb", collection = "deployments", url = db_url)$find(query)
    # media, but only if specified
    if(media){media_df = mongolite::mongo(db = "wildobs_camdb", collection = "media", url = db_url)$find(query)}
    # covariate
    covs = mongolite::mongo(db = "wildobs_camdb", collection = "covariates", url = db_url)$find(query)
  }# end else use api

  ## save them per project
  dp_list = list()
  for(p in 1:length(project_ids)){

    # select one project
    proj = project_ids[p]

    # subset for specific project
    obs_proj = obs[obs$projectName == proj, ]
    deps_proj = deps[deps$projectName == proj, ]
    if(media){media_proj = media_df[media_df$projectName == proj, ]}
    cov_proj = covs[covs$projectName == proj, ]

    # Extract timezone from temporal metadata for proper datetime handling
    # Get timezone from temporal metadata to ensure all datetime columns use the correct local timezone
    project_timezone <- if(!is.null(formatted_metadata[[proj]]$project_level_metadata$temporal[[1]]$timeZone)) {
      formatted_metadata[[proj]]$project_level_metadata$temporal[[1]]$timeZone
    } else {
      "UTC"  # Fallback to UTC if timezone not specified
    }

    # but before we save, apply schemas to make sure were good!
    # UPDATED: Pass timezone parameter to apply_schema_types for proper POSIXct datetime handling
    obs_proj = suppressWarnings(apply_schema_types(obs_proj, formatted_metadata[[proj]]$observations_schema, timezone = project_timezone))
    deps_proj = suppressWarnings(apply_schema_types(deps_proj, formatted_metadata[[proj]]$deployments_schema, timezone = project_timezone))
    if(media){media_proj = suppressWarnings(apply_schema_types(media_proj, formatted_metadata[[proj]]$media_schema, timezone = project_timezone))}
    cov_proj = suppressWarnings(apply_schema_types(cov_proj, formatted_metadata[[proj]]$covariates_schema, timezone = project_timezone))


    ## and make sure the columns follow the order in the schema
    # observations
    col_order = c()
    for(i in 1:length(formatted_metadata[[proj]]$observations_schema$fields)){
      col_order = c(col_order, formatted_metadata[[proj]]$observations_schema$fields[[i]]$name)
    }
    ## re-order to match
    obs_proj = obs_proj[, col_order]
    # deployments
    col_order = c()
    for(i in 1:length(formatted_metadata[[proj]]$deployments_schema$fields)){
      col_order = c(col_order, formatted_metadata[[proj]]$deployments_schema$fields[[i]]$name)
    }
    ## re-order to match
    deps_proj = deps_proj[, col_order]
    # media
    if(media){
      col_order = c()
      for(i in 1:length(formatted_metadata[[proj]]$media_schema$fields)){
        col_order = c(col_order, formatted_metadata[[proj]]$media_schema$fields[[i]]$name)
      }
      ## re-order to match
      media_proj = media_proj[, col_order]
    }
    # covariates
    col_order = c()
    for(i in 1:length(formatted_metadata[[proj]]$covariates_schema$fields)){
      col_order = c(col_order, formatted_metadata[[proj]]$covariates_schema$fields[[i]]$name)
    }
    ## re-order to match
    cov_proj = cov_proj[, col_order]


    ## now bundle into a frictionless DP
    # use metadata to create the DP
    dp = frictionless::create_package(formatted_metadata[[proj]]$project_level_metadata)

    # Add camtrapdp class to data package for proper class identification
    # This ensures the data package is recognized as a camera trap data package following the camtrap-dp standard
    class(dp) <- c("camtrapdp", class(dp)) # COME HERE!! ZDA thinks there is more to it than just this, since media will need eventID (already present) and taxonomic info in observations.
    ## ideally we can get someone from INBO to do this cleverly.

    # Allow admin to access all data via db_url OR api_key
    is_admin <- FALSE
    if(!is.null(db_url) && grepl("admin", db_url)) {
      is_admin <- TRUE
    } else if(!is.null(api_key) && grepl("e95f47130dd589ca84d8f0b0a94c7d3f223d7", api_key)) {
      is_admin <- TRUE
    }

    if(is_admin){
      # deployments
      dp = frictionless::add_resource(package = dp,
                                      resource_name = "deployments",
                                      data = deps_proj,
                                      schema = formatted_metadata[[proj]]$deployments_schema)
      # observations
      dp = frictionless::add_resource(package = dp,
                                      resource_name = "observations",
                                      data = obs_proj,
                                      schema = formatted_metadata[[proj]]$observations_schema)
      # media
      if(media){
        dp = frictionless::add_resource(package = dp,
                                        resource_name = "media",
                                        data = media_proj,
                                        schema = formatted_metadata[[proj]]$media_schema)
      }
      # covariates
      dp = frictionless::add_resource(package = dp,
                                      resource_name = "covariates",
                                      data = cov_proj,
                                      schema = formatted_metadata[[proj]]$covariates_schema)
    }else{
      # but if not admin, only allow open data to be returned
      # only add data if there is an open data sharing agreement!
      if(formatted_metadata[[proj]]$project_level_metadata$WildObsMetadata$tabularSharingPreference == "open"){
        # deployments
        dp = frictionless::add_resource(package = dp,
                                        resource_name = "deployments",
                                        data = deps_proj,
                                        schema = formatted_metadata[[proj]]$deployments_schema)
        # observations
        dp = frictionless::add_resource(package = dp,
                                        resource_name = "observations",
                                        data = obs_proj,
                                        schema = formatted_metadata[[proj]]$observations_schema)
        # media
        if(media){
          dp = frictionless::add_resource(package = dp,
                                          resource_name = "media",
                                          data = media_proj,
                                          schema = formatted_metadata[[proj]]$media_schema)
        }
        # covariates
        dp = frictionless::add_resource(package = dp,
                                        resource_name = "covariates",
                                        data = cov_proj,
                                        schema = formatted_metadata[[proj]]$covariates_schema)

      } # end open data sharing condition
    } # end admin condition

    ## save in a list
    dp_list[[p]] = dp
    names(dp_list)[[p]] = proj

  } # end per dp

  # Now return the final list of data packages
  dp_list

} # end function
