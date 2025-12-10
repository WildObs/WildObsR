#' Download Data from WildObs' MongoDB and Format as Frictionless Data Packages
#'
#' This function connects to the WildObs MongoDB server, retrieves project-specific
#' metadata and data resources, and bundles them into Frictionless Data Packages
#' formatted using the camtrap DP data standard. Note, while neither the db_url or
#' api_key parameters are required, the function will return an error if at
#' least one of these parameters are not provided.
#'
#' @param db_url A character string specifying the MongoDB connection URI. This should follow the format:
#'   `'mongodb://username:password@host:port/database'`. This parameter allows
#'   users to specify their own connection string to the WildObs MongoDB instance.
#'   Defaults to `NULL`, in which case the function expects a valid `api_key` to connect.
#' @param api_key A character string specifying the API key used for authenticated access to the WildObs
#'  public API. If provided, the function will query the API instead of connecting directly to the MongoDB
#'  instance with `mongolite`. API keys grant read-only access to specific endpoints and should be kept
#'  confidential (e.g., stored in an `.Renviron` file or other secure environment variable).
#'  Defaults to `NULL`, in which case the function expects a valid `db_url` to connect directly to MongoDB.
#' @param project_ids A character vector of project IDs to retrieve from MongoDB,
#' which is generated from from a query to WildObs' MongoDB.
#' @param media A logical (TRUE/FALSE) value to include the media resource in your
#'  data package download. This is the largest spreadsheet and significantly slows
#'   down the download process, so this value defaults to FALSE.
#' @param metadata_only Logical; if TRUE, only metadata will be retrieved from
#' each data package, without downloading the associated data resources. This can
#'  significantly improve speed when only project-level information is required.
#'  Defaults to FALSE, in which case all data (metadata and tabular resources) are
#'   downloaded. Note: this only applies for data with 'open' data sharing agreements,
#'    since 'partial' will be returned with metadata-only.
#' @return A named list of Frictionless Data Packages formatted using camtrap DP, where each element corresponds to a project. To learn more about [camtrapDP, click here](https://camtrap-dp.tdwg.org/), and to learn more about [Frictionless Data Packages, click here](https://specs.frictionlessdata.io/data-package/). Note that only data with an 'open' data sharing agreement will return tabular data, while data shared with a  'partial' data sharing agreement will return only the project-level metadata. Furthermore, any species listed as threatened under the Australian Federal Government's Environment Protection and Biodiversity Conservation Act 1999 will have their locaiton data obscured.
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
#' @importFrom httr POST
#' @importFrom httr2 req_perform req_body_json req_method request
#' @importFrom curl curl_fetch_memory handle_setopt handle_setheaders new_handle
#' @importFrom stringr str_detect regex
#'
#' @author Zachary Amir
#'
#' @export
wildobs_dp_download = function(db_url = NULL, api_key = NULL, project_ids, media = FALSE, metadata_only = FALSE) {

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

  ## Begin by checking if API key or db_url was provied
  if(is.null(api_key) && is.null(db_url)){
    # stop the function and tell them to get more info!
    stop("You have not provided an API key or a database URL to access MongoDB.\nPlease provide an appropriate API key or URL if you want to access the database. \nIf you do not know how to access an appropriate API key or database URL, please contact the WildObs team at wildobs-support@qcif.edu.au")
  } # end double null condition

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

  ### Determine if we have admin credentials
  # if we are using the API,
  if(use_api){
    # check if admin string is present in the api key
    if(grepl("e95f47130dd589ca84d8f0b0a94c7d3f223d7", api_key)){
      # and if it is, we can be admin
      use_admin = TRUE
    }else{
      # but if not, we cant
      use_admin = FALSE
    }
  }else{ # end else admin api grepl
    # if not api, check for admin in the db_url
    if(grepl("admin", db_url)){
      # if present, we can be admin
      use_admin = TRUE
    }else{
      # but if not, we cant
      use_admin = FALSE
    } # end else admin db_url
  } # end else api conditon

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
    # Send a POST request using the URL, API key, and only query for the metadata
    response <-  httr::POST(#httr::GET(
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
    proj_meta$contributors = WildObsR:::convert_df_to_list(proj_meta$contributors)
    proj_meta$licenses = WildObsR:::convert_df_to_list(proj_meta$licenses)
    proj_meta$project = WildObsR:::convert_df_to_list(proj_meta$project)
    proj_meta$WildObsMetadata = WildObsR:::convert_df_to_list(proj_meta$WildObsMetadata)

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

    ## Use new chatGPT helper function to handle geoJSON + bboxes + flat bbox
    proj_meta$spatial <- WildObsR:::format_spatial_to_geojson(meta_list$spatial)

    #
    ##
    ###
    #### Extract temporal ####

    # Grab temporal DF as a list
    t = as.list(meta_list$temporal)

    # apply the function, only keeping non-NA values
    t_clean = purrr::keep(t, ~ !WildObsR:::is_empty_temporal(.x))
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
      schema = WildObsR:::reformat_schema(resources[r, "schema"])
      ## and create a new field for projectName, which must be present in all datasets
      schema$fields[[length(schema$fields) + 1]] = list(name = "projectName",
                                                        description = "This is the persistent identifier used to describe the overall datapackage, stored in dataPackage$id. This identifier is used to manage and track many different dataPackages. This value is a short url-usable and preferably human-readable name of the package. The name should be invariant, meaning that it should not change when a data package is updated.",
                                                        `skos:exactMatch` = "https://specs.frictionlessdata.io/data-package/#id",
                                                        constraits = list(required = "TRUE"),
                                                        example = "QLD_Kgari_BIOL2015_2023-24_WildObsID_0004",
                                                        type = "string")

      ### BUG IN THE DOWNLOADED DATA! COME HERE AND FIX
      # should be removed, but leaving here to stay safe
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
  # rm(b, b_tax, meta_list, proj_meta, res_list, resources,
  #   schema,t_clean, t_nested, tax, tax_list, meta,
  #    t, i,  n, na, name, r, s, final_meta, tz, raw_text)


  #
  ##
  ###
  #### Determine which projects to download, extract key resources, apply schemas, and bundle together

  ### Extract project_ids that have an open data sharing preference
  ## BUT if were admin, all projects can be included
  # First check if we are checking API or DB url for admin access
  if(use_api){
    # check if we have admin rights
    if(use_admin){
      # if so, all projects can be accessed by admin
      project_ids_query = project_ids
    }else{
      ## But if not admin, check for open projects
      project_ids_query = c() # store them here
      for(i in 1:length(formatted_metadata)){
        x = formatted_metadata[[i]]
        val = x$project_level_metadata$WildObsMetadata$tabularSharingPreference
        ## and ALSO check for published data link! COME HERE
        ## This is a temporary fix until we have proper internal PIDs.
        cit = x$project_level_metadata$bibliographicCitation
        ## verify cit is kosher (i.e. not null or zero length)
        if (is.null(cit) || length(cit) == 0 || is.null(cit[[1]])) {
          cit <- "no_citation"  # fallback if missing
        } else {
          cit <- cit[[1]]  # normal case
        }
        # check for open data sharing & a link to GBIF published data
        if(val == "open" &
           grepl("https://doi.org/10.15468", cit)){
          project_ids_query = c(project_ids_query, x$project_level_metadata$id)
        } # end open condition
      } # end per metadata
    } # end else admin api check
  }else{
    ## But if not using api keys, check for admin in the db_url
    if(use_admin){
      # all projects can be accessed by admin
      project_ids_query = project_ids
    }else{
      # check for open projects only if not admin
      project_ids_query = c() # store them here
      for(i in 1:length(formatted_metadata)){
        x = formatted_metadata[[i]]
        val = x$project_level_metadata$WildObsMetadata$tabularSharingPreference
        ## and ALSO check for published data link! COME HERE
        ## This is a temporary fix until we have proper internal PIDs.
        cit = x$project_level_metadata$bibliographicCitation
        ## verify cit is kosher (i.e. not null or zero length)
        if (is.null(cit) || length(cit) == 0 || is.null(cit[[1]])) {
          cit <- "no_citation"  # fallback if missing
        } else {
          cit <- cit[[1]]  # normal case
        }
        # check for open data sharing & a link to GBIF published data
        if(val == "open" &
           grepl("https://doi.org/10.15468", cit)){
          project_ids_query = c(project_ids_query, x$project_level_metadata$id)
        } # end open condition
      } # end per metadata
    } # end else admin condition
  } # end use api

  ### BEFORE accessing any data, check if we can return anything anyway!
  if(is.null(project_ids_query)){
    # convert metadata to TRUE
    metadata_only = TRUE
    # and let the user know about this
    message("You have requested data that is unpublished without admin credentials. \nThis will produce metadata only, not any spreadsheets.")
  }


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
    } # end else proj_id = 1 condition

    ## compile all bodies in a list
    bodies = list("deployments" = dep_body,
                  "observations" = obs_body,
                  "covariates" = cov_body)

    ### Add a condition to query metadata only or full records
    if(!isTRUE(metadata_only)){
      ## repeat for each data resource EXCEPT media
      for(ht in 1:length(bodies)){
        ### Instead of httr2, use curl package
        # Create a new curl "handle", which is like a container that will hold all the settings for your API request
        h_dep <- curl::new_handle()
        # Add HTTP headers to the handle, which are metadata that tells the server:
        # 1. The API key for authentication, and
        # 2. What type of data were querying (JSON format)
        curl::handle_setheaders(h_dep,
                                "X-API-Key" = api_key,
                                "Content-Type" = "application/json"
        )
        # Configure the request options for the handle:
        # 1. customrequest = "POST" tells it to use POST method (not GET)
        # 2. postfields = the actual data to send in the request body
        #    - jsonlite::toJSON() converts your R list (dep_body) into JSON text
        #    - auto_unbox = TRUE prevents single values from becoming arrays in JSON
        curl::handle_setopt(h_dep,
                            customrequest = "POST",
                            postfields = jsonlite::toJSON(bodies[[ht]], auto_unbox = TRUE)
        )
        # Actually send the HTTP request to the API and store the raw response in memory
        resp_raw <- curl::curl_fetch_memory(url, handle = h_dep)
        # Convert the raw response into usable R data:
        # 1. dep_resp_raw$content is raw bytes
        # 2. rawToChar() converts those bytes into readable text (JSON string)
        # 3. jsonlite::fromJSON() converts that JSON text into an R list/data frame
        data <- jsonlite::fromJSON(rawToChar(resp_raw$content))
        # then grab the data.frame from the API data
        dat = data[[1]][[2]]
        # and save the object as the relevant name
        if(names(bodies)[ht] == "deployments"){  deps = dat}
        if(names(bodies)[ht] == "observations"){ obs = dat}
        if(names(bodies)[ht] == "covariates"){   covs = dat}

      } # end per bodies

      # media, but only if true!
      if(media){

        ### Establish batch uploads for API b/c media requires too much bandwidth
        batch_size = 5000 # can explore values here, maybe allow users to toggle?

        ## give us an update about accessing media in batches
        message("Downloading media data over API connection in ", batch_size, " row batches...")
        # create a last_id vector to track what was the last mediaID pulled
        last_id <- NULL
        # store all media here
        all_media <- list()

        ## Repeat the following code until the batch upload is completed
        repeat {
          # check if there are multiple projects
          if(length(project_ids_query) == 1){
            # but if just one, specify that as one
            filt <- list(projectName = project_ids_query)
          }else{
            # buf there are multiple, save as an array
            filt <- list(projectName = list(`$in` = project_ids_query))
          } # end else project length conditon

          # if the last_id isnt null
          if (!is.null(last_id)) {
            # save the new last mediaID
            filt$mediaID <- list(`$gt` = last_id)
          } # end null last_id

          # Build a new request body w/ the updated filter
          media_body <- list(
            collection = "media",
            filter = filt,
            sort = list(mediaID = 1),
            limit = batch_size
          )

          ### Use curl to make the request (same pattern as other resources)
          # Create a new curl handle for this batch
          h_media <- curl::new_handle()
          # Add HTTP headers (API key and content type)
          curl::handle_setheaders(h_media,
                                  "X-API-Key" = api_key,
                                  "Content-Type" = "application/json"
          )
          # Configure POST request with the media_body data
          curl::handle_setopt(h_media,
                              customrequest = "POST",
                              postfields = jsonlite::toJSON(media_body, auto_unbox = TRUE)
          )

          # Try the request safely
          media_resp_raw <- try(curl::curl_fetch_memory(url, handle = h_media), silent = TRUE)

          # If it failed, print a warning and stop
          if (inherits(media_resp_raw, "try-error")) {
            warning(paste("Request failed at batch starting from mediaID:", last_id))
            break
          }

          # Convert the raw response into a dataframe using jsonlite
          data_media <- jsonlite::fromJSON(rawToChar(media_resp_raw$content))

          # Extract the actual media table from the nested list
          media_part <- data_media[[1]][[2]]

          # Stop if this batch is empty (means we reached the end)
          if (length(media_part) == 0) break

          # Store the batch back in the list
          all_media[[length(all_media) + 1]] <- media_part

          # Update last_id for next iteration
          last_id <- tail(sort(media_part$mediaID), 1)

          # Compute cumulative progress
          total_rows <- sum(vapply(all_media, nrow, integer(1)))
          # batch_rows <- nrow(media_part)
          # and let us know about it
          message("Fetched batch ", length(all_media),
                  ": ", total_rows, " total rows so far...")

        } # end per repeat

        # Combine all batches into one dataframe
        media_df <- dplyr::distinct(dplyr::bind_rows(all_media))
        # let us know its all been said and done.
        message("Finished! Total media records downloaded: ", nrow(media_df))

      } # end media condition

    }else {
      # but give us a reminder if were only accessing metadata
      message("metadata_only = TRUE → skipping data resource downloads for speed.")
    } # end elsemetadata_only condition

  }else{
    ## if not using API, stick with the old method
    # construct the project_id based query in json format
    query <- if(length(project_ids_query) == 1) {
      jsonlite::toJSON(list(projectName = project_ids_query), auto_unbox = TRUE) # Single ID
    } else {
      jsonlite::toJSON(list(projectName = list("$in" = project_ids_query)), auto_unbox = TRUE) # Multiple IDs
    }

    ### add a condition for metadata only
    if(!isTRUE(metadata_only)){
      # grab observations
      obs = mongolite::mongo(db = "wildobs_camdb", collection = "observations", url = db_url)$find(query) # filtering w/ query
      # deployments
      deps = mongolite::mongo(db = "wildobs_camdb", collection = "deployments", url = db_url)$find(query)
      # media, but only if specified
      if(media){media_df = mongolite::mongo(db = "wildobs_camdb", collection = "media", url = db_url)$find(query)}
      # covariate
      covs = mongolite::mongo(db = "wildobs_camdb", collection = "covariates", url = db_url)$find(query)

    } else {
      message("metadata_only = TRUE → skipping data resource downloads for speed.")
    }# end else metadata_only condition

  }# end else use api

  #
  ##
  ###
  #### Before we save each DP, update the osbervation schema to include taxonomic info
  ## but only if we want more than metadata
  if(!isTRUE(metadata_only)){
    # repeat for each project
    for(p in 1:length(project_ids)){
      # select one project
      proj = project_ids[p]
      # Get the existing schema
      obs_schema <- formatted_metadata[[proj]]$observations_schema
      # Specify the new columns added to obs from taxonomic metadata
      new_fields <-c("taxonID", "taxonRank", "vernacularNamesEnglish")

      # For each new field,
      for (field in new_fields) {
        # Create a simple placeholder field entry for the schema
        new_field_def <- list(
          name = field,
          type = "string",  # default to string; can update types later
          description = paste("Additional field automatically added during data integration:", field)
        )
        # Assign known metadata to recognized fields
        if (field == "taxonID") {
          new_field_def$type <- "string"
          new_field_def$description <- "Unique taxonomic identifier (e.g. ALA or GBIF verified uri)."
          new_field_def$`skos:exactMatch` <- "http://rs.tdwg.org/dwc/terms/taxonID"
        } else if (field == "taxonRank") {
          new_field_def$type <- "string"
          new_field_def$description <- "Taxonomic rank of the identified organism (e.g. species, genus)."
          new_field_def$`skos:exactMatch` <- "http://rs.tdwg.org/dwc/terms/taxonRank"
        } else if (field == "vernacularNamesEnglish") {
          new_field_def$type <- "string"
          new_field_def$description <- "Common English name of the organism, if available."
          new_field_def$`skos:exactMatch` <- "http://rs.tdwg.org/dwc/terms/vernacularName"
        }
        # Append to schema
        obs_schema$fields[[length(obs_schema$fields) + 1]] <- new_field_def
      } # end per new field
      # Replace the schema in the formatted metadata
      formatted_metadata[[proj]]$observations_schema <- obs_schema
    } # end per project_id
  } # end false metadata only condition

  # load a quick helper to safely test if a data object exists and has rows
  # which may not be the case for partial data sharing options!
  has_rows <- function(x) {
    ### Returns TRUE only if:
    #   1. the object exists,
    if (missing(x) || is.null(x)) return(FALSE)
    #   2. it's a data.frame-like object, and
    if (!is.data.frame(x)) return(FALSE)
    #   3. it has at least one row.
    nrow(x) > 0
  }

  ## save them per project
  dp_list = list() # empty list to store DPs
  for(p in 1:length(project_ids)){

    # select one project
    proj = project_ids[p]

    ## Take a safe subset of deps to see if there is anything to save here,
    ## since partial data sharing agreements will have nothing, but we dont want an error!
    safe_subset <- tryCatch({
      # if its a dataframe
      if (is.data.frame(deps)){
        # take the subset
        deps[deps$projectName == proj, , drop = FALSE]
      }else{
        # but if not, return NULL
        NULL
      }
    }, error = function(e) NULL) # and in the case of an error, return NULL too.

    ## before we get busy, check if we only want metadata
    ## or if this is a partial data sharing agreement (and therefore now rows of data!)
    if (isTRUE(metadata_only) || !has_rows(safe_subset)) {
      # if so, construct a metadata-only package with no data resources
      dp <- frictionless::create_package(formatted_metadata[[proj]]$project_level_metadata)
      # add camtrapDP to the classes
      class(dp) <- c("camtrapdp", class(dp))
      # save back in the loop
      dp_list[[p]] <- dp
      names(dp_list)[[p]] <- proj
      # and skip the rest of this loop
      next
    }

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
    if(is.na(project_timezone)){project_timezone = "UTC"} # default to UTC if NA is provided.

    # but before we save, apply schemas to make sure were good!
    # UPDATED: Pass timezone parameter to apply_schema_types for proper POSIXct datetime handling
    obs_proj = suppressWarnings(apply_schema_types(obs_proj, formatted_metadata[[proj]]$observations_schema, timezone = project_timezone))
    deps_proj = suppressWarnings(apply_schema_types(deps_proj, formatted_metadata[[proj]]$deployments_schema, timezone = project_timezone))
    if(media){media_proj = suppressWarnings(apply_schema_types(media_proj, formatted_metadata[[proj]]$media_schema, timezone = project_timezone))}
    cov_proj = suppressWarnings(apply_schema_types(cov_proj, formatted_metadata[[proj]]$covariates_schema, timezone = project_timezone))

    ## now bundle into a frictionless DP
    # use metadata to create the DP
    dp = frictionless::create_package(formatted_metadata[[proj]]$project_level_metadata)

    # Add taxonomic info to the observations to match camtrapDP data class
    taxa = WildObsR::extract_metadata(dp, "taxonomic")
    taxa$DPID = NULL # dont need the ID
    # and merge it with the observations
    obs_proj = dplyr::left_join(obs_proj, taxa, by = "scientificName")

    #
    ##
    ###
    #### Obscure sensitive species information using EPBC classifications
    ## load the species traits data
    utils::data("species_traits", envir = environment())
    species_traits <- get("species_traits", envir = environment())
    # thin to species & listings only
    traits = dplyr::select(species_traits, binomial_verified, epbc_category, epbc_location)

    # only obscure species information IF this is NOT admin credentials
    if(!use_admin){
      # thin traits down to species that match in the dataset
      traits_sub = traits[which(traits$binomial_verified %in% taxa$scientificName), ]
      # thin traits_sub to anything w/ a listing
      traits_sub = traits_sub[which(traits_sub$epbc_category != "not_listed"), ]

      ## Now ensure we are only retaining threats for species in the correct state
      # but first add state to the deployments
      deps_proj2 = suppressWarnings(WildObsR::AUS_state_locator(deps_proj)) # known warning re: inconsistent geom, thats fine.
      # grab the specific state(s)
      target_state = unique(deps_proj2$state)
      # collapse multiple states together w/ boundaries (\\b), if present
      pattern <- paste0("\\b(", paste(target_state, collapse = "|"), ")\\b")
      # further filter traits to those with matching states
      traits_matched <- traits_sub %>%
        filter(stringr::str_detect(epbc_location, stringr::regex(pattern, ignore_case = TRUE)))

      ## finally, make sure there is only one binomial_verified (duplicates can happen from sub-species listings!)
      traits_matched <- traits_matched %>%
        distinct(binomial_verified, .keep_all = TRUE)

      # make sure we actually have data left!
      if(nrow(traits_matched) > 0){
        # then check for each of these species and make corrections
        for(s in 1:nrow(traits_matched)){
          # select one sp
          sp = traits_matched$binomial_verified[s]
          ## obscure observations
          obs_proj$observationID[which(obs_proj$scientificName == sp)] = paste("obscured_for",
                                                                               traits_matched$epbc_category[which(traits_matched$binomial_verified == sp)],
                                                                               "species", sep = "_")
          obs_proj$eventID[which(obs_proj$scientificName == sp)] = paste("obscured_for",
                                                                         traits_matched$epbc_category[which(traits_matched$binomial_verified == sp)],
                                                                         "species", sep = "_")
          obs_proj$mediaID[which(obs_proj$scientificName == sp)] = paste("obscured_for",
                                                                         traits_matched$epbc_category[which(traits_matched$binomial_verified == sp)],
                                                                         "species", sep = "_")
          obs_proj$deploymentID[which(obs_proj$scientificName == sp)] = paste("obscured_for",
                                                                              traits_matched$epbc_category[which(traits_matched$binomial_verified == sp)],
                                                                              "species", sep = "_")
        } # end per threatened species
      } # end nrow condition
    } # end admin conditon

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

    ## finally, add a new 'data' slot in the data package (which is redundant from resources...)
    ## to allow the data to be recognized by camtrap DP
    dp$data <- list(
      deployments  = deps_proj,
      observations = obs_proj,
      covariates   = cov_proj,
      media        = if (exists("media_proj")){
        media_proj
      }else{
        data.frame() # give em an empty DF if they need something!
      } # end else media conditon
    )
    ## finally, add camtrapDP to the class for recognition
    class(dp) <- c("camtrapdp", class(dp))

    ## save in a list
    dp_list[[p]] = dp
    names(dp_list)[[p]] = proj

  } # end per dp

  # Now return the final list of data packages
  dp_list

} # end function
