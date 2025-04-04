#' Download Data from WildObs' MongoDB and Format as Frictionless Data Packages
#'
#' This function connects to the WildObs MongoDB server, retrieves project-specific
#' metadata and data resources, and bundles them into Frictionless Data Packages formatted using the camtrap DP data standard.
#'
#' @param project_ids A character vector of project IDs to retrieve from MongoDB, which is generated from from a query to WildObs' MongoDB.
#' @return A named list of Frictionless Data Packages formatted using camtrap DP, where each element corresponds to a project. To learn more about [camtrapDP, click here](https://camtrap-dp.tdwg.org/), and to learn more about [Frictionless Data Packages, click here](https://specs.frictionlessdata.io/data-package/)
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
#' project_ids <- c("QLD_Kgari_BIOL2015_2023-24_WildObsID_0004",
#'                  "QLD_Kgari_potoroos_Amir_2022_WildObsID_0003")
#' dp_list <- wildobs_dp_download(project_ids)
#'
#' # Access the data for a specific project
#' dp_list[["QLD_Kgari_BIOL2015_2023-24_WildObsID_0004"]]
#' }
#'
#' @import mongolite
#' @import frictionless
#' @importFrom jsonlite toJSON
#' @importFrom purrr map keep
#' @importFrom lutz tz_lookup_coords
#'
#' @author Zachary Amir
#'
#' @export
wildobs_dp_download = function(project_ids) {

  ## read in environment file with confidential DB access info
  readRenviron(".Renviron.local.ro") # local version
  # readRenviron(".Renviron.dev.ro") # remote version

  ## load information from enviromnet
  HOST <- Sys.getenv("HOST")
  PORT <- Sys.getenv("PORT")
  DATABASE <- Sys.getenv("DATABASE")
  USER <- Sys.getenv("USER")
  PASS <- Sys.getenv("PASS")

  ## combine all the information into a database-url to enable access
  db_url <- sprintf("mongodb://%s:%s@%s:%s/%s", USER, PASS, HOST, PORT, DATABASE)
  # rm(USER, PASS, HOST, PORT, DATABASE)

  ## access the metadata from the DB
  metadata = mongolite::mongo(db = "wildobs_camdb", collection = "metadata", url = db_url)$find()

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
    if(nrow(meta)>1){print(paste("The project name:", name, "has multiple rows of metadata. This is not good and means repeated project names which violates our database rules.Please inspect manually!"))}

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
    proj_meta$homepage = unlist(proj_meta$relatedIdentifiers)
    proj_meta$homepage = proj_meta$references[[1]] # come here, this might change!
    proj_meta$sources = as.list(proj_meta$sources[1, ])

    # "temporal","spatial", and "taxonomic" need special attention!
    ## resources is the specific schema for each resource (figured out below).

    #
    ##
    ###
    #### Extract spaital ####

    # first grab spatial df
    s = as.list(meta_list$spatial)

    # Keep only non-null bounding boxes
    s_clean <- purrr:::keep(s$bbox, ~ !is_empty_spatial(.x))

    # convert clean list (no null) into nested structure to match camtrapDP
    s_nested = purrr:::map(convert_df_to_list(s_clean), function(bbox) list(bbox))[[1]]

    # Save cleaned spatial data
    proj_meta$spatial = list(
      type = s$type,                # Preserve type ("polygon")
      bbox = s_nested[[1]]          # Store formatted bounding boxes
    )

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
      # if not, grab spatial information
      bbox = proj_meta$spatial$bbox
      # and extract mean lat
      lat = mean(c(bbox$ymin, bbox$ymax))
      lon = mean(c(bbox$xmin, bbox$xmax))
      # and then calculate a new one
      tz = lutz::tz_lookup_coords(lat,lon, method = "accurate")
      # then save the Tz
      t_clean$timeZone = tz
    }
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
                                                        description = "This is the same name used to describe the overall datapackage, stored in dataPackage$name. This name is used to manage and track many different dataPackages. This value is a short url-usable and preferably human-readable name of the package. The name should be invariant, meaning that it should not change when a data package is updated.",
                                                        `skos:exactMatch` = "https://specs.frictionlessdata.io/data-package/#name",
                                                        constraits = list(required = "TRUE"),
                                                        example = "QLD_Kgari_BIOL2015_2023-24",
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

    ## bundle it all up into a clear list
    final_meta = list("project_level_metadata" = proj_meta,
                      "deployments_schema" = res_list$deployments,
                      "observations_schema" = res_list$observations,
                      "media_schema" = res_list$media)

    # and save all the metadata in the final list
    formatted_metadata[[i]] = final_meta
    names(formatted_metadata)[i] = name


  } # end per project name
  # for testing
  # rm(b, b_tax, meta_list, proj_meta, res_list, resources, meta,
  #    s_clean, s_nested, schema,t_clean, t_nested, tax, tax_list,
  #    t, i,  n, na, name, r, s, final_meta, tz)


  #
  ##
  ###
  #### Extract key resources, apply schemas, and bundle together

  ## construct the project_id based query in json format
  query <- if(length(project_ids) == 1) {
    jsonlite::toJSON(list(projectName = project_ids), auto_unbox = TRUE) # Single ID
  } else {
    jsonlite::toJSON(list(projectName = list("$in" = project_ids)), auto_unbox = TRUE) # Multiple IDs
  }

  # grab observations
  obs = mongolite::mongo(db = "wildobs_camdb", collection = "observations", url = db_url)$find(query) # filtering w/ query
  # deployments
  deps = mongolite::mongo(db = "wildobs_camdb", collection = "deployments", url = db_url)$find(query)
  # media
  media = mongolite::mongo(db = "wildobs_camdb", collection = "media", url = db_url)$find(query)
  # covariate
  # covs = mongolite::mongo(db = "wildobs_camdb", collection = "covariates", url = db_url)$find(query) # do this when ready!

  ## save them per project
  dp_list = list()
  for(p in 1:length(project_ids)){

    # select one project
    proj = project_ids[p]

    # subset for specific project
    obs_proj = obs[obs$projectName == proj, ]
    deps_proj = deps[deps$projectName == proj, ]
    media_proj = media[media$projectName == proj, ]

    # but before we save, apply schemas to make sure were good!
    obs_proj = suppressWarnings(apply_schema_types(obs_proj, formatted_metadata[[proj]]$observations_schema))
    deps_proj = suppressWarnings(apply_schema_types(deps_proj, formatted_metadata[[proj]]$deployments_schema))
    media_proj = suppressWarnings(apply_schema_types(media_proj, formatted_metadata[[proj]]$media_schema))

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
    col_order = c()
    for(i in 1:length(formatted_metadata[[proj]]$media_schema$fields)){
      col_order = c(col_order, formatted_metadata[[proj]]$media_schema$fields[[i]]$name)
    }
    ## re-order to match
    media_proj = media_proj[, col_order]


    ## now bundle into a frictionless DP
    dp = frictionless::create_package(formatted_metadata[[proj]]$project_level_metadata)
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
    dp = frictionless::add_resource(package = dp,
                                    resource_name = "media",
                                    data = media_proj,
                                    schema = formatted_metadata[[proj]]$media_schema)
    ## save in a list
    dp_list[[p]] = dp
    names(dp_list)[[p]] = proj

  } # end per dp

  # Now return the final list of data packages
  dp_list

} # end function
