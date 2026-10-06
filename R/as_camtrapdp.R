#' Convert a WildObs Data Package to Canonical Camtrap DP
#'
#' Removes every WildObs addition from a data package so it matches a published Camtrap DP
#' version exactly, and can be read by standard consumers such as the \pkg{camtrapdp} R
#' package, GBIF (via `camtrapdp::write_dwc()`) and Agouti. Returns the converted package
#' with a report of every change.
#'
#' @description
#' What counts as "canonical" is read from the official TDWG Camtrap DP profile and table
#' schemas vendored in the package (`inst/profiles/`), not from a hard-coded list. So the
#' function stays complete as WildObs adds extensions, and new Camtrap DP versions only
#' need their files vendored. The WildObs flavour of Camtrap DP (also vendored) is used to
#' label each removal as an expected WildObs extension or as something unexpected.
#'
#' @details
#' The conversion works in this order:
#' \enumerate{
#'   \item \strong{Descriptor}: every key the target profile does not define is removed, at
#'     any depth, e.g. `WildObsMetadata`, `versionControlWildObs`, `project$DPID`,
#'     `contributors[]$ROR`, and the per-deploymentGroup blocks and `timeZone` in
#'     `temporal`. Empty (`NA`) values are dropped too, since Camtrap DP treats a missing
#'     property as absent rather than null. `profile` and `version` are pointed at the
#'     target version. `spatial` is left untouched: it is GeoJSON, which allows extra
#'     members.
#'   \item \strong{Tables}: `deployments`, `media` and `observations` keep only the target
#'     schema's fields, in the target order, and their inline schemas are replaced with the
#'     TDWG table-schema URL. The values in every kept column are unchanged. The
#'     `covariates` table is removed unless `keep_covariates = TRUE` (Camtrap DP allows
#'     extra tables, so keeping it still validates).
#' }
#' Four value changes are made, because the values would otherwise fail validation or
#' break \pkg{camtrapdp} functions:
#' \itemize{
#'   \item `taxonomic[]$vernacularNamesEnglish` becomes `vernacularNames$eng`, the
#'     Camtrap DP form, so common names are kept.
#'   \item A `taxonRank` outside the Camtrap DP list (e.g. `subclass`) is rounded up to the
#'     nearest broader rank in it (e.g. `class`), so a record never claims more precision
#'     than its identification had.
#'   \item A `relatedIdentifierType` of `RAiD`, which DataCite does not list, becomes
#'     `Handle`.
#'   \item Media rows whose `filePath` or `fileMediatype` break the Camtrap DP pattern
#'     (e.g. a path on a contributor's own computer) are removed, and any observation whose
#'     `mediaID` pointed at one has its `mediaID` set to `NA`. This can empty the media
#'     table, in which case a warning says so.
#' }
#'
#' The input must hold all three Camtrap DP tables in memory, as packages from
#' `wildobs_dp_download(..., media = TRUE)` do; a package without one stops with an
#' explanation. The original `dp` is never modified. The result keeps its tables in
#' `resources` only (read them with `frictionless::read_resource()`), without the extra
#' `dp$data` copy, so `frictionless::write_package()` writes a clean Camtrap DP package.
#'
#' @param dp A WildObs data package, as returned (one element of the list) by
#'   [wildobs_dp_download()].
#' @param version Character string. The Camtrap DP version to convert to: `"1.0.2"`
#'   (default) or `"1.0.1"`. Reading 1.0.2 with \pkg{camtrapdp} needs camtrapdp >= 0.5.0.
#' @param keep_covariates Logical. Keep the WildObs `covariates` table? Defaults to
#'   `FALSE`.
#' @param validate Logical. Check the result after converting? Validates the descriptor
#'   against the target profile (needs \pkg{jsonvalidate}) and checks that
#'   `camtrapdp::read_camtrapdp()` and `camtrapdp::check_camtrapdp()` accept it (needs
#'   \pkg{camtrapdp}). Defaults to `TRUE`; skipped with a warning when a package is missing.
#' @param warn Character string. `"important"` (default) warns when information with no
#'   Camtrap DP home is lost: a relationship between tables (`media$observationID`), a
#'   populated `covariates` table, removed media rows, or a failed validation. `"all"` also
#'   summarises every removal in one warning. `"none"` never warns; everything is still in
#'   the report.
#'
#' @return A list with four elements:
#'   \describe{
#'     \item{package}{The converted data package, of the same class as `dp`. Save it with
#'       `frictionless::write_package()`. Its tables are in `resources`, so the
#'       \pkg{camtrapdp} print method reports 0 tables; use `camtrapdp` below with that
#'       package's functions.}
#'     \item{camtrapdp}{The same package read back with `camtrapdp::read_camtrapdp()`, ready
#'       for \pkg{camtrapdp} functions such as `camtrapdp::write_dwc()`. `NULL` if
#'       \pkg{camtrapdp} is not installed or could not read it (see `validation`).}
#'     \item{report}{A data frame with one row per change: `level` (`package`, `resource`,
#'       `field`, `value` or `row`), `path`, `action`, `n` (how many values or rows),
#'       `origin`, and `note`. `origin` is `WildObs extension` when the WildObs flavour of
#'       Camtrap DP declares what was changed, `not in WildObs profile` for anything else
#'       removed (e.g. `projectName`, which [wildobs_dp_download()] adds), `empty value`
#'       for dropped `NA`s, and `conversion` for steps the conversion itself takes
#'       (repointing schemas, removing rows that break a pattern).}
#'     \item{validation}{`NULL` when `validate = FALSE`, otherwise a list with `descriptor`
#'       and `camtrapdp`, each holding `valid` (logical) and `messages` (character).}
#'   }
#'   Stops if `dp` is not a data package with tables in memory, or `version` is not
#'   vendored.
#'
#' @examples
#' \dontrun{
#' api_key <- Sys.getenv("WILDOBSR_API_KEY")
#' dp <- wildobs_dp_download(api_key = api_key,
#'                           project_ids = "ZAmir_QLD_Wet_Tropics_2022_WildObsID_0001")[[1]]
#'
#' # Convert to canonical Camtrap DP 1.0.2
#' out <- as_camtrapdp(dp)
#'
#' # What was changed, and whether the result validates
#' out$report
#' out$validation
#'
#' # Use it with the camtrapdp package, e.g. to export Darwin Core for GBIF
#' camtrapdp::write_dwc(out$camtrapdp, "dwc_export")
#'
#' # Or save it as a Camtrap DP package on disk
#' frictionless::write_package(out$package, "camtrapdp_export")
#' }
#'
#' @author Zachary Amir & Claude Opus 5.5
#'
#' @seealso [wildobs_dp_download()]; the Camtrap DP standard at
#'   <https://camtrap-dp.tdwg.org/>.
#'
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom frictionless write_package
#'
#' @export
as_camtrapdp <- function(dp, version = "1.0.2", keep_covariates = FALSE,
                         validate = TRUE, warn = c("important", "all", "none")) {

  ## settle which warnings the caller wants
  warn <- match.arg(warn)

  #
  ##
  ### Check the input and load the profiles ----

  # a data package is a list with resources
  if (!is.list(dp) || is.null(dp$resources)) {
    stop("'dp' must be a single data package, e.g. one element of the list returned by ",
         "wildobs_dp_download().", call. = FALSE)
  } # end data package check

  # which Camtrap DP versions are vendored, read from the file names
  available <- sub("^camtrap-dp-profile-(.*)\\.json$", "\\1",
                   list.files(.profiles_dir(), pattern = "^camtrap-dp-profile-.*\\.json$"))
  if (!is.character(version) || length(version) != 1 || !version %in% available) {
    stop(sprintf("'%s' is not a Camtrap DP version as_camtrapdp() can convert to.\nAvailable versions: %s",
                 paste(version, collapse = ", "), paste(available, collapse = ", ")),
         call. = FALSE)
  } # end version check

  # the target profile, and the WildObs flavour used to label removals
  target_profile <- .load_profile(file.path(.profiles_dir(), sprintf("camtrap-dp-profile-%s.json", version)))
  wildobs_profile <- .load_profile(file.path(.profiles_dir(), "wildobs",
                                             "camtrap-dp-wildobs-profile-1.0.2-wildobs.1.json"))

  # the three tables Camtrap DP defines
  core_tables <- c("deployments", "media", "observations")
  # where TDWG publishes each version's profile and table schemas
  tdwg_base <- "https://raw.githubusercontent.com/tdwg/camtrap-dp"

  # every change gets one row here, and is bound into the report at the end
  report <- list()
  ## a quick helper to build one report row; callers append it to `report`
  change_row <- function(level, path, action, n, origin, note = NA_character_) {
    data.frame(level = level, path = path, action = action, n = as.integer(n),
               origin = origin, note = note, stringsAsFactors = FALSE)
  } # end row helper

  # work on a copy so the caller's package is never modified
  out <- dp

  #
  ##
  ### Value changes the pruning needs first ----

  ## keep English common names by moving them to Camtrap DP's language-keyed form
  # how many taxa carry a WildObs English name
  n_vern <- 0
  for (i in seq_along(out$taxonomic)) {
    # the WildObs field, if this taxon has one
    eng <- out$taxonomic[[i]][["vernacularNamesEnglish"]]
    if (!is.null(eng) && length(eng) == 1 && !is.na(eng) && nzchar(eng)) {
      ## store it as vernacularNames: {eng: ...}, unless one is already there
      ## ([[ ]] not $, which would partial-match vernacularNamesEnglish itself)
      if (is.null(out$taxonomic[[i]][["vernacularNames"]])) {
        out$taxonomic[[i]][["vernacularNames"]] <- list(eng = eng)
      } # end no existing names condition
      n_vern <- n_vern + 1
    } # end has name condition
    # the WildObs field itself is removed later by the descriptor pruning
  } # end per taxon
  if (n_vern > 0) {
    report[[length(report) + 1]] <- change_row("value", "taxonomic[].vernacularNamesEnglish",
                                               "moved to vernacularNames$eng", n_vern, "WildObs extension",
                                               "Camtrap DP keys common names by ISO 639-3 language code")
  } # end any names condition

  ## round non-standard taxonomic ranks up to the nearest broader standard rank
  allowed_ranks <- .schema_enum(target_profile, c("taxonomic", "taxonRank"))
  # each recode, e.g. "subclass -> class", for the report
  rank_changes <- character(0)
  for (i in seq_along(out$taxonomic)) {
    rank <- out$taxonomic[[i]][["taxonRank"]]
    if (length(allowed_ranks) > 0 && !is.null(rank) && length(rank) == 1 && !is.na(rank) &&
        !rank %in% allowed_ranks) {
      # the broader standard rank this one sits under
      new_rank <- .broader_rank(rank, allowed_ranks)
      rank_changes <- c(rank_changes, paste(rank, "->", new_rank))
      out$taxonomic[[i]][["taxonRank"]] <- new_rank
    } # end non-standard rank condition
  } # end per taxon
  # one report row per kind of change
  for (change in unique(rank_changes)) {
    report[[length(report) + 1]] <- change_row("value", "taxonomic[].taxonRank", paste("recoded", change),
                                               sum(rank_changes == change), "WildObs extension",
                                               "rounded up to the nearest broader rank Camtrap DP lists")
  } # end per change

  ## record RAiD identifiers under the DataCite type that fits them
  allowed_id_types <- .schema_enum(target_profile, c("relatedIdentifiers", "relatedIdentifierType"))
  n_raid <- 0
  for (i in seq_along(out$relatedIdentifiers)) {
    if (identical(out$relatedIdentifiers[[i]][["relatedIdentifierType"]], "RAiD") &&
        length(allowed_id_types) > 0 && !"RAiD" %in% allowed_id_types) {
      out$relatedIdentifiers[[i]][["relatedIdentifierType"]] <- "Handle"
      n_raid <- n_raid + 1
    } # end RAiD condition
  } # end per identifier
  if (n_raid > 0) {
    report[[length(report) + 1]] <- change_row("value", "relatedIdentifiers[].relatedIdentifierType",
                                               "recoded RAiD -> Handle", n_raid, "WildObs extension",
                                               "DataCite has no RAiD type; a RAiD resolves as a Handle")
  } # end any RAiD condition

  #
  ##
  ### Prune the descriptor against the target profile ----

  ## resources and the in-memory tables are handled below, and spatial is GeoJSON,
  ## which allows extra members, so set those aside while the rest is pruned
  set_aside <- intersect(c("resources", "data", "spatial"), names(out))
  held <- out[set_aside]
  descriptor <- out[setdiff(names(out), set_aside)]

  # what the target removes, and what even the WildObs flavour would remove
  pruned <- .prune_to_schema(descriptor, list(target_profile), target_profile)
  wildobs_pruned <- .prune_to_schema(descriptor, list(wildobs_profile), wildobs_profile)

  ## report each removed path once, with how many times it occurred
  dropped_paths <- gsub("\\[[0-9]+\\]", "[]", pruned$dropped)
  wildobs_unknown <- gsub("\\[[0-9]+\\]", "[]", wildobs_pruned$dropped)
  emptied_paths <- gsub("\\[[0-9]+\\]", "[]", pruned$emptied)
  for (p in unique(dropped_paths)) {
    # an empty value is dropped whichever profile it is in, so label it as empty
    empty <- p %in% emptied_paths
    report[[length(report) + 1]] <- change_row(
      level = "package", path = p,
      action = if (empty) "dropped empty value" else "dropped",
      n = sum(dropped_paths == p),
      origin = if (empty) "empty value"
               else if (p %in% wildobs_unknown) "not in WildObs profile"
               else "WildObs extension")
  } # end per dropped path

  # put the pruned descriptor back together with what was set aside,
  # restoring the package's attributes (its class, and the directory frictionless needs)
  out <- c(pruned$x, held)
  attributes(out) <- c(list(names = names(out)), attributes(dp)[setdiff(names(attributes(dp)), "names")])

  ## point the package at the target version
  out$profile <- sprintf("%s/%s/camtrap-dp-profile.json", tdwg_base, version)
  out$version <- version
  report[[length(report) + 1]] <- change_row(level = "package", path = "profile",
                                             action = "repointed", n = 1, origin = "conversion",
                                             note = sprintf("now Camtrap DP %s", version))

  #
  ##
  ### Prune the tables ----

  ## Camtrap DP requires all three core tables, so a package without them cannot be converted
  resource_names <- vapply(out$resources, function(r) r$name, character(1))
  missing_tables <- setdiff(core_tables, resource_names)
  if (length(missing_tables) > 0) {
    stop(sprintf("%s has no %s table, and Camtrap DP requires deployments, media and observations.\n",
                 dp$id, paste(missing_tables, collapse = " or ")),
         "Download it with wildobs_dp_download(..., media = TRUE). A project shared as 'partial' ",
         "has no tables and cannot be converted.", call. = FALSE)
  } # end missing tables condition

  ## every table must be in memory to be pruned
  for (r in seq_along(out$resources)) {
    if (resource_names[r] %in% core_tables && !is.data.frame(out$resources[[r]]$data)) {
      stop(sprintf("The %s table of %s is not held in memory.\n", resource_names[r], dp$id),
           "as_camtrapdp() needs a package from wildobs_dp_download(), which keeps tables in memory.",
           call. = FALSE)
    } # end in-memory check
  } # end per resource

  # columns whose loss breaks a link between tables, found for the warning below
  lost_links <- character(0)

  # for each table Camtrap DP defines
  for (r in which(resource_names %in% core_tables)) {
    name <- resource_names[r]
    # the target schema's fields, and the WildObs flavour's, for labelling
    target_schema <- .load_json(file.path(.profiles_dir(),
                                          sprintf("camtrap-dp-%s-table-schema-%s.json", name, version)))
    wildobs_schema <- .load_json(file.path(.profiles_dir(), "wildobs",
                                           sprintf("%s-table-schema-1.0.2-wildobs.1.json", name)))
    target_fields <- vapply(target_schema$fields, function(f) f$name, character(1))
    wildobs_fields <- vapply(wildobs_schema$fields, function(f) f$name, character(1))
    # columns the WildObs flavour uses as foreign keys, i.e. links to other tables
    wildobs_fk <- unlist(lapply(wildobs_schema$foreignKeys, function(k) k$fields))

    df <- out$resources[[r]]$data

    ## drop every column the target does not define, noting how much data each held
    for (col in setdiff(names(df), target_fields)) {
      extension <- col %in% wildobs_fields
      report[[length(report) + 1]] <- change_row(
        level = "field", path = sprintf("%s.%s", name, col), action = "dropped",
        n = sum(!is.na(df[[col]])),
        origin = if (extension) "WildObs extension" else "not in WildObs profile",
        note = if (col %in% wildobs_fk) "links this table to another; Camtrap DP has no field for it"
               else NA_character_)
      # a populated link between tables is information with no Camtrap DP home
      if (col %in% wildobs_fk && any(!is.na(df[[col]]))) {
        lost_links <- c(lost_links, sprintf("%s.%s", name, col))
      } # end lost link condition
    } # end per dropped column

    ## a target field the data lacks is reported, never invented
    for (col in setdiff(target_fields, names(df))) {
      report[[length(report) + 1]] <- change_row(
        level = "field", path = sprintf("%s.%s", name, col), action = "missing from data", n = 0,
        origin = "conversion", note = "Camtrap DP defines this field but the package does not have it")
    } # end per missing column

    # keep the target's fields, in the target's order
    df <- df[, intersect(target_fields, names(df)), drop = FALSE]

    # and cite the published table schema instead of the inline one
    out$resources[[r]]$data <- df
    out$resources[[r]]$schema <- sprintf("%s/%s/%s-table-schema.json", tdwg_base, version, name)
    report[[length(report) + 1]] <- change_row(
      level = "resource", path = name, action = "schema repointed", n = 1, origin = "conversion",
      note = "inline schema replaced with the TDWG table-schema URL")
  } # end per core table

  #
  ##
  ### Remove media rows that break a Camtrap DP pattern ----

  # media rows removed, and how many observations lost their mediaID as a result
  n_media_removed <- 0
  n_media_before <- 0
  media_idx <- which(resource_names == "media")
  if (length(media_idx) == 1) {
    media <- out$resources[[media_idx]]$data
    n_media_before <- nrow(media)
    target_schema <- .load_json(file.path(.profiles_dir(),
                                          sprintf("camtrap-dp-media-table-schema-%s.json", version)))
    # flag rows breaking any pattern constraint the target declares
    bad <- rep(FALSE, nrow(media))
    for (field in target_schema$fields) {
      pattern <- field$constraints$pattern
      if (is.null(pattern) || !field$name %in% names(media)) next
      # only real values can break a pattern; empty cells are judged by other rules
      breaks <- !is.na(media[[field$name]]) & !grepl(pattern, media[[field$name]], perl = TRUE)
      if (any(breaks)) {
        report[[length(report) + 1]] <- change_row(
          level = "row", path = sprintf("media.%s", field$name), action = "rows removed", n = sum(breaks),
          origin = "conversion", note = sprintf("values do not match the Camtrap DP pattern %s", pattern))
      } # end any breaks condition
      bad <- bad | breaks
    } # end per field

    if (any(bad)) {
      # the mediaIDs going away
      removed_ids <- media$mediaID[bad]
      n_media_removed <- sum(bad)
      out$resources[[media_idx]]$data <- media[!bad, , drop = FALSE]

      ## keep the observations-to-media link valid: blank any mediaID that now points nowhere
      obs_idx <- which(resource_names == "observations")
      if (length(obs_idx) == 1) {
        obs <- out$resources[[obs_idx]]$data
        orphaned <- !is.na(obs$mediaID) & obs$mediaID %in% removed_ids
        if (any(orphaned)) {
          obs$mediaID[orphaned] <- NA
          out$resources[[obs_idx]]$data <- obs
          report[[length(report) + 1]] <- change_row(
            level = "value", path = "observations.mediaID", action = "set to NA", n = sum(orphaned),
            origin = "conversion", note = "pointed at a media row removed for breaking a Camtrap DP pattern")
        } # end orphaned condition
      } # end observations condition
    } # end any bad rows condition
  } # end media condition

  #
  ##
  ### The covariates table ----

  cov_idx <- which(resource_names == "covariates")
  covariates_dropped <- FALSE
  if (length(cov_idx) == 1 && !isTRUE(keep_covariates)) {
    # how many rows of data go with it
    n_cov <- if (is.data.frame(out$resources[[cov_idx]]$data)) nrow(out$resources[[cov_idx]]$data) else 0
    out$resources <- out$resources[-cov_idx]
    covariates_dropped <- n_cov > 0
    report[[length(report) + 1]] <- change_row(
      level = "resource", path = "covariates", action = "dropped", n = n_cov, origin = "WildObs extension",
      note = "set keep_covariates = TRUE to keep it; Camtrap DP allows extra tables")
  } # end covariates condition

  ## drop WildObsR's extra in-memory copy of the tables (dp$data): the tables stay in
  ## the resources, where frictionless keeps them, and frictionless::write_package()
  ## would otherwise write every table a second time inside datapackage.json
  out$data <- NULL

  # bind the report, with an empty but well-formed table when nothing changed
  report <- if (length(report) > 0) do.call(rbind, report) else
    data.frame(level = character(0), path = character(0), action = character(0), n = integer(0),
               origin = character(0), note = character(0), stringsAsFactors = FALSE)

  #
  ##
  ### Validate the result ----

  ## read the result back with the camtrapdp package once, so it can be handed to
  ## camtrapdp functions such as write_dwc() directly, and checked from the same object
  ctdp <- .read_with_camtrapdp(out)

  validation <- NULL
  if (isTRUE(validate)) {
    validation <- list(descriptor = .validate_descriptor(out, target_profile),
                       camtrapdp = .validate_with_camtrapdp(ctdp))
  } # end validate condition

  #
  ##
  ### Warn about what matters ----

  if (warn != "none") {
    # a link between tables with no Camtrap DP field to carry it
    if (length(lost_links) > 0) {
      warning(sprintf("%s: dropped %s, which links tables together; %s",
                      dp$id, paste(lost_links, collapse = ", "),
                      "Camtrap DP has no field for it, so the link is lost."), call. = FALSE)
    } # end lost links condition
    # a populated covariates table
    if (covariates_dropped) {
      warning(sprintf("%s: dropped the covariates table. Use keep_covariates = TRUE to keep it.", dp$id),
              call. = FALSE)
    } # end covariates warning
    # media rows removed, and whether any are left
    if (n_media_removed > 0) {
      warning(sprintf("%s: removed %d of %d media rows whose %s breaks the Camtrap DP pattern.%s",
                      dp$id, n_media_removed, n_media_before, "filePath or fileMediatype",
                      if (n_media_removed == n_media_before) " The media table is now empty." else ""),
              call. = FALSE)
    } # end media rows warning
    # a result that still fails validation
    for (check in names(validation)) {
      if (isFALSE(validation[[check]]$valid)) {
        warning(sprintf("%s: the converted package fails %s validation: %s", dp$id, check,
                        paste(utils::head(validation[[check]]$messages, 3), collapse = "; ")), call. = FALSE)
      } # end failed check condition
    } # end per check
    # and, if asked, everything that was removed
    if (warn == "all" && nrow(report) > 0) {
      warning(sprintf("%s: %d changes made: %s", dp$id, nrow(report),
                      paste(report$path, report$action, collapse = "; ")), call. = FALSE)
    } # end all condition
  } # end warn condition

  return(list(package = out, camtrapdp = ctdp$object, report = report, validation = validation))
} # end function


#
##
### Helpers used only by as_camtrapdp() ----

## where the vendored profiles live inside the installed package
.profiles_dir <- function() {
  return(system.file("profiles", package = "WildObsR"))
} # end profiles dir

## read a JSON schema as nested lists, keeping arrays as lists
.load_json <- function(path) {
  return(jsonlite::fromJSON(path, simplifyVector = FALSE))
} # end json reader

## Load a profile and splice in the vendored schemas it references by URL
## (the frictionless base and GeoJSON), so it is self-contained and needs no network.
## Ported from camDB's .load_camtrap_profile() and .resolve_remote_refs().
.load_profile <- function(path) {
  # the schemas the profiles pull in by URL, keyed by the URL they use
  vendored <- list(
    "https://specs.frictionlessdata.io/schemas/data-package.json" =
      .load_json(file.path(.profiles_dir(), "frictionless-data-package.json")),
    "http://json.schemastore.org/geojson.json" =
      .load_json(file.path(.profiles_dir(), "geojson.json"))
  )
  ## walk the schema, replacing any node whose $ref is a vendored URL
  resolve <- function(node) {
    # only lists can hold a reference
    if (!is.list(node)) return(node)
    ref <- node[["$ref"]]
    if (is.character(ref) && length(ref) == 1 && ref %in% names(vendored)) {
      return(vendored[[ref]])
    } # end vendored reference condition
    # otherwise look deeper
    return(lapply(node, resolve))
  } # end resolver
  return(resolve(.load_json(path)))
} # end profile loader

## Gather what a set of schema nodes says an object or array may contain, following
## internal $refs and every allOf/anyOf/oneOf branch, and merging them. Merging matters:
## the Camtrap DP profile lists only `role` for contributors, while the frictionless base
## lists title, email, path and organization, so reading one branch alone would wrongly
## strip the others.
.schema_parts <- function(schemas, root) {
  props <- list()
  patterns <- list()
  items <- list()
  # work through every node, adding the branches it points to
  queue <- schemas
  while (length(queue) > 0) {
    node <- queue[[1]]
    queue <- queue[-1]
    if (!is.list(node)) next
    # an internal reference like "#/$defs/version" points elsewhere in the same schema
    ref <- node[["$ref"]]
    if (is.character(ref) && startsWith(ref, "#/")) {
      target <- root
      for (key in strsplit(sub("^#/", "", ref), "/")[[1]]) target <- target[[key]]
      queue <- c(queue, list(target))
    } # end internal reference condition
    # every combinator branch applies too
    for (combinator in c("allOf", "anyOf", "oneOf")) {
      if (is.list(node[[combinator]])) queue <- c(queue, node[[combinator]])
    } # end per combinator
    # collect the properties this node defines, keeping every branch's sub-schema
    for (key in names(node$properties)) props[[key]] <- c(props[[key]], list(node$properties[[key]]))
    # and keys allowed by pattern, e.g. one block per deploymentGroup, or a language code
    for (regex in names(node$patternProperties)) {
      patterns[[regex]] <- c(patterns[[regex]], list(node$patternProperties[[regex]]))
    } # end per pattern
    # and the item schema, for arrays
    if (is.list(node$items)) items <- c(items, list(node$items))
  } # end queue
  return(list(props = props, patterns = patterns, items = items))
} # end schema parts

## Remove everything a schema does not define, recursing through objects and arrays.
## Returns the pruned node, every removed path, and which of those were empty values.
.prune_to_schema <- function(x, schemas, root, path = "") {
  dropped <- character(0)
  emptied <- character(0)
  parts <- .schema_parts(schemas, root)

  ## an object whose allowed keys the schema lists, by name or by pattern
  if (is.list(x) && !is.null(names(x)) && (length(parts$props) > 0 || length(parts$patterns) > 0)) {
    for (key in names(x)) {
      key_path <- if (nzchar(path)) paste0(path, ".", key) else key
      value <- x[[key]]
      # the sub-schemas for this key: its own definition, plus any pattern it matches
      matched <- names(parts$patterns)[vapply(names(parts$patterns), function(rx) grepl(rx, key, perl = TRUE),
                                              logical(1))]
      key_schemas <- c(parts$props[[key]], unlist(parts$patterns[matched], recursive = FALSE))
      if (length(key_schemas) == 0) {
        # a key the schema does not define
        dropped <- c(dropped, key_path)
        x[[key]] <- NULL
      } else if (is.null(value) || (is.atomic(value) && length(value) == 1 && is.na(value))) {
        # an empty value: Camtrap DP treats a missing property as absent, not null
        dropped <- c(dropped, key_path)
        emptied <- c(emptied, key_path)
        x[[key]] <- NULL
      } else {
        # a defined key, so prune inside it
        inner <- .prune_to_schema(value, key_schemas, root, key_path)
        ## an array of one value would be written as a bare value (jsonlite's auto_unbox),
        ## which breaks the schema, so mark array-typed values to stay arrays
        is_array <- any(vapply(key_schemas, function(ks) "array" %in% unlist(ks$type), logical(1)))
        if (is_array && is.atomic(inner$x)) inner$x <- I(inner$x)
        x[[key]] <- inner$x
        dropped <- c(dropped, inner$dropped)
        emptied <- c(emptied, inner$emptied)
      } # end key condition
    } # end per key

  ## an array whose items the schema describes
  } else if (is.list(x) && is.null(names(x)) && length(parts$items) > 0) {
    for (i in seq_along(x)) {
      inner <- .prune_to_schema(x[[i]], parts$items, root, sprintf("%s[%d]", path, i))
      x[[i]] <- inner$x
      dropped <- c(dropped, inner$dropped)
      emptied <- c(emptied, inner$emptied)
    } # end per item
  } # end structure condition

  return(list(x = x, dropped = dropped, emptied = emptied))
} # end prune

## the enum a profile allows at a path such as c("taxonomic", "taxonRank")
.schema_enum <- function(profile, keys) {
  # start at the profile, then step into each key through arrays and objects
  schemas <- list(profile)
  for (key in keys) {
    parts <- .schema_parts(schemas, profile)
    # step through an array's items first, if this is an array
    if (length(parts$items) > 0 && !key %in% names(parts$props)) {
      parts <- .schema_parts(parts$items, profile)
    } # end array condition
    schemas <- parts$props[[key]]
  } # end per key
  # gather every enum along the way's final branches
  values <- unlist(lapply(schemas, function(s) s$enum))
  return(unique(as.character(values)))
} # end enum lookup

## The nearest broader rank Camtrap DP allows, for a rank it does not. The full order
## runs from broadest to narrowest; a non-standard rank takes the first allowed rank above it.
.broader_rank <- function(rank, allowed) {
  full_order <- c("kingdom", "phylum", "superclass", "class", "subclass", "superorder", "order",
                  "suborder", "superfamily", "family", "subfamily", "tribe", "genus",
                  "species", "subspecies")
  position <- match(tolower(rank), full_order)
  # a rank not in the list at all is left for validation to report
  if (is.na(position)) return(rank)
  above <- full_order[seq_len(position)]
  return(utils::tail(above[above %in% allowed], 1))
} # end broader rank

## Validate the descriptor against the target profile with jsonvalidate.
## Tables are replaced by their CSV paths, as they would be on disk.
.validate_descriptor <- function(pkg, profile) {
  if (!requireNamespace("jsonvalidate", quietly = TRUE)) {
    warning("Install the jsonvalidate package to validate the descriptor.", call. = FALSE)
    return(list(valid = NA, messages = "jsonvalidate not installed"))
  } # end package check
  # the descriptor as written to datapackage.json
  desc <- unclass(pkg)
  desc$data <- NULL
  desc$resources <- lapply(desc$resources, function(r) {
    # an in-memory table is written as a CSV, so describe it the way the file will be
    if (is.data.frame(r$data)) {
      r$data <- NULL
      r$path <- paste0(r$name, ".csv")
      r$format <- "csv"
      r$mediatype <- "text/csv"
      r$encoding <- "utf-8"
      r$dialect <- NULL
    } # end in-memory condition
    r
  }) # end per resource
  result <- jsonvalidate::json_validate(
    json = jsonlite::toJSON(desc, auto_unbox = TRUE, null = "null", digits = NA),
    schema = jsonlite::toJSON(profile, auto_unbox = TRUE, null = "null", digits = NA),
    engine = "imjv", verbose = TRUE, greedy = TRUE)
  errors <- attr(result, "errors")
  messages <- if (is.null(errors) || nrow(errors) == 0) character(0) else paste(errors$field, errors$message)
  return(list(valid = isTRUE(as.logical(result)), messages = messages))
} # end descriptor validation

## Read the package back with the camtrapdp package, its main consumer, giving an object
## its functions (write_dwc(), filter_observations(), ...) accept. Returns list(object, error).
.read_with_camtrapdp <- function(pkg) {
  # without camtrapdp there is nothing to read it into
  if (!requireNamespace("camtrapdp", quietly = TRUE)) {
    message("Install the camtrapdp package (>= 0.5.0) to get a camtrapdp object back as well.")
    return(list(object = NULL, error = "camtrapdp not installed"))
  } # end package check
  ### write to a temporary folder, as a user would before reading it with camtrapdp
  ## the object remembers this folder, so it stays until R removes its temp folder at exit
  dir <- tempfile("as_camtrapdp_")
  result <- tryCatch({
    frictionless::write_package(pkg, dir)
    x <- suppressMessages(camtrapdp::read_camtrapdp(file.path(dir, "datapackage.json")))
    list(object = x, error = NULL)
  }, error = function(e) list(object = NULL, error = conditionMessage(e)))
  # a failed read leaves nothing worth keeping
  if (is.null(result$object)) unlink(dir, recursive = TRUE)
  return(result)
} # end camtrapdp read

## Check the read-back object passes the camtrapdp package's own checks.
.validate_with_camtrapdp <- function(ctdp) {
  # camtrapdp missing entirely: the check could not run
  if (identical(ctdp$error, "camtrapdp not installed")) {
    return(list(valid = NA, messages = "camtrapdp not installed"))
  } # end not installed condition
  # it could not even be read, so that is the failure to report
  if (is.null(ctdp$object)) return(list(valid = FALSE, messages = ctdp$error))
  # otherwise run camtrapdp's own checks on it
  result <- tryCatch({
    camtrapdp::check_camtrapdp(ctdp$object)
    list(valid = TRUE, messages = character(0))
  }, error = function(e) list(valid = FALSE, messages = conditionMessage(e)))
  return(result)
} # end camtrapdp validation
