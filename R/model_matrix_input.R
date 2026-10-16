#' Format Detection Histories and Covariates for Occupancy and Abundance Models
#'
#' @description Combines the output of [detection_matrix()] and [effort_matrix()] with
#' site and observation covariates, in the exact form each modelling package expects. Sites
#' are matched to covariates by name, never by row order, so a reordered covariate table
#' cannot misalign them.
#'
#' Two observation covariates are always added from the effort matrix:
#' \itemize{
#'   \item `effort`, the camera-days in each site and occasion, for modelling detection
#'     probability as a function of effort;
#'   \item when `effort_matrix()` was given `split_by`, one covariate per level,
#'     `<split_by>_<level>`, holding the share of that occasion's camera-days under the
#'     level (for example the share of effort at trail cameras).
#' }
#' Where no camera ran, observation covariates are `NA`, matching the detections.
#'
#' The outputs are:
#' \describe{
#'   \item{`"unmarked"`}{An `unmarkedFrameOccu` (occupancy) or `unmarkedFramePCount`
#'     (abundance) for one species; an `unmarkedFrameOccuComm` for several species with
#'     occupancy data, for `unmarked::occuComm()`; a named list of `unmarkedFramePCount`
#'     for several species with abundance data; and an `unmarkedMultFrame` (one species)
#'     when `seasons` is given, for `unmarked::colext()` or `unmarked::pcountOpen()`.}
#'   \item{`"spOccupancy"`}{A list with `y`, `occ.covs`, `det.covs` and `coords`: `y` is
#'     site x occasion for one species (`PGOcc()`, `spPGOcc()`), species x site x occasion
#'     for several (`msPGOcc()`, `sfMsPGOcc()`), and gains a season dimension before the
#'     occasions when `seasons` is given (`tPGOcc()`, `tMsPGOcc()`).}
#'   \item{`"spAbundance"`}{A list with `y`, `abund.covs`, `det.covs` and `coords`, for
#'     `NMix()` (one species) or `msNMix()` (several).}
#'   \item{`"array"`}{Plain arrays and tables for custom models in JAGS, NIMBLE or Stan:
#'     `y`, `effort`, `effort_by_unit`, `site_covs`, `obs_covs`, `coords`, the
#'     dimensions, and `observed`, which is `TRUE` where a camera ran.}
#' }
#'
#' @param detections The array returned by [detection_matrix()] (species x site x occasion).
#' @param effort The [effort_matrix()] the detections were built from.
#' @param site_covs A data frame of site covariates, or `NULL`. Sites are matched by a
#'   column with the same name as the effort matrix's `site_col` (for example
#'   `"locationID"`) or a `site` column, else by row names.
#' @param obs_covs A named list of observation covariates, each a site x occasion matrix
#'   with the same site and occasion names as `detections`, or `NULL`.
#' @param coords Site coordinates for spatial models, or `NULL`: a data frame with the site
#'   column and two coordinate columns (for example projected `X` and `Y` in metres), or a
#'   two-column matrix with sites as row names.
#' @param output `"unmarked"` (default), `"spOccupancy"`, `"spAbundance"` or `"array"`.
#' @param seasons A vector or `NULL`. For multi-season models, the season of every
#'   occasion (one value per column of `detections`), for example the year each occasion
#'   starts in. Needs an effort matrix built with `align = "calendar"`, so every site's
#'   occasion `k` covers the same dates.
#'
#' @return The model input described for `output`. Stops if a site in `detections` has no
#'   row in `site_covs` or `coords`, if `obs_covs` do not match the detections, or if a
#'   needed modelling package is not installed.
#'
#' @examples
#' # one camera at each of three stations for 10 days
#' deps <- data.frame(
#'   deploymentID = c("A", "B", "C"),
#'   locationID = c("s1", "s2", "s3"),
#'   deploymentStart = as.POSIXct("2024-03-01 00:00", tz = "UTC"),
#'   deploymentEnd = as.POSIXct("2024-03-11 00:00", tz = "UTC"),
#'   baitUse = c(TRUE, FALSE, TRUE)
#' )
#' eff <- effort_matrix(deps, site_col = "locationID", w = 5, split_by = "baitUse")
#'
#' # a feral pig at two stations
#' obs <- data.frame(
#'   deploymentID = c("A", "C"), eventID = c("e1", "e2"), scientificName = "Sus scrofa",
#'   eventStart = as.POSIXct(c("2024-03-02 03:00", "2024-03-08 21:00"), tz = "UTC")
#' )
#' y <- detection_matrix(obs, eff, "Sus scrofa")
#'
#' # site covariates, deliberately in a different order to the detections
#' covs <- data.frame(locationID = c("s3", "s1", "s2"), elevation = c(820, 640, 700))
#'
#' # plain arrays for a custom model
#' inp <- model_matrix_input(y, eff, site_covs = covs, output = "array")
#' inp$site_covs
#' names(inp$obs_covs)
#'
#' # an unmarked occupancy frame
#' if (requireNamespace("unmarked", quietly = TRUE)) {
#'   umf <- model_matrix_input(y, eff, site_covs = covs, output = "unmarked")
#'   umf
#' }
#'
#' @seealso [effort_matrix()] and [detection_matrix()], which build its inputs.
#' @author Zachary Amir
#' @export
model_matrix_input <- function(detections, effort, site_covs = NULL, obs_covs = NULL,
                               coords = NULL,
                               output = c("unmarked", "spOccupancy", "spAbundance", "array"),
                               seasons = NULL) {

  ## settle the output format
  output <- match.arg(output)


  #
  ##
  ### Verify the inputs ----

  ## the detections must be the three-dimensional array from detection_matrix()
  if (!is.array(detections) || length(dim(detections)) != 3) {
    stop("model_matrix_input() needs the species x site x occasion array from detection_matrix().\n",
         "Use detection_matrix(..., by_camera = FALSE).", call. = FALSE)
  } # end detections check
  # and the effort matrix they were built from
  if (!inherits(effort, "wildobs_effort")) {
    stop("model_matrix_input() needs effort from effort_matrix(), built from the same deployments.",
         call. = FALSE)
  } # end effort check
  # with the same sites and occasions
  sites <- dimnames(detections)[[2]]
  occs <- dimnames(detections)[[3]]
  if (!identical(sites, rownames(effort$effort_total)) ||
      !identical(occs, colnames(effort$effort_total))) {
    stop("model_matrix_input() found detections and effort with different sites or occasions.\n",
         "Build the detections with detection_matrix() from this effort matrix.", call. = FALSE)
  } # end matching check
  # what the values mean, recorded by detection_matrix()
  type <- attr(detections, "type")
  # if type is missing, check if there are detections greater than 1, which signals abundance. 
  # otherwise, occupancy. 
  if (is.null(type)) type <- if (any(detections > 1, na.rm = TRUE)) "abundance" else "occupancy"
  # save species from detection history dimensions 
  species <- dimnames(detections)[[1]]


  #
  ##
  ### Site covariates and coordinates, matched by site name ----

  site_df <- .match_sites(site_covs, sites, effort$settings$site_col, "site_covs")
  # missing covariate values break most model fitting functions, so say where they are
  if (!is.null(site_df) && anyNA(site_df)) {
    # warn but dont stop the process. 
    warning(sprintf("model_matrix_input(): site_covs has missing values in %s. Most model functions need complete covariates.",
                    paste(names(site_df)[vapply(site_df, anyNA, logical(1))], collapse = ", ")),
            call. = FALSE)
  } # end missing covariate condition
  # coordinates as a two-column matrix with sites as row names
  coord_mat <- NULL
  if (!is.null(coords)) {
    if (is.matrix(coords)) coords <- as.data.frame(coords)
    coord_df <- .match_sites(coords, sites, effort$settings$site_col, "coords")
    # the two coordinate columns
    if (ncol(coord_df) != 2) {
      stop("model_matrix_input() needs coords to have exactly two coordinate columns beside the site.",
           call. = FALSE)
    } # end two columns check
    coord_mat <- as.matrix(coord_df)
    rownames(coord_mat) <- sites
  } # end coords condition


  #
  ##
  ### Observation covariates ----

  ## camera-days, NA where no camera ran, so they line up with the detections
  eff_total <- effort$effort_total
  eff_obs <- eff_total
  eff_obs[eff_obs == 0] <- NA
  obs_list <- list(effort = eff_obs)
  ## the share of effort under each level of split_by
  split_by <- effort$settings$split_by
  if (!is.null(split_by)) {
    # for each level
    for (lv in dimnames(effort$effort)[[3]]) {
      # its camera-days over the total
      obs_list[[paste(split_by, lv, sep = "_")]] <- effort$effort[, , lv] / eff_obs
    } # end per level
  } # end split condition
  ## the user's own observation covariates, checked for shape and names
  if (!is.null(obs_covs)) {
    # a named list of matrices
    if (!is.list(obs_covs) || is.null(names(obs_covs)) || any(!nzchar(names(obs_covs)))) {
      stop("model_matrix_input() needs obs_covs as a named list of site x occasion matrices.",
           call. = FALSE)
    } # end list check
    for (nm in names(obs_covs)) {
      # grab one
      m <- as.matrix(obs_covs[[nm]])
      # it must have one row per site and one column per occasion
      if (!identical(dim(m), dim(eff_total))) {
        stop(sprintf("model_matrix_input(): obs_covs$%s is %d x %d, but the detections have %d sites and %d occasions.",
                     nm, nrow(m), ncol(m), nrow(eff_total), ncol(eff_total)), call. = FALSE)
      } # end dimension check
      # with row names, put its rows in the detections' site order
      if (!is.null(rownames(m))) {
        if (!setequal(rownames(m), sites)) {
          stop(sprintf("model_matrix_input(): the row names of obs_covs$%s are not the detections' sites.", nm),
               call. = FALSE)
        } # end site names check
        m <- m[sites, , drop = FALSE]
      } # end row names condition
      dimnames(m) <- dimnames(eff_total)
      obs_list[[nm]] <- m
    } # end per covariate
  } # end obs_covs condition


  #
  ##
  ### Seasons ----

  if (!is.null(seasons)) {
    # one season per occasion
    if (length(seasons) != length(occs)) {
      stop(sprintf("model_matrix_input() needs one season per occasion: %d occasions, %d seasons given.",
                   length(occs), length(seasons)), call. = FALSE)
    } # end seasons length check
    # every site's occasions must cover the same dates for seasons to mean the same thing
    if (!identical(effort$settings$align, "calendar")) {
      stop("model_matrix_input() needs an effort matrix built with align = \"calendar\" for multi-season output,\n",
           "so occasion k covers the same dates at every site.", call. = FALSE)
    } # end calendar check
    # spAbundance has no multi-season N-mixture model to format for
    if (output == "spAbundance") {
      stop("model_matrix_input(): spAbundance has no multi-season model; use output = \"spOccupancy\", \"unmarked\" or \"array\".",
           call. = FALSE)
    } # end spAbundance check
  } # end seasons condition


  #
  ##
  ### Build the requested format ----

  if (output == "array") {
    out <- list(y = detections, effort = eff_total, effort_by_unit = effort$effort,
                site_covs = site_df, obs_covs = obs_list, coords = coord_mat,
                n_species = length(species), n_site = length(sites), n_occ = length(occs),
                observed = eff_total > 0, type = type)
    # multi-season models get the occasions grouped by season too
    if (!is.null(seasons)) {
      out$y_seasons <- .by_season(detections, seasons)
      out$seasons <- unique(seasons)
    } # end seasons condition
    return(out)
  } # end array output

  if (output == "spOccupancy" || output == "spAbundance") {
    # one species drops the species dimension, as these packages expect
    y <- if (length(species) == 1) detections[1, , , drop = TRUE] else detections
    if (length(species) == 1) {
      y <- matrix(y, nrow = length(sites), dimnames = list(sites, occs))
    } # end single species condition
    det_covs <- obs_list
    if (!is.null(seasons)) {
      # a season dimension before the occasions: [species, ]site x season x occasion
      y4 <- .by_season(detections, seasons)
      y <- if (length(species) == 1) y4[1, , , , drop = TRUE] else y4
      det_covs <- lapply(obs_list, function(m) {
        # the same regrouping for each observation covariate
        .by_season(array(m, dim = c(1, dim(m)), dimnames = c(list("x"), dimnames(m))), seasons)[1, , , ]
      }) # end per covariate
    } # end seasons condition
    # the packages expect a plain array
    attr(y, "type") <- NULL
    if (output == "spOccupancy") {
      return(list(y = y, occ.covs = site_df, det.covs = det_covs, coords = coord_mat))
    } # end spOccupancy
    return(list(y = y, abund.covs = site_df, det.covs = det_covs, coords = coord_mat))
  } # end sp* output

  ## unmarked, which must be installed
  if (!requireNamespace("unmarked", quietly = TRUE)) {
    stop("model_matrix_input(output = \"unmarked\") needs the unmarked package. Install it with install.packages(\"unmarked\").",
         call. = FALSE)
  } # end unmarked check
  # unmarked takes covariates as a data frame or NULL, never a zero-column frame
  site_arg <- site_df
  # one site x occasion matrix per species
  y_list <- lapply(species, function(s) {
    m <- detections[s, , , drop = TRUE]
    matrix(m, nrow = length(sites), dimnames = list(sites, occs))
  }) # end per species
  names(y_list) <- species

  ## multi-season: one unmarkedMultFrame per species
  if (!is.null(seasons)) {
    frames <- lapply(species, function(s) {
      # occasions grouped by season, then flattened season by season
      y4 <- .by_season(detections[s, , , drop = FALSE], seasons)[1, , , , drop = FALSE]
      n_t <- dim(y4)[3]
      n_k <- dim(y4)[4]
      y_wide <- matrix(aperm(y4[1, , , , drop = FALSE], c(2, 4, 3, 1)), nrow = length(sites))
      # observation covariates regrouped the same way
      oc <- lapply(obs_list, function(m) {
        m4 <- .by_season(array(m, dim = c(1, dim(m)), dimnames = c(list("x"), dimnames(m))), seasons)
        matrix(aperm(m4, c(2, 4, 3, 1)), nrow = length(sites))
      }) # end per covariate
      unmarked::unmarkedMultFrame(y = y_wide, siteCovs = site_arg, obsCovs = oc, numPrimary = n_t)
    }) # end per species
    names(frames) <- species
    return(if (length(species) == 1) frames[[1]] else frames)
  } # end multi-season unmarked

  ## occupancy
  if (type == "occupancy") {
    # one species: a single-season occupancy frame
    if (length(species) == 1) {
      return(unmarked::unmarkedFrameOccu(y = y_list[[1]], siteCovs = site_arg, obsCovs = obs_list))
    } # end single species
    # several: a community occupancy frame for occuComm()
    return(unmarked::unmarkedFrameOccuComm(y = y_list, siteCovs = site_arg, obsCovs = obs_list))
  } # end occupancy

  ## abundance: an N-mixture frame per species
  frames <- lapply(y_list, function(y) {
    unmarked::unmarkedFramePCount(y = y, siteCovs = site_arg, obsCovs = obs_list)
  }) # end per species
  return(if (length(species) == 1) frames[[1]] else frames)
} # end model_matrix_input


#' Put a table's rows in the detections' site order
#'
#' @param x A data frame or `NULL`.
#' @param sites The site names, in order.
#' @param site_col The effort matrix's site column name.
#' @param what The argument name, for messages.
#' @return `x` without its site column, one row per site in `sites` order, or `NULL`.
#' @author Zachary Amir
#' @noRd
.match_sites <- function(x, sites, site_col, what) {
  # nothing to match
  if (is.null(x)) return(NULL)
  # a table is needed
  if (!is.data.frame(x)) {
    stop(sprintf("model_matrix_input() needs %s as a data frame with one row per site.", what),
         call. = FALSE)
  } # end table check
  ## where the site names are: the site column, a 'site' column, or the row names
  if (site_col %in% names(x)) {
    key <- as.character(x[[site_col]])
    x <- x[, setdiff(names(x), site_col), drop = FALSE]
  } else if ("site" %in% names(x)) {
    key <- as.character(x$site)
    x <- x[, setdiff(names(x), "site"), drop = FALSE]
  } else {
    key <- rownames(x)
  } # end site key condition
  # a site with no row cannot be modelled
  missing_sites <- setdiff(sites, key)
  if (length(missing_sites) > 0) {
    stop(sprintf("model_matrix_input(): %d site(s) have no row in %s, e.g. %s.\n",
                 length(missing_sites), what, paste(utils::head(missing_sites, 3), collapse = ", ")),
         sprintf("Give %s a '%s' or 'site' column, or row names, covering every site.", what, site_col),
         call. = FALSE)
  } # end missing sites check
  # a site listed twice is ambiguous
  if (any(duplicated(key[key %in% sites]))) {
    stop(sprintf("model_matrix_input(): some sites appear more than once in %s.", what), call. = FALSE)
  } # end duplicate check
  # reorder to the detections' sites
  out <- x[match(sites, key), , drop = FALSE]
  rownames(out) <- sites
  return(out)
} # end .match_sites


#' Regroup occasions by season
#'
#' @param a An array, species x site x occasion.
#' @param seasons The season of each occasion.
#' @return An array, species x site x season x occasion-within-season, padded with `NA`
#'   where a season has fewer occasions than the longest.
#' @author Zachary Amir
#' @noRd
.by_season <- function(a, seasons) {
  # the seasons in the order they first appear
  season_levels <- unique(seasons)
  # the position of each occasion within its season
  within <- stats::ave(seq_along(seasons), seasons, FUN = seq_along)
  n_k <- max(within)
  # an empty array to fill
  out <- array(NA_real_, dim = c(dim(a)[1], dim(a)[2], length(season_levels), n_k),
               dimnames = list(dimnames(a)[[1]], dimnames(a)[[2]], as.character(season_levels),
                               paste0("k", seq_len(n_k))))
  # for each occasion, put it in its season and position
  for (k in seq_along(seasons)) {
    out[, , match(seasons[k], season_levels), within[k]] <- a[, , k]
  } # end per occasion
  return(out)
} # end .by_season
