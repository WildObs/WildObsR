#' Cite the Camera Trap Data Packages Used in an Analysis
#'
#' @description Builds a reference for each data package in a list, ready to paste
#'   into a manuscript, import into a reference manager, or keep as a table. Works on
#'   any Camtrap DP package, from WildObs or elsewhere:
#'   \itemize{
#'     \item When a package carries a `bibliographicCitation`, that text is the
#'       reference, word for word, because it is the one the data providers asked for.
#'       WildObs citations end with the project's RAiD.
#'     \item Otherwise the reference is built from the Camtrap DP descriptor: authors
#'       from `contributors` (people with the role `contact`, `principalInvestigator`
#'       or `contributor`, or no role; never `rightsHolder`, `publisher` or `funder`),
#'       the year `created`, `title`, and `homepage`, else `id`.
#'   }
#'   With `include_version = TRUE` the version of the data is appended: for WildObs
#'   packages the WildObs database release they were downloaded from
#'   (`versionControlWildObs`), otherwise the package `version`, unless it is only the
#'   Camtrap DP standard version named in `profile` or the citation already gives it.
#'
#'   With `covariates = TRUE`, the products behind the WildObs covariates (e.g. the
#'   Forest Landscape Integrity Index or the human footprint) are cited too, once
#'   each. They are read from the `custom$source$citation` of each field in the
#'   covariates table schema, so only packages downloaded with their tables have them.
#'
#' @param dp_list A list of data packages, such as the result of
#'   [wildobs_dp_download()], or a single data package. Packages downloaded with
#'   `metadata_only = TRUE` can be cited too.
#' @param format The form of the result: `"text"` (default) for one formatted
#'   reference per package, `"bibtex"` for a [utils::bibentry()] object that
#'   [utils::toBibtex()] writes as BibTeX, or `"data.frame"` for one row per reference.
#' @param sort_by How to order the references: `"author"` (default, alphabetically by
#'   the first author's family name, as most reference lists are), `"year"` (oldest first, then by
#'   author), or `"none"` (the order of `dp_list`).
#' @param include_version Logical. Append the version of the data to each reference?
#'   Defaults to `TRUE`, because the same project can grow between database releases.
#' @param accessed Optional date the data were downloaded, as a `Date` or a
#'   `"YYYY-MM-DD"` string, appended as "Accessed YYYY-MM-DD.". Defaults to `NULL`
#'   (left out).
#' @param covariates Logical. Also cite the products behind the covariates table?
#'   Defaults to `FALSE`.
#'
#' @return Depends on `format`:
#'   \describe{
#'     \item{`"text"`}{A character vector of references, named by package `id`. The
#'       covariate product citations, if asked for, follow, named `covariate_1`,
#'       `covariate_2`, ...}
#'     \item{`"bibtex"`}{A [utils::bibentry()] object: one `@misc` entry per package,
#'       keyed by package `id`, plus one per covariate product.}
#'     \item{`"data.frame"`}{One row per reference, with columns `type` (`"data
#'       package"` or `"covariate product"`), `id`, `authors`, `year`, `title`,
#'       `version`, `identifier` (RAiD or DOI where there is one), `url` (the
#'       identifier, else the package `homepage` or `id`), `licenses` and `citation`.}
#'   }
#'   Stops if `dp_list` holds something that is not a data package, or if a package
#'   has neither a `bibliographicCitation` nor a `title` to build one from.
#'
#' @examples
#' # any Camtrap DP package can be cited, here the camtrapdp package's example
#' if (requireNamespace("camtrapdp", quietly = TRUE)) {
#'   x <- camtrapdp::example_dataset()
#'   cite_datapackages(x)
#'   # as a table, with the date the data were accessed
#'   cite_datapackages(x, format = "data.frame", accessed = "2026-10-15")
#'   # or as BibTeX for a reference manager
#'   utils::toBibtex(cite_datapackages(x, format = "bibtex"))
#' }
#'
#' \dontrun{
#' # cite every WildObs project downloaded for an analysis, and the covariate products
#' api_key <- Sys.getenv("WILDOBSR_API_KEY")
#' wet_tropics <- list(xmin = 144.9, xmax = 146.5, ymin = -19.6, ymax = -15.3)
#' ids <- wildobs_mongo_query(api_key = api_key, spatial = wet_tropics,
#'                            tabularSharingPreference = "open")
#' dp_list <- wildobs_dp_download(api_key = api_key, project_ids = ids)
#' writeLines(cite_datapackages(dp_list, covariates = TRUE, accessed = Sys.Date()))
#' }
#'
#' @seealso [wildobs_dp_download()] to download the packages; [extract_metadata()]
#'   for their contributors, licences and related identifiers as tables.
#' @author Zachary Amir
#' @importFrom utils bibentry person as.person
#' @export
cite_datapackages <- function(dp_list,
                              format = c("text", "bibtex", "data.frame"),
                              sort_by = c("author", "year", "none"),
                              include_version = TRUE,
                              accessed = NULL,
                              covariates = FALSE) {

  ## settle the choices the caller made
  format <- match.arg(format)
  sort_by <- match.arg(sort_by)

  #
  ##
  ### Check the input ----

  ## a single package is a list with descriptor keys, so wrap it as a list of one
  if (.is_datapackage(dp_list)) dp_list <- list(dp_list)
  # anything else has to be a list whose every element is a package
  if (!is.list(dp_list) || length(dp_list) == 0) {
    stop("cite_datapackages() needs a data package or a list of them, and got an empty or non-list object.\n",
         "Pass the result of wildobs_dp_download(), or a package read with frictionless or camtrapdp.",
         call. = FALSE)
  } # end empty input check
  # find the elements that are not packages
  not_dp <- which(!vapply(dp_list, .is_datapackage, logical(1)))
  if (length(not_dp) > 0) {
    stop(sprintf("cite_datapackages() found %d element(s) of dp_list that are not data packages (positions %s).\n",
                 length(not_dp), paste(not_dp, collapse = ", ")),
         "Each element must be a data package with at least an id or a title.", call. = FALSE)
  } # end non-package check

  ## the access date, checked once and formatted once
  accessed_text <- NULL
  if (!is.null(accessed)) {
    # a Date or ISO string becomes a Date, anything else becomes NA
    accessed_date <- tryCatch(as.Date(accessed), error = function(e) as.Date(NA))
    # an unreadable date is a mistake worth stopping for
    if (length(accessed_date) != 1 || is.na(accessed_date)) {
      stop("cite_datapackages() could not read accessed = ", format(accessed), " as a date.\n",
           "Give one date, as a Date or a \"YYYY-MM-DD\" string.", call. = FALSE)
    } # end bad date check
    accessed_text <- sprintf("Accessed %s.", format(accessed_date, "%Y-%m-%d"))
  } # end accessed condition


  #
  ##
  ### Build one reference per package ----

  # init an empty list to hold one row per package
  rows <- list()
  # for each package
  for (i in seq_along(dp_list)) {
    # grab it
    dp <- dp_list[[i]]
    # and the name to call it by in messages and keys
    dp_id <- .first_text(dp$id, dp$name, names(dp_list)[i], paste0("package_", i))

    ## the parts every reference needs
    title <- .first_text(dp$title)
    citation <- .first_text(dp$bibliographicCitation)
    # without a citation or a title there is nothing to cite
    if (is.na(citation) && is.na(title)) {
      stop(dp_id, " has neither a bibliographicCitation nor a title in cite_datapackages().\n",
           "Add one of them to the package descriptor before citing it.", call. = FALSE)
    } # end nothing to cite check

    ## the authors, from the citation's own author list when there is one,
    ## since a citation can name people the contributors list gives other roles
    authors <- if (!is.na(citation)) .citation_authors(citation) else character(0)
    # otherwise from the contributors who are people credited with the work
    if (length(authors) == 0) authors <- .contributor_authors(dp$contributors)

    ## the year: the one in the citation, else the year the package was created
    year <- if (!is.na(citation)) .citation_year(citation) else NA_character_
    if (is.na(year)) year <- substr(.first_text(dp$created), 1, 4)

    ## the version of the data, if wanted and worth stating
    version <- if (isTRUE(include_version)) .data_version(dp) else NA_character_

    ## a persistent identifier: the RAiD, else a DOI the package names as itself
    identifier <- .package_identifier(dp, citation)
    # the link to give: the identifier, else the homepage, else the id
    link <- .first_text(identifier, dp$homepage, dp$id)

    ## the text of the reference
    if (!is.na(citation)) {
      # the provider's own citation, word for word, ending in a full stop
      text <- sub("([^.])$", "\\1.", trimws(citation))
    } else {
      # or one built from the descriptor: Authors (Year). Title. Link.
      text <- sprintf("%s (%s). %s. %s.",
                      if (length(authors) > 0) paste(authors, collapse = ", ") else "Anonymous",
                      if (is.na(year)) "n.d." else year, sub("\\.$", "", title), link)
    } # end citation condition
    # add the version, unless the citation already gives it
    if (!is.na(version) && !grepl(version, text, fixed = TRUE)) {
      # WildObs data are versioned by the database release they came from
      label <- if (!is.null(dp$versionControlWildObs)) "WildObs database version" else "Version"
      text <- paste(text, sprintf("%s %s.", label, version))
    } # end version condition
    # and the access date
    if (!is.null(accessed_text)) text <- paste(text, accessed_text)

    ## the licences, by name where they have one, else by link
    licenses <- unique(vapply(dp$licenses, function(l) .first_text(l$name, l$path), character(1)))

    ## store the row
    rows[[i]] <- data.frame(
      type = "data package", id = dp_id,
      authors = paste(authors, collapse = "; "),
      year = year, title = title, version = version, identifier = identifier, url = link,
      licenses = paste(licenses[!is.na(licenses)], collapse = "; "),
      citation = text, stringsAsFactors = FALSE)
  } # end per package
  # stack the rows
  refs <- do.call(rbind, rows)


  #
  ##
  ### Order the references ----

  ## sort on the first author's family name, ignoring case, as reference lists do;
  ## a reference with no author list sorts on its own text
  first_author <- vapply(seq_len(nrow(refs)), function(k) {
    # the first author, as written
    a <- strsplit(refs$authors[k], "; ", fixed = TRUE)[[1]][1]
    # no author means the citation text itself
    if (is.na(a) || !nzchar(a)) return(tolower(refs$citation[k]))
    # otherwise the family name, then the whole citation to break ties
    tolower(paste(.as_bib_person(a)$family, refs$citation[k]))
  }, character(1))
  # alphabetical by first author, or by year then author
  if (sort_by == "author") refs <- refs[order(first_author), , drop = FALSE]
  if (sort_by == "year") refs <- refs[order(refs$year, first_author), , drop = FALSE]
  # tidy row names after sorting
  rownames(refs) <- NULL


  #
  ##
  ### Add the covariate products ----

  if (isTRUE(covariates)) {
    # every covariate source cited across the packages, once each
    cov_refs <- do.call(rbind, lapply(dp_list, .covariate_sources))
    # a product cited by several fields can carry slightly different links, so keep its first
    if (!is.null(cov_refs)) cov_refs <- cov_refs[!duplicated(cov_refs$citation), , drop = FALSE]
    # a package without its covariates table has none to give
    if (is.null(cov_refs) || nrow(cov_refs) == 0) {
      message("cite_datapackages(): no covariate sources found. Covariate citations come from ",
              "the covariates table schema, so download the packages with their tables ",
              "(metadata_only = FALSE).")
    } else {
      # in alphabetical order
      cov_refs <- cov_refs[order(tolower(cov_refs$citation)), , drop = FALSE]
      # one row each, in the same columns as the packages
      cov_rows <- data.frame(
        type = "covariate product", id = paste0("covariate_", seq_len(nrow(cov_refs))),
        authors = NA_character_, year = vapply(cov_refs$citation, .citation_year, character(1)),
        title = NA_character_, version = NA_character_, identifier = cov_refs$doi,
        url = cov_refs$doi, licenses = NA_character_, citation = cov_refs$citation, stringsAsFactors = FALSE)
      # follow the data packages
      refs <- rbind(refs, cov_rows)
      rownames(refs) <- NULL
    } # end found sources condition
  } # end covariates condition


  #
  ##
  ### Return the chosen format ----

  ## a table, as it stands
  if (format == "data.frame") return(refs)

  ## text, one reference per element, named by package id
  if (format == "text") return(stats::setNames(refs$citation, refs$id))

  ## BibTeX entries, one @misc per reference
  entries <- lapply(seq_len(nrow(refs)), function(k) {
    ## the people, built one at a time so each name is split correctly
    people <- if (is.na(refs$authors[k]) || !nzchar(refs$authors[k])) NULL else
      do.call(c, lapply(strsplit(refs$authors[k], "; ", fixed = TRUE)[[1]], .as_bib_person))
    ## a covariate product keeps its whole citation as the title, and its author list
    ## as one protected name, since product citations follow no single style
    if (refs$type[k] == "covariate product") {
      # the authors are what comes before the (YEAR), when there is one
      prefix <- regmatches(refs$citation[k],
                           regexpr("^[^(]+(?=\\s*\\(\\d{4}\\))", refs$citation[k], perl = TRUE))
      people <- if (length(prefix) == 1) utils::person(family = trimws(prefix)) else NULL
    } # end covariate product condition
    # a @misc entry needs no author, so a reference without one simply leaves it out
    utils::bibentry(
      bibtype = "Misc",
      key = refs$id[k],
      title = if (is.na(refs$title[k])) refs$citation[k] else refs$title[k],
      author = people,
      year = if (is.na(refs$year[k])) "n.d." else refs$year[k],
      note = if (refs$type[k] == "covariate product") NULL else
        paste(c(if (!is.na(refs$version[k])) paste("Version", refs$version[k]),
                if (nzchar(refs$licenses[k])) paste("Licence:", refs$licenses[k]),
                accessed_text), collapse = ". "),
      url = if (is.na(refs$url[k])) NULL else refs$url[k]
    ) # end bibentry
  }) # end per reference
  # combine them into one bibentry object
  return(do.call(c, entries))
} # end cite_datapackages


#
##
### Helpers for cite_datapackages() ----

#' Is this object a single data package?
#'
#' @param x Any object.
#' @return `TRUE` when `x` is a named list with a package `id`, `name` or `title`.
#' @author Zachary Amir
#' @noRd
.is_datapackage <- function(x) {
  # a package is a named list
  if (!is.list(x) || is.null(names(x))) return(FALSE)
  # carrying at least one of the keys that identify it
  return(any(c("id", "name", "title") %in% names(x)))
} # end .is_datapackage

#' The first of several values that is non-empty text
#'
#' @param ... Candidate values, in order of preference.
#' @return One character string, or `NA_character_` if none is usable.
#' @author Zachary Amir
#' @noRd
.first_text <- function(...) {
  # for each candidate, in order
  for (v in list(...)) {
    # take the first element of a vector or list
    if (length(v) > 0) v <- unlist(v)[1] else next
    # and keep it if it is real text
    if (!is.null(v) && !is.na(v) && nzchar(trimws(as.character(v)))) return(trimws(as.character(v)))
  } # end per candidate
  # nothing usable
  return(NA_character_)
} # end .first_text

#' The authors named at the start of a citation
#'
#' @param citation A citation that starts "Authors (YEAR).".
#' @return A character vector of author names, empty if the citation has no
#'   "(YEAR)" to find them by.
#' @author Zachary Amir
#' @noRd
.citation_authors <- function(citation) {
  # the authors are everything before the first "(YEAR)"
  m <- regmatches(citation, regexpr("^[^(]+(?=\\s*\\(\\d{4}\\))", citation, perl = TRUE))
  # no year in brackets means no author list to read
  if (length(m) == 0) return(character(0))
  # split on commas, ampersands and a final "and"
  names_found <- trimws(strsplit(m, ",|&|\\band\\b")[[1]])
  # and drop the empty pieces a trailing comma leaves
  return(names_found[nzchar(names_found)])
} # end .citation_authors

#' The year in brackets in a citation
#'
#' @param citation A citation.
#' @return The first four-digit year in round brackets, or `NA_character_`.
#' @author Zachary Amir
#' @noRd
.citation_year <- function(citation) {
  # the first (YYYY)
  m <- regmatches(citation, regexpr("\\((\\d{4})[a-z]?\\)", citation))
  # none found
  if (length(m) == 0) return(NA_character_)
  # keep the digits only
  return(substr(gsub("[^0-9]", "", m), 1, 4))
} # end .citation_year

#' The contributors credited as authors
#'
#' @param contributors The `contributors` list of a Camtrap DP descriptor.
#' @return A character vector of names, in the order given.
#' @author Zachary Amir
#' @noRd
.contributor_authors <- function(contributors) {
  # the roles that credit the data rather than hold or publish it
  author_roles <- c("contact", "principalInvestigator", "contributor")
  # for each contributor
  keep <- vapply(contributors, function(c) {
    # someone with no role is credited, as the Camtrap DP default
    is.null(c$role) || c$role %in% author_roles
  }, logical(1))
  # their names, skipping any without one
  out <- vapply(contributors[keep], function(c) .first_text(c$title), character(1))
  return(unique(out[!is.na(out)]))
} # end .contributor_authors

#' The version of the data in a package, if it is worth stating
#'
#' @param dp A data package.
#' @return The WildObs database release, else a package version that is not just
#'   the Camtrap DP standard version, else `NA_character_`.
#' @author Zachary Amir
#' @noRd
.data_version <- function(dp) {
  # WildObs stores the release as text, or as a block holding currentVersion
  wildobs <- dp$versionControlWildObs
  if (is.list(wildobs)) wildobs <- wildobs$currentVersion
  wildobs <- .first_text(wildobs)
  if (!is.na(wildobs)) return(wildobs)
  # otherwise the package version
  v <- .first_text(dp$version)
  # which is often only the standard's version, read from the profile URL
  profile_version <- .first_text(basename(dirname(.first_text(dp$profile))))
  if (!is.na(v) && identical(v, profile_version)) return(NA_character_)
  return(v)
} # end .data_version

#' A persistent identifier for a package
#'
#' @param dp A data package.
#' @param citation Its citation, or `NA`.
#' @return The RAiD in the citation or related identifiers, else a DOI the package
#'   names as itself, else `NA_character_`.
#' @author Zachary Amir
#' @noRd
.package_identifier <- function(dp, citation) {
  # a RAiD written into the citation
  if (!is.na(citation)) {
    raid <- regmatches(citation, regexpr("https?://raid\\.org/[^ ]+[^ .]", citation))
    if (length(raid) == 1) return(raid)
  } # end citation condition
  # a RAiD among the related identifiers
  for (r in dp$relatedIdentifiers) {
    if (identical(r$relatedIdentifierType, "RAiD")) return(.first_text(r$relatedIdentifier))
  } # end per related identifier
  # a DOI as the package id
  id <- .first_text(dp$id)
  if (!is.na(id) && grepl("^(https?://doi\\.org/)?10\\.", id)) return(id)
  return(NA_character_)
} # end .package_identifier

#' The covariate products cited in a package's covariates schema
#'
#' @param dp A data package.
#' @return A data frame with columns `citation` and `doi`, one row per distinct
#'   product, or `NULL` when the package has no covariates table.
#' @author Zachary Amir
#' @noRd
.covariate_sources <- function(dp) {
  # find the covariates resource, if the package has one
  res <- Filter(function(r) identical(r$name, "covariates"), dp$resources)
  if (length(res) == 0 || is.null(res[[1]]$schema$fields)) return(NULL)
  # each field may name its source under custom
  srcs <- lapply(res[[1]]$schema$fields, function(f) f$custom$source)
  # keep the fields that cite one
  srcs <- Filter(function(s) !is.null(s$citation), srcs)
  if (length(srcs) == 0) return(NULL)
  # one row per field, then one per product
  out <- data.frame(
    citation = vapply(srcs, function(s) .first_text(s$citation), character(1)),
    doi = vapply(srcs, function(s) .first_text(s$doi, s$url), character(1)),
    stringsAsFactors = FALSE)
  return(unique(out))
} # end .covariate_sources

#' A BibTeX person from a name as written in a citation
#'
#' @param name A name such as "Zachary Amir" or "Desmet P".
#' @return A [utils::person()].
#' @author Zachary Amir
#' @noRd
.as_bib_person <- function(name) {
  # "Family I" or "Family IJ": the initials come last
  if (grepl("^.+ [A-Z]{1,3}$", name)) {
    parts <- regmatches(name, regexec("^(.+) ([A-Z]{1,3})$", name))[[1]]
    return(utils::person(given = parts[3], family = parts[2]))
  } # end initials condition
  # otherwise "Given Family", which as.person() splits on the last space
  return(utils::as.person(name))
} # end .as_bib_person
