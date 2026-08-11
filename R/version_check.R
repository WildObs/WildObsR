# Runtime version checking (issue #114).
#
# Split deliberately into three pieces:
#   .fetch_remote_version()   talks to the network and nothing else
#   .compare_versions()       pure, no network, so it can be fully unit tested
#   .check_wildobs_version()  wires the two together and caches the result
#
# The split is what makes this testable without mocking: every decision about
# what to say to the user lives in .compare_versions(), which takes two plain
# strings and returns either NULL or a message.


#' Fetch the Version Field from the GitHub DESCRIPTION
#'
#' Reads the `Version:` field from the DESCRIPTION file on GitHub so the
#' installed version can be compared against the released one.
#'
#' @details
#' Two protections apply, and both are needed:
#' \enumerate{
#'   \item An explicit 2-second `timeout` and `connecttimeout` on the curl
#'     handle. `tryCatch()` catches errors but not slowness, so without this a
#'     silently dropping connection would block the user's session until the OS
#'     default gives up, which can exceed a minute.
#'   \item A `tryCatch()` around everything, so DNS failure, a proxy, a captive
#'     portal, or malformed content all return `NULL` rather than propagating.
#' }
#'
#' The URL uses `HEAD` rather than a named branch, so it keeps working if the
#' default branch is ever renamed between `main` and `master`.
#'
#' While the repository is private this returns `NULL`, because GitHub answers
#' with HTTP 404. That is the expected pre-release state and is handled silently.
#'
#' @return A single version string such as `"0.2.0"`, or `NULL` on any failure.
#'   Never errors, never warns, never messages.
#'
#' @author Zachary Amir
#'
#' @importFrom curl new_handle curl_fetch_memory
#'
#' @keywords internal
#' @noRd
.fetch_remote_version <- function() {
  tryCatch({
    # 2-second ceiling. A version check is never worth making the user wait.
    h <- curl::new_handle(timeout = 2, connecttimeout = 2)

    # HEAD resolves to whatever the default branch is called.
    res <- curl::curl_fetch_memory(
      "https://raw.githubusercontent.com/WildObs/WildObsR/HEAD/DESCRIPTION",
      handle = h
    )

    # Anything other than 200 means we did not get a DESCRIPTION back.
    # A private repository returns 404 here.
    if (!identical(res$status_code, 200L)) return(NULL)

    # Pull just the Version: line out of the raw DESCRIPTION text.
    txt <- rawToChar(res$content)
    matched <- regmatches(txt, regexpr("(?m)^Version:[ \t]*(.+)$", txt, perl = TRUE))
    if (length(matched) == 0) return(NULL)

    # Strip the field name and any surrounding whitespace.
    trimws(sub("^Version:[ \t]*", "", matched))
  },
  # Any failure at all, including a warning, yields NULL.
  error = function(e) NULL,
  warning = function(w) NULL)
}


#' Compare an Installed Version Against a Remote Version
#'
#' Decides whether the user should be told to update. Pure: takes two strings,
#' touches no network, and returns either `NULL` or the message to show.
#'
#' @details
#' Keeping this separate from the network call is deliberate. It means every
#' branch below is unit testable with no mocking, no fixtures, and no
#' `skip_if_offline()`.
#'
#' Comparison uses `package_version()` rather than string comparison, because
#' `"0.10.0" > "0.9.0"` is `FALSE` when compared as text but `TRUE` as versions.
#'
#' Returns `NULL`, meaning stay silent, whenever:
#' \enumerate{
#'   \item `remote` is `NULL`, i.e. the network check could not run.
#'   \item Either input is missing, empty, or not a single value.
#'   \item Either input is not a valid version string.
#'   \item The installed version is equal to or ahead of the remote version.
#' }
#'
#' @param installed Character string. The installed package version.
#' @param remote Character string, or `NULL`. The version found on GitHub.
#'
#' @return A single character string to pass to `warning()`, or `NULL` if there
#'   is nothing worth saying.
#'
#' @author Zachary Amir
#'
#' @keywords internal
#' @noRd
.compare_versions <- function(installed, remote) {
  # No remote version means the check could not run. Say nothing.
  if (is.null(remote) || is.null(installed)) return(NULL)

  # Guard against vectors, empty strings and NA before parsing.
  if (length(remote) != 1 || length(installed) != 1) return(NULL)
  if (is.na(remote) || is.na(installed)) return(NULL)
  if (!nzchar(trimws(remote)) || !nzchar(trimws(installed))) return(NULL)

  # package_version() errors on malformed input, so a bad string must not
  # escape as an error to the user.
  parsed <- tryCatch(
    list(installed = package_version(installed),
         remote    = package_version(remote)),
    error = function(e) NULL,
    warning = function(w) NULL
  )
  if (is.null(parsed)) return(NULL)

  # Only speak when the installed copy is genuinely behind the release.
  if (parsed$installed >= parsed$remote) return(NULL)

  # Plain language first, then the one command to fix it.
  paste0(
    "You are using WildObsR ", installed, ", but ", remote, " is available. ",
    "Older versions can download data incorrectly, so please update by running: ",
    "devtools::install_github(\"WildObs/WildObsR\")"
  )
}


#' Warn Once per Session if WildObsR Is Out of Date
#'
#' Called at the top of the functions that connect to WildObs. Fetches the
#' released version, compares it to the installed one, and warns if behind.
#'
#' @details
#' The result is cached in `.wildobsr_warned` so that repeated calls in a single
#' session hit GitHub only once. The cache flag is set *before* the network call,
#' so even a slow or failing fetch happens at most once per session.
#'
#' This warns rather than stops: the current database change is backwards
#' compatible, so an out-of-date user must still be able to download data.
#'
#' @return Invisibly `NULL`. Called for the side effect of warning.
#'
#' @author Zachary Amir
#'
#' @importFrom utils packageVersion
#'
#' @keywords internal
#' @noRd
.check_wildobs_version <- function() {
  # Only ever run once per session, so repeat calls do not re-hit GitHub.
  if (isTRUE(.wildobsr_warned$version_check)) return(invisible(NULL))
  .wildobsr_warned$version_check <- TRUE

  # Ask GitHub what the current release is; NULL if that failed for any reason.
  remote <- .fetch_remote_version()

  # Decide what, if anything, to say.
  msg <- .compare_versions(
    installed = as.character(utils::packageVersion("WildObsR")),
    remote    = remote
  )

  # Silent when up to date. Only speak when the user is genuinely behind.
  if (!is.null(msg)) warning(msg, call. = FALSE)

  invisible(NULL)
}
