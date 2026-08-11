# Package load hooks (issue #114).
#
# .onAttach() runs when a user calls library(WildObsR). Everything in here is
# best effort: an offline HPC compute node must still attach the package
# cleanly, so no failure in this file is ever allowed to surface.


#' WildObs API Base Endpoint
#'
#' The endpoint used for the load-time reachability probe. Kept in one place so
#' the probe and its tests cannot drift apart.
#'
#' @return A single URL string.
#'
#' @author Zachary Amir & Claude Opus 5
#'
#' @keywords internal
#' @noRd
.wildobs_api_url <- function() {
  "https://camdbapi.wildobs.org.au/find"
}


#' Check Whether the WildObs API Answers
#'
#' Best-effort reachability probe. Asks only whether the service completed a
#' round trip, not whether any particular request would succeed.
#'
#' @details
#' The HTTP status code is deliberately ignored. `/find` expects a POST with an
#' API key, so a bodyless probe gets 405 Method Not Allowed, and the service root
#' gets 404. Both prove the host is alive and routing, which is the only thing
#' this probe is trying to establish. Treating a non-200 as a failure here would
#' produce a startup warning on every single load.
#'
#' What does count as unreachable is a failure to complete the round trip at all:
#' DNS failure, connection refused, or the 2-second timeout expiring.
#'
#' @param url Character string. The endpoint to probe. Parameterised so tests can
#'   point it at a deliberately unresolvable host.
#'
#' @return `TRUE` if the service answered, `FALSE` otherwise. Never errors.
#'
#' @author Zachary Amir & Claude Opus 5
#'
#' @importFrom curl new_handle handle_setopt curl_fetch_memory
#'
#' @keywords internal
#' @noRd
.wildobs_api_reachable <- function(url = .wildobs_api_url()) {
  tryCatch({
    # Same 2-second ceiling as the version check. This runs on every
    # library(WildObsR), so it must never be something a user waits on.
    h <- curl::new_handle(timeout = 2, connecttimeout = 2)

    # Headers only. We do not need a body to know the host answered.
    curl::handle_setopt(h, nobody = TRUE)

    curl::curl_fetch_memory(url, handle = h)

    # Reaching this line means a response came back, whatever its status.
    TRUE
  },
  error = function(e) FALSE,
  warning = function(w) FALSE)
}


#' Package Attach Hook
#'
#' Runs a single best-effort database reachability check when the package is
#' attached, and says nothing at all unless something is actually wrong.
#'
#' @details
#' Design rules, all deliberate:
#' \enumerate{
#'   \item **Silent on success.** Users learn to ignore output that appears every
#'     time, so the package only speaks when the message means something.
#'   \item **`packageStartupMessage()`**, never `message()` or `cat()`, so the
#'     output can be silenced with `suppressPackageStartupMessages()`.
#'   \item **Wrapped in `tryCatch()` and given an explicit timeout.** Attaching
#'     the package must never fail or hang because of a network problem.
#'   \item **Once per session**, cached in `.wildobsr_warned`.
#' }
#'
#' @param libname Library path. Supplied by R; unused.
#' @param pkgname Package name. Supplied by R; unused.
#'
#' @return Invisibly `NULL`.
#'
#' @author Zachary Amir & Claude Opus 5
#'
#' @keywords internal
#' @noRd
.onAttach <- function(libname, pkgname) {
  tryCatch({
    # Only probe once per session.
    if (isTRUE(.wildobsr_warned$db_status)) return(invisible(NULL))
    .wildobsr_warned$db_status <- TRUE

    # Stay completely silent when the service answers.
    if (.wildobs_api_reachable()) return(invisible(NULL))

    # Only speak when the connection genuinely failed.
    packageStartupMessage(
      "WildObsR could not reach the WildObs database at ", .wildobs_api_url(), ".\n",
      "Downloading data will not work until that connection returns. This is ",
      "usually a local network or VPN problem.\nIf it persists, contact the ",
      "WildObs team at support@wildobs.org.au"
    )
  },
  # Attaching the package must succeed even if everything above goes wrong.
  error = function(e) invisible(NULL))

  invisible(NULL)
}
