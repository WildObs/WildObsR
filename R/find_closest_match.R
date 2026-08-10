#' Find Closest Matches in Two Character Vectors
#'
#' @description
#' **Deprecated as of WildObsR 0.2.0.** `find_closest_match()` will be removed in
#' a future release. The function still runs normally; it emits a warning once
#' per session (it is commonly passed to `sapply()`, so an unguarded warning
#' would fire once per element).
#'
#' This function finds the closest match between species names provided in an unverified list
#' and a verified species taxonomy database. It handles differences in spelling or capitalization.
#'
#' @details This function internally uses \code{sapply()} to apply string distance calculations
#' across all species names in the \code{string} vector. The closest match for each name is determined
#' using the Jaro-Winkler string distance method from the \code{stringdist} package.
#'
#' @param string This is a character vector of species names in the observations that have no matching value in the verified species taxonomy database.
#' @param target_strings This is a character vector of the species names in the verified species taxonomy database to compare against.
#'
#' @return A data frame containing the original species names and their closest match from the verified species taxonomy database.
#'
#' @importFrom stringdist stringdist
#'
#' @author Zachary Amir & ChatGPT
#'
#'
#' @keywords internal
find_closest_match <- function(string, target_strings) {

  # Warn only once per session. This function is passed to sapply() in
  # collaborator scripts, so an unguarded warning fires once per element.
  if (!isTRUE(.wildobsr_warned$find_closest_match)) {
    .Deprecated(
      msg = paste0(
        "find_closest_match() is deprecated as of WildObsR 0.2.0 and will be ",
        "removed in a future release."
      )
    )
    .wildobsr_warned$find_closest_match <- TRUE
  }

  # Function to find the closest match for each species name
  match_func <- function(single_string) {
    dists <- stringdist::stringdist(single_string, target_strings, method = "jw")
    closest_match <- target_strings[which.min(dists)]
    return(closest_match)
  }

  # Apply the match function to each string using sapply
  closest_matches <- sapply(string, match_func, USE.NAMES = FALSE)

  # Create a data frame with the original and closest matches
  result <- data.frame(
    original = string,
    verified_match = closest_matches,
    stringsAsFactors = FALSE
  )

  return(result)
}
