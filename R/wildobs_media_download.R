#' Download the Media Files Listed in a Camtrap DP Media Table
#'
#' Fetches the image and video files referenced by `media$filePath` and saves them
#' to a folder, recording what happened to each one. Works on the media table from
#' `wildobs_dp_download(..., media = TRUE)` or any Camtrap DP media table.
#'
#' @details
#' Each row is handled according to where its `filePath` points:
#' \enumerate{
#'   \item **Web addresses** (`http://`, `https://`) are downloaded.
#'   \item **Google Cloud Storage paths** (`gs://bucket/...`) are downloaded through
#'     `https://storage.googleapis.com/bucket/...`. This only works if the bucket is
#'     public, or if you supply a `gcs_token` for an account with read access to it.
#'   \item **Paths on this computer** (e.g. `F:\\camera\\IMG_0001.JPG`) are copied if
#'     the file exists here. This lets data providers collect their own original
#'     images without any credentials.
#'   \item Anything else, such as a placeholder like `not_provided` or a path on
#'     someone else's computer, is skipped.
#' }
#'
#' Files are saved as `out_dir/<projectName>/<deploymentID>/<mediaID>.<ext>`
#' (without the `projectName` folder if that column is absent), keeping the
#' original file extension. Characters that are not allowed in file names are
#' replaced with `_`. A file already on disk is not fetched again unless
#' `overwrite = TRUE`, so an interrupted run can simply be repeated.
#'
#' Files are downloaded in batches of 100, several at a time within each batch, with a
#' 60 second connection timeout. Queueing every file at once made later files time out
#' while they waited for a connection to `data.wildobs.org.au`.
#'
#' A download only counts as successful if the server answered with a 2xx status
#' and did not send back a web page. Anything else is recorded as `failed`, and the
#' partial file is deleted.
#'
#' Most WildObs media cannot be downloaded by the public. Only files with
#' `filePublic = TRUE`, currently about 1% of WildObs media, are openly hosted.
#' To fetch only those, filter first: `media[media$filePublic, ]`.
#'
#' @param media A data frame of media records with at least `mediaID`,
#'   `deploymentID` and `filePath` columns, e.g. `dp$data$media`.
#' @param out_dir Character string. The folder to save files into. Created if it
#'   does not exist.
#' @param overwrite Logical. Re-fetch files that already exist in `out_dir`.
#'   Defaults to `FALSE`.
#' @param gcs_token Character string or `NULL`. An OAuth access token for Google
#'   Cloud Storage, e.g. the output of `gcloud auth print-access-token`. It is sent
#'   only to `storage.googleapis.com`. Defaults to `NULL` (no authentication).
#'
#' @return The `media` data frame with three added columns:
#'   \describe{
#'     \item{localPath}{Where the file was, or would be, saved.}
#'     \item{downloadStatus}{`downloaded`, `copied`, `already_exists`, `skipped`,
#'       or `failed`.}
#'     \item{downloadNote}{A short explanation, e.g. `HTTP 403 (no permission)`.}
#'   }
#'   Stops if `media` lacks the required columns or `out_dir` is not a single
#'   folder path.
#'
#' @examples
#' \dontrun{
#' api_key <- Sys.getenv("WILDOBSR_API_KEY")
#' dp <- wildobs_dp_download(api_key = api_key,
#'                           project_ids = "QLD_Dwyers_Scrub_ANIM3018_2023_WildObsID_0005",
#'                           media = TRUE)[[1]]
#'
#' # Download a small batch of the publicly hosted images first
#' media <- dp$data$media
#' media_public <- media[media$filePublic, ]
#' result <- wildobs_media_download(head(media_public, 20), out_dir = "camera_images")
#'
#' # See what happened to each file
#' table(result$downloadStatus)
#' }
#'
#' @author Zachary Amir & Claude Opus 5.5
#'
#' @importFrom curl multi_download
#'
#' @export
wildobs_media_download <- function(media, out_dir, overwrite = FALSE, gcs_token = NULL) {

  ## make sure we were handed a media table we can work with
  if (!is.data.frame(media)) {
    stop("'media' must be a data frame, such as dp$data$media from ",
         "wildobs_dp_download(..., media = TRUE).", call. = FALSE)
  } # end data frame check
  # these three columns are what every row needs
  missing_cols <- setdiff(c("mediaID", "deploymentID", "filePath"), names(media))
  if (length(missing_cols) > 0) {
    stop("'media' is missing the column(s): ", paste(missing_cols, collapse = ", "),
         "\nPlease provide a Camtrap DP media table.", call. = FALSE)
  } # end column check
  # and somewhere to put the files
  if (missing(out_dir) || !is.character(out_dir) || length(out_dir) != 1 || !nzchar(out_dir)) {
    stop("Please provide 'out_dir', a single folder path to save the files into.",
         call. = FALSE)
  } # end out_dir check

  # how many rows we are working through
  n <- nrow(media)
  # the paths as plain text, with missing ones as empty strings
  path <- as.character(media$filePath)
  path[is.na(path)] <- ""

  #
  ##
  ### Sort each row by where its file lives ----

  ## web addresses and Google Cloud paths are fetched, files on this computer are
  ## copied, and everything else (placeholders, other people's drives) is skipped
  kind <- ifelse(grepl("^https?://", path), "web",
                 ifelse(grepl("^gs://", path), "gcs",
                        ifelse(nzchar(path) & file.exists(path), "local", "none")))

  # the address to request, translating gs://bucket/... to Google's web endpoint
  url <- sub("^gs://", "https://storage.googleapis.com/", path)

  #
  ##
  ### Work out where each file goes ----

  ## a quick helper to swap out characters that are not allowed in file names
  safe_name <- function(x) gsub("[/\\\\:*?\"<>|]", "_", as.character(x))

  # take the extension from the path, ignoring any ?query on the end of a URL
  file_part <- basename(sub("\\?.*$", "", path))
  ext <- ifelse(grepl("\\.[A-Za-z0-9]+$", file_part),
                tolower(sub("^.*\\.([A-Za-z0-9]+)$", "\\1", file_part)),
                NA_character_)
  # if the path has no extension, fall back on the media type, e.g. image/jpeg -> jpeg
  if ("fileMediatype" %in% names(media)) {
    from_type <- sub("^[a-z]+/", "", tolower(as.character(media$fileMediatype)))
    use_type <- is.na(ext) & grepl("^(image|video|audio)/[a-z0-9]+$", tolower(media$fileMediatype))
    ext[use_type] <- from_type[use_type]
  } # end media type fallback
  # and jpg as the last resort, since nearly all camera trap media are JPEGs
  ext[is.na(ext)] <- "jpg"

  # one folder per deployment, inside one per project when we know the project
  folder <- if ("projectName" %in% names(media)) {
    file.path(out_dir, safe_name(media$projectName), safe_name(media$deploymentID))
  } else {
    file.path(out_dir, safe_name(media$deploymentID))
  } # end folder condition
  # each file is named by its unique mediaID
  local_path <- file.path(folder, paste0(safe_name(media$mediaID), ".", ext))

  #
  ##
  ### Decide what actually needs doing ----

  # everything starts as skipped, and gets updated as it is handled
  status <- rep("skipped", n)
  note <- rep("no reachable file: a placeholder, or a path not on this computer", n)

  ## dont fetch files we already have, unless asked to
  have_it <- kind != "none" & file.exists(local_path)
  if (!isTRUE(overwrite)) {
    status[have_it] <- "already_exists"
    note[have_it] <- "kept the file already in out_dir"
  } # end overwrite condition
  # the rows still to fetch
  todo <- kind != "none" & status != "already_exists"

  # report how much of the table can be attempted before starting
  message(sprintf("%d of %d media files have a path that can be tried (%d web, %d Google Cloud, %d on this computer).",
                  sum(todo), n, sum(todo & kind == "web"), sum(todo & kind == "gcs"),
                  sum(todo & kind == "local")))

  # make every folder we are about to write into
  for (f in unique(dirname(local_path[todo]))) {
    dir.create(f, recursive = TRUE, showWarnings = FALSE)
  } # end per folder

  #
  ##
  ### Copy files that are on this computer ----

  # rows whose file sits on a drive this computer can see
  i_local <- which(todo & kind == "local")
  if (length(i_local) > 0) {
    # copy them across, replacing any older copy
    copied <- file.copy(path[i_local], local_path[i_local], overwrite = TRUE)
    status[i_local] <- ifelse(copied, "copied", "failed")
    note[i_local] <- ifelse(copied, "copied from this computer", "could not copy the local file")
  } # end local copy

  #
  ##
  ### Download web and Google Cloud files ----

  ## the two groups are downloaded separately so a Google token is only ever
  ## sent to Google, never to any other website
  for (k in c("web", "gcs")) {
    # rows of this kind still to fetch
    i <- which(todo & kind == k)
    # nothing of this kind, so move on
    if (length(i) == 0) next

    # only attach the token to Google Cloud requests
    headers <- if (k == "gcs" && !is.null(gcs_token)) {
      paste("Authorization: Bearer", gcs_token)
    } else {
      character(0)
    } # end token condition

    ### fetch them in batches of 100, several at a time within each batch
    ## curl's connect timeout also counts the time a file waits in line for a free
    ## connection, so queueing thousands at once makes the late ones time out with
    ## "0 bytes received". Small batches keep every wait short, and the 60 second
    ## timeout gives a slow server time to answer.
    batches <- split(i, ceiling(seq_along(i) / 100))
    res <- NULL
    for (b in seq_along(batches)) {
      # rows in this batch
      rows <- batches[[b]]
      # download them, sending the token header only for Google Cloud
      batch_res <- curl::multi_download(url[rows], local_path[rows], progress = FALSE,
                                        connecttimeout = 60, httpheader = headers)
      # and stack the results in the same order as i
      res <- rbind(res, batch_res)
      # report progress on long runs
      if (length(batches) > 1) {
        message(sprintf("  %s files: %d of %d done", if (k == "web") "web" else "Google Cloud",
                        min(b * 100, length(i)), length(i)))
      } # end progress condition
    } # end per batch

    ## curl calls a request successful whenever the server answered, even with a
    ## 403, so judge by the status code, and reject web pages posing as media
    code <- res$status_code
    is_page <- grepl("^text/html", res$type)
    ok <- !is.na(code) & code >= 200 & code < 300 & !is_page

    # record the outcome for each file
    status[i] <- ifelse(ok, "downloaded", "failed")
    note[i] <- ifelse(ok, "downloaded",
                      ifelse(!is.na(res$error), res$error,
                             ifelse(is_page, "the address returned a web page, not a media file",
                                    ifelse(code %in% c(401, 403),
                                           sprintf("HTTP %d (no permission)", code),
                                           sprintf("HTTP %d", code)))))
    # and remove anything a failed request left behind
    bad <- local_path[i][!ok]
    unlink(bad[file.exists(bad)])
  } # end per download kind

  #
  ##
  ### Hand back the table with what happened to each file ----

  # add the results as new columns
  media$localPath <- local_path
  media$downloadStatus <- status
  media$downloadNote <- note

  # summarise the run, e.g. "downloaded: 18 | failed: 2"
  counts <- table(status)
  message(paste(names(counts), counts, sep = ": ", collapse = " | "))

  return(media)
} # end function
