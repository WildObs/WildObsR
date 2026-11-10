# Download the Media Files Listed in a Camtrap DP Media Table

Fetches the image and video files referenced by `media$filePath` and
saves them to a folder, recording what happened to each one. Works on
the media table from `wildobs_dp_download(..., media = TRUE)` or any
Camtrap DP media table.

## Usage

``` r
wildobs_media_download(media, out_dir, overwrite = FALSE, gcs_token = NULL)
```

## Arguments

- media:

  A data frame of media records with at least `mediaID`, `deploymentID`
  and `filePath` columns, e.g.
  `frictionless::read_resource(dp, "media")`.

- out_dir:

  Character string. The folder to save files into. Created if it does
  not exist.

- overwrite:

  Logical. Re-fetch files that already exist in `out_dir`. Defaults to
  `FALSE`.

- gcs_token:

  Character string or `NULL`. An OAuth access token for Google Cloud
  Storage, e.g. the output of `gcloud auth print-access-token`. It is
  sent only to `storage.googleapis.com`. Defaults to `NULL` (no
  authentication).

## Value

The `media` data frame with three added columns:

- localPath:

  Where the file was, or would be, saved.

- downloadStatus:

  `downloaded`, `copied`, `already_exists`, `skipped`, or `failed`.

- downloadNote:

  A short explanation, e.g. `HTTP 403 (no permission)`.

Stops if `media` lacks the required columns or `out_dir` is not a single
folder path.

## Details

Each row is handled according to where its `filePath` points:

1.  **Web addresses** (`http://`, `https://`) are downloaded.

2.  **Google Cloud Storage paths** (`gs://bucket/...`) are downloaded
    through `https://storage.googleapis.com/bucket/...`. This only works
    if the bucket is public, or if you supply a `gcs_token` for an
    account with read access to it.

3.  **Paths on this computer** (e.g. `F:\\camera\\IMG_0001.JPG`) are
    copied if the file exists here. This lets data providers collect
    their own original images without any credentials.

4.  Anything else, such as a placeholder like `not_provided` or a path
    on someone else's computer, is skipped.

Files are saved as
`out_dir/<projectName>/<deploymentID>/<mediaID>.<ext>` (without the
`projectName` folder if that column is absent), keeping the original
file extension. Characters that are not allowed in file names are
replaced with `_`. A file already on disk is not fetched again unless
`overwrite = TRUE`, so an interrupted run can simply be repeated.

Files are downloaded in batches of 100, several at a time within each
batch, with a 60 second connection timeout. Queueing every file at once
made later files time out while they waited for a connection to
`data.wildobs.org.au`.

A download only counts as successful if the server answered with a 2xx
status and did not send back a web page. Anything else is recorded as
`failed`, and the partial file is deleted.

Most WildObs media cannot be downloaded by the public. Only files with
`filePublic = TRUE`, currently about 3% of public WildObs media, are
openly hosted. To fetch only those, filter first:
`media[media$filePublic, ]`.

## Author

Zachary Amir & Claude Opus 5.5

## Examples

``` r
if (FALSE) { # \dontrun{
api_key <- Sys.getenv("WILDOBSR_API_KEY")
dp <- wildobs_dp_download(api_key = api_key,
                          project_ids = "QLD_Dwyers_Scrub_ANIM3018_2023_WildObsID_0005",
                          media = TRUE)[[1]]

# Download a small batch of the publicly hosted images first
media <- frictionless::read_resource(dp, "media")
media_public <- media[media$filePublic, ]
result <- wildobs_media_download(head(media_public, 20), out_dir = "camera_images")

# See what happened to each file
table(result$downloadStatus)
} # }
```
