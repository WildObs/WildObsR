# Determine Single-Season Sampling Efforts from Camera Trap Images

This function analyzes date/time information from camera trap images to
identify spatially and temporally discrete sampling efforts. The
function updates the input data with new survey and deployment IDs by
analyzing spatial and temporal patterns in camera detections. Users can
specify the maximum duration of a survey and the maximum gap between
detections before a new survey is triggered. This is useful for
organizing images into surveys and deployments for further analysis.

## Usage

``` r
survey_and_deployment_generator(
  caps,
  deps,
  cam_long = 20,
  max_dur = 100,
  szn_filter = TRUE,
  cam_filter = TRUE
)
```

## Arguments

- caps:

  A data frame of camera trap observations that contains date/time
  information for each image. It must include columns named
  `eventStart`, `deploymentID`, `dataSource`, and `locationName`.

- deps:

  A data frame of camera deployments that contains location information
  for each camera deployment. It must include columns named
  `deploymentID` and `locationName`.

- cam_long:

  A numeric value (in days) representing the maximum gap between
  detections before a new survey is started. Defaults to 20 days.

- max_dur:

  A numeric value (in days) representing the maximum duration a survey
  can last before starting a new one. Defaults to 100 days.

- szn_filter:

  A logical value indicating whether to filter out short survey periods.
  If `TRUE`, surveys with fewer detections within a defined time frame
  will be combined with adjacent surveys. Defaults to `TRUE`.

- cam_filter:

  A logical value indicating whether to use the cam_long argument to
  look for natural breaks in detections. This is best set to `FALSE`
  when there are manually created start/stop records when a data
  provider has not provided all records. Defaults to `TRUE`.

## Value

A data frame based on the `caps` input, but with additional columns:

- new_deploymentID:

  A new deployment identifier that reflects updated deployment groups.

- new_deploymentGroups:

  A new survey identifier that represents the adjusted survey groupings
  based on the detection intervals and survey duration.

- new_locationName:

  A new location identifier combining location and deployment
  information.

## Details

This function assumes that camera coordinates and date/time data have
been verified for accuracy and are free of errors. Spatially distinct
groupings of cameras should be indicated in the `locationName` column in
the deployments data frame. The function will:

1.  Group detections into surveys based on the `max_dur` (maximum survey
    duration) and `cam_long` (maximum gap between detections).

2.  Assign new deployment IDs and survey IDs, ensuring each deployment
    has unique identifiers based on the adjusted time periods.

3.  Handle cameras that are active over long periods and split their
    data based on natural breaks in detections or user-defined
    thresholds.

4.  Optionally filter out short survey periods based on the `szn_filter`
    parameter.

## Author

Zachary Amir
