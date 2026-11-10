# Reclassify eventID Values Using a New Temporal Threshold

The WildObsR pipeline pre-calculates `deltaTime_event` (seconds since
the previous independent observation of the same species at the same
deployment) and groups records into events using a fixed independence
threshold (default 30 minutes). This function reuses that pre-calculated
column to redraw event boundaries at a different, user-supplied
threshold, without re-parsing `dateTime`, making it much faster than any
temporal grouping analysis.

This is useful for identifying "continuous" detections that exceed the
default independence threshold (e.g., an animal resting in front of a
camera for hours): rather than discarding or condensing these
observations, users can reclassify them under a longer threshold and
decide for themselves whether to keep or remove them.

## Usage

``` r
reclassify_eventID(obs, new_event_time)
```

## Arguments

- obs:

  A data frame or tibble of observations (e.g., from a Camtrap DP
  `observations` resource) that has already been processed by the
  WildObsR pipeline. Must include the columns `deploymentID`,
  `scientificName`, `eventStart`, and `deltaTime_event`.

- new_event_time:

  A single positive numeric value giving the new independence threshold,
  in minutes, used to group observations into events.

## Value

The input `obs` data frame with `eventID` recalculated using the new
threshold. All other columns, including `eventStart` and `eventEnd`, are
left unchanged. The original row order is preserved.

## Details

For a given `new_event_time` threshold (in minutes), a row is flagged as
the start of a new event if `deltaTime_event == 0` (the first record of
a deploymentID/scientificName group) or `deltaTime_event` exceeds the
new threshold (converted to seconds). A cumulative sum of those boundary
flags, within each `deploymentID` + `scientificName` group and ordered
by `eventStart`, produces a sequential event counter that increments
only at genuine boundaries. Rows where `deltaTime_event` is `NA` (i.e.,
non-boundary rows) do not increment the counter.

The new `eventID` is built as
`deploymentID_scientificName_<N>min_event_<counter>`, with spaces in
`scientificName` replaced by underscores. `scientificName` is embedded
in the eventID because the event counter calculated here resets per
species per deployment, unlike the sequential counter used to build
eventID in the WildObsR pipeline (which is unique across all species at
a deploymentID). Including `scientificName` keeps the new eventID unique
in the same way.

`eventStart` and `eventEnd` are intentionally left untouched by this
function, as they reflect the temporal span of each observationID's
media files rather than the event-grouping threshold. Because a longer
threshold can combine multiple pre-existing events (and their
`eventStart`/ `eventEnd` values) under a single new `eventID`, users who
need event-level start/end times consistent with the new threshold
should recompute them (e.g., by taking the min/max of `eventStart`/
`eventEnd` per new `eventID`) as a separate step.

## Author

Zachary Amir & Claude Opus 5

## Examples

``` r
obs <- data.frame(
  deploymentID = "Cam01",
  scientificName = c("Canis lupus", "Canis lupus", "Canis lupus", "Canis lupus"),
  eventStart = as.POSIXct(c("2023-01-01 08:00:00", "2023-01-01 08:20:00",
                            "2023-01-01 09:10:00", "2023-01-02 08:00:00"),
                          tz = "UTC"),
  eventEnd = as.POSIXct(c("2023-01-01 08:00:10", "2023-01-01 08:20:10",
                          "2023-01-01 09:10:10", "2023-01-02 08:00:10"),
                        tz = "UTC"),
  # seconds since the previous independent (30-min) event boundary
  deltaTime_event = c(0, 1200, 3000, 82800),
  stringsAsFactors = FALSE
)

# Reclassify events using a 60-minute threshold
reclassify_eventID(obs, new_event_time = 60)
#> # A tibble: 4 × 6
#>   deploymentID scientificName eventStart          eventEnd           
#>   <chr>        <chr>          <dttm>              <dttm>             
#> 1 Cam01        Canis lupus    2023-01-01 08:00:00 2023-01-01 08:00:10
#> 2 Cam01        Canis lupus    2023-01-01 08:20:00 2023-01-01 08:20:10
#> 3 Cam01        Canis lupus    2023-01-01 09:10:00 2023-01-01 09:10:10
#> 4 Cam01        Canis lupus    2023-01-02 08:00:00 2023-01-02 08:00:10
#> # ℹ 2 more variables: deltaTime_event <dbl>, eventID <chr>
```
