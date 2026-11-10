# Recombine 100-Day Deployments Into Continuous Long-Term Deployments

The WildObs data curation pipeline splits long camera deployments into
pieces of at most 100 days, so each piece fits inside a single-season
survey (`deploymentGroups`). Long-term analyses (multi-season occupancy
models, Living Planet Index style trends, the Threatened Species Index)
want the opposite: one deployment per camera for as long as it ran
without a break. This function stitches the pieces back together using
the `multiSeason_deploymentGroup` column, which already groups
consecutive surveys at a `locationName` that are separated by 30 days or
less.

A data package goes in and a data package comes out, so every resource
that carries `deploymentID` (deployments, covariates, observations and,
if present, media) is updated together and stays joinable, and the
package's `temporal` metadata describes the same survey groups as its
data.

## Usage

``` r
recombine_deployment_groups(dp, max_gap_days = 30)
```

## Arguments

- dp:

  A WildObs camtrap data package, as returned by
  [`wildobs_dp_download`](https://wildobs.github.io/WildObsR/reference/wildobs_dp_download.md),
  holding at least the `deployments` resource. The deployments must
  include `deploymentID`, `locationID`, `deploymentStart`,
  `deploymentEnd`, `deploymentGroups` and `multiSeason_deploymentGroup`.

- max_gap_days:

  Numeric. The longest gap, in days, allowed between one piece ending
  and the next starting at the same camera before the run is split into
  a new `_partN` deployment. Defaults to 30, matching the gap that
  separates multi-season groups.

## Value

The same data package with:

- one `deployments` row per continuous camera run, with
  `deploymentGroups` replaced by its `multiSeason_deploymentGroup`;

- `covariates` collapsed to match the new deployments;

- `deploymentID` remapped in `observations` and `media`, except obscured
  threatened-species rows (`obscured_for_<category>_species`), which
  have no camera and are kept unchanged with a message;

- `temporal` metadata holding one block per
  `multiSeason_deploymentGroup`;

- a new `deployment_crosswalk` resource with one row per original piece:
  `original_deploymentID`, `deploymentID` (the new one),
  `original_deploymentGroups`, `pieceStart` and `pieceEnd`;

- `dp$data`, the in-memory copy of the tables that
  [`wildobs_dp_download`](https://wildobs.github.io/WildObsR/reference/wildobs_dp_download.md)
  adds, refreshed to match the resources.

Stops if `max_gap_days` is not a single non-negative number, if a
required resource or column is missing, if two pieces of the same camera
overlap in time, or if observations or media hold a `deploymentID`
(other than an obscured placeholder) that is not in the deployments.

## Details

100-day deployments (i.e., pieces) are chained in three steps:

1.  Pieces are grouped by `locationID`, `multiSeason_deploymentGroup`
    and the `_CamN` suffix that the camDB pipeline appends to
    `deploymentID` when one station holds several cameras in the same
    survey (e.g. a lured and an unlured camera).

2.  Within a group, pieces are ordered by `deploymentStart`. A piece
    starts a new chain when it begins more than `max_gap_days` after the
    previous piece ended, because the 30-day rule behind
    `multiSeason_deploymentGroup` applies to the whole `locationName`,
    so a single station can sit idle for much longer inside one group. A
    piece that starts before the previous one ended cannot be the same
    camera, so the function stops and names it; the cameras need
    relabelling in curation before the package can be recombined.

3.  Each chain becomes one deployment named
    `<locationID>_ms<N>[_CamN][_partN]`, where `N` orders the
    multi-season groups at that station in time.

Columns are merged across the pieces of a chain as follows. A value
shared by every piece is kept as is, including `NA`. Otherwise:

- `deploymentStart` takes the earliest piece start and `deploymentEnd`
  the latest piece end;

- numeric columns take a mean weighted by each piece's duration, rounded
  for integer columns;

- logical columns are `TRUE` if any piece is `TRUE`;

- factor columns (controlled vocabularies such as `featureType`) take
  the value that covers the most time, so they stay inside their
  vocabulary;

- other date and date-time columns also take the value that covers the
  most time, keeping their class and time zone;

- `deploymentTags` keeps each `key: value` pair once. Where pieces
  disagree on a key, placeholder values (`unspecified`, `unknown`, `-`,
  `NA` or empty, ignoring case) are dropped if a real value exists, then
  the value that covers the most time is kept, so every key appears
  once;

- other character columns list the distinct values, separated by
  `" | "`.

The new `deploymentStart`/`deploymentEnd` span includes any gaps of up
to `max_gap_days` between pieces. Calculate sampling effort from
`pieceStart` and `pieceEnd` in `deployment_crosswalk`, not from the
recombined span. `observationID` and `eventID` are left unchanged; they
remain unique.

In the `temporal` metadata, the per-`deploymentGroups` blocks are
replaced by one block per `multiSeason_deploymentGroup`. Each new block
starts at the earliest start and ends at the latest end of the
single-season blocks it absorbs. The package-level `start`, `end` and
`timeZone` are left unchanged.

## See also

[`survey_and_deployment_generator`](https://wildobs.github.io/WildObsR/reference/survey_and_deployment_generator.md),
which creates the 100-day deployments.

## Author

Zachary Amir & Claude Opus 5.5

## Examples

``` r
# the Woomargama project shipped with WildObsR ran four surveys over two years
dp <- example_datapackages("NSW_Woomargama_wombats_Linley_2021-22_WildObsID_0010")[[1]]
long_dp <- recombine_deployment_groups(dp)
#> NSW_Woomargama_wombats_Linley_2021-22_WildObsID_0010: 94 obscured observations row(s) kept unchanged in recombine_deployment_groups(); they have no camera to join to.

# fewer, longer deployments
nrow(frictionless::read_resource(dp, "deployments"))
#> [1] 219
nrow(frictionless::read_resource(long_dp, "deployments"))
#> [1] 57

# and every original piece, for calculating effort without the gaps
head(frictionless::read_resource(long_dp, "deployment_crosswalk"))
#> # A tibble: 6 × 5
#>   original_deploymentID  deploymentID original_deploymentG…¹ pieceStart         
#>   <chr>                  <chr>        <chr>                  <dttm>             
#> 1 10_WildObsID_0010_202… 10_WildObsI… Woomargama_NP_2021_a_… 2021-07-04 00:36:04
#> 2 10_WildObsID_0010_202… 10_WildObsI… Woomargama_NP_2021_b_… 2021-10-07 03:20:43
#> 3 10_WildObsID_0010_202… 10_WildObsI… Woomargama_NP_2022_a_… 2022-01-16 02:41:54
#> 4 10_WildObsID_0010_202… 10_WildObsI… Woomargama_NP_2022_b_… 2022-04-23 16:03:39
#> 5 10_WildObsID_0010_202… 10_WildObsI… Woomargama_NP_2021_a_… 2021-06-28 21:36:44
#> 6 10_WildObsID_0010_202… 10_WildObsI… Woomargama_NP_2021_b_… 2021-10-05 17:37:17
#> # ℹ abbreviated name: ¹​original_deploymentGroups
#> # ℹ 1 more variable: pieceEnd <dttm>
```
