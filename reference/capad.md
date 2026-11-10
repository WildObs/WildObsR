# Collaborative Australian Protected Areas Database (CAPAD) 2022, Terrestrial

The 14,234 terrestrial protected areas in CAPAD 2022, as an `sf` polygon
layer. This is the layer
[`locationName_verification_CAPAD`](https://wildobs.github.io/WildObsR/reference/locationName_verification_CAPAD.md)
and
[`locationName_buffer_CAPAD`](https://wildobs.github.io/WildObsR/reference/locationName_buffer_CAPAD.md)
use to name the protected area around each camera.

## Usage

``` r
capad
```

## Format

An `sf` data frame with 14,234 rows (one POLYGON or MULTIPOLYGON per
protected area) and 3 attribute columns, in WGS84 longitude/latitude
(EPSG:4326):

- NAME:

  Protected area name.

- TYPE_ABBR:

  Abbreviated protected area type, e.g. `NP` (national park), `NR`
  (nature reserve), `IPA` (Indigenous Protected Area).

- IUCN:

  IUCN protected area management category: `Ia`, `Ib`, `II`, `III`,
  `IV`, `V`, `VI`, `NR` (not reported), `NAS` (not assigned), or `NA`
  (not applicable).

## Source

Australian Government Department of Climate Change, Energy, the
Environment and Water (DCCEEW). *Collaborative Australian Protected
Areas Database (CAPAD) 2022 - Terrestrial*. Commonwealth of Australia.
Licensed under CC BY 4.0.
<https://www.dcceew.gov.au/environment/land/nrs/science/capad>

## Details

Derived from the CAPAD 2022 terrestrial shapefile (web Mercator,
EPSG:3857) by keeping only the columns above, repairing invalid
polygons, simplifying boundaries with a 5 m tolerance, and reprojecting
to EPSG:4326. Simplification was checked against the full resolution
layer for all 19,736 WildObs deployments: 4 changed between "inside a
protected area" and "outside", and none switched from one protected area
to another. The build is recorded in `dev/add_data_to_package.R`.

## See also

[`locationName_verification_CAPAD`](https://wildobs.github.io/WildObsR/reference/locationName_verification_CAPAD.md),
[`locationName_buffer_CAPAD`](https://wildobs.github.io/WildObsR/reference/locationName_buffer_CAPAD.md),
[`ibra`](https://wildobs.github.io/WildObsR/reference/ibra.md)

## Examples

``` r
# The attribute table, without the polygons
head(sf::st_drop_geometry(capad))
#>                   NAME TYPE_ABBR IUCN
#> 1          Callum Brae        NR   IV
#> 2     Greater Dordagup        NP   II
#> 3          Mount Royal        NP   II
#> 4      Unnamed WA28710        NR   Ia
#> 5 Watta Wella H21 B.R.       NFR   IV
#> 6     Malabar Headland        NP   II

# How many protected areas of each IUCN category
table(capad$IUCN)
#> 
#>   II  III   IV   Ia   Ib  NAS   NR    V   VI 
#> 1826 2490 4142 2794   93   23    2 1015 1839 
```
