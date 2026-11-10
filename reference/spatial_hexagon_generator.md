# Generate spatially distinct sampling units

This function generates spatially distinct hexagonal sampling units to
group nearby camera trap deployments. It ensures that sampling units are
spatially and temporally independent across multiple contributors and
surveys.

## Usage

``` r
spatial_hexagon_generator(data, scales)
```

## Arguments

- data:

  A dataframe of covariates that includes, at minimum, the following
  columns: `deploymentID`, `locationName`, `deploymentGroups`,
  `projectName`, `latitude`, and `longitude`. These fields are part of
  WildObs camtrapDP-formatted Frictionless Data Packages.

- scales:

  A numeric vector specifying the **areas of hexagonal grid cells in
  square meters (m²)**. Values smaller than 1e6 (1 km²) are allowed. The
  function automatically labels each scale as either m² or km² for
  readability.

## Value

A dataframe containing the original input data plus:

- One column per scale named `polygon_<scale>` (spatial hexagon ID).

- One column per scale named `cellID_<scale>` (spatial + temporal
  hexagon ID).

## Details

The function overlays one or more hexagonal grids at user-specified
*spatial scales* and assigns each camera deployment (`deploymentID`) to
its corresponding hexagonal cell. Each scale value defines the **area**
of a single hexagon (not its edge length), expressed in **square meters
(m²)**. Internally, these area values are converted into the
corresponding apothem distance (distance from the hexagon's center to
the midpoint of a side) using the geometric relationship:

\$\$A = \frac{3 \sqrt{3}}{2} s^2\$\$ \$\$a = \frac{\sqrt{3}}{2} s\$\$

where *A* is the hexagon area, *s* is the side length, and *a* is the
apothem (used to set the grid spacing). This means:

- A value of **1000000** (1e6) corresponds to hexagons with an area of
  **1 km²**.

- A value of **25000000** (25e6) corresponds to hexagons with an area of
  **25 km²**.

- Smaller values (e.g., 100,000 = 0.1 km²) produce finer grids.

Each deployment is assigned two identifiers per scale:

- `polygon_<scale>`:

  Spatially distinct hexagon cell ID (independent of time).

- `cellID_<scale>`:

  Spatially and temporally distinct hexagon cell ID, incorporating
  survey year and data source

If several projects sample the same hexagon in overlapping time windows,
each project keeps its own `cellID`, so their detections and effort stay
separate, and one warning lists the overlapping cells at each scale by
their `polygon_<scale>` value, with the projects involved. Filter the
result on those polygons to inspect them. To model the projects as one
site, give their deployments a shared cellID before building the effort
matrix.

## Author

Zachary Amir

## Examples

``` r
# Create example camera-trap metadata
# \donttest{
set.seed(42)

# Simulate 12 camera deployments across 3 sites and 2 contributors
data <- tibble::tibble(
  deploymentID = paste0("cam_", 1:12),
  locationName = rep(c("Site_A", "Site_B", "Site_C"), each = 4),
  projectName = rep(c("UQ_team", "CSIRO_team"), length.out = 12),

  # Cluster each site around distinct centers (approx Wet Tropics, QLD)
  latitude = c(
    rnorm(4, mean = -17.10, sd = 0.01),   # Site_A: tight cluster
    rnorm(4, mean = -17.25, sd = 0.005),  # Site_B: tighter cluster
    rnorm(4, mean = -16.95, sd = 0.03)    # Site_C: more spread out
  ),
  longitude = c(
    rnorm(4, mean = 145.65, sd = 0.01),   # Site_A
    rnorm(4, mean = 145.50, sd = 0.005),  # Site_B
    rnorm(4, mean = 145.75, sd = 0.03)    # Site_C
  ),

  # Random deployment dates over two months
  deploymentStart = as.POSIXct("2023-07-01") + runif(12, 0, 30) * 86400,
  deploymentEnd   = as.POSIXct("2023-09-01") + runif(12, 0, 30) * 86400
)

# Simulate a column that identifies unique survey events
data$deploymentGroups <- paste0(data$locationName, "_", format(data$deploymentStart, "%Y"))

# --- Define grid scales ---
# The values represent hexagon *areas* in square meters.
# 1e6 = 1 km² hexagons; 25e6 = 25 km² hexagons.
scales <- c(1e6, 25e6)

# --- Generate hexagonal sampling units ---
hex_data <- spatial_hexagon_generator(data, scales)
#> Warning: spatial_hexagon_generator() found cells that several projects sampled at overlapping times:
#>   polygon_25km2: 2 cell(s), e.g. Site_A_2_cellID_25km2, Site_B_2_cellID_25km2; projects CSIRO_team, UQ_team
#> Each project keeps its own cellID, so their detections and effort stay separate. To inspect them, filter the result on those polygon values; to model them as one site, give the deployments a shared cellID before building the effort matrix.

# Inspect new columns (polygon_ and cellID_ per scale)
dplyr::select(hex_data, deploymentID, starts_with("polygon"), starts_with("cellID")) %>%
  head()
#>   deploymentID          polygon_1km2         polygon_25km2
#> 1        cam_1  Site_A_4_cellID_1km2 Site_A_2_cellID_25km2
#> 2        cam_2  Site_A_7_cellID_1km2 Site_A_2_cellID_25km2
#> 3        cam_3  Site_A_9_cellID_1km2 Site_A_2_cellID_25km2
#> 4        cam_4 Site_A_12_cellID_1km2 Site_A_2_cellID_25km2
#> 5        cam_5  Site_B_4_cellID_1km2 Site_B_2_cellID_25km2
#> 6        cam_6  Site_B_2_cellID_1km2 Site_B_2_cellID_25km2
#>                             cellID_1km2                          cellID_25km2
#> 1     Site_A_4_cellID_1km2_2023_UQ_team    Site_A_2_cellID_25km2_2023_UQ_team
#> 2  Site_A_7_cellID_1km2_2023_CSIRO_team Site_A_2_cellID_25km2_2023_CSIRO_team
#> 3     Site_A_9_cellID_1km2_2023_UQ_team    Site_A_2_cellID_25km2_2023_UQ_team
#> 4 Site_A_12_cellID_1km2_2023_CSIRO_team Site_A_2_cellID_25km2_2023_CSIRO_team
#> 5     Site_B_4_cellID_1km2_2023_UQ_team    Site_B_2_cellID_25km2_2023_UQ_team
#> 6  Site_B_2_cellID_1km2_2023_CSIRO_team Site_B_2_cellID_25km2_2023_CSIRO_team
# }
```
