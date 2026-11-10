# Species Trait Data

A .csv file containing species trait information used within the WildObs
framework. This dataset provides cross-referenced trait data to
facilitate consistent ecological analyses.

## Usage

``` r
species_traits
```

## Format

A data frame with the following columns:

- binomial_verified:

  Verified scientific name (species level).

- uri:

  Unique Resource Identifier corresponding to the taxonomic record.

- phylum, class, order, family, genus, species:

  Standard taxonomic ranks.

- home_range_km2:

  Estimated home range (km²) for mammals, primarily from
  `HomeRange::GetHomeRangeData()` or, where unavailable,
  `traitdata::pantheria()`.

- home_range_source:

  DOI of the source home range dataset.

- AdultBodyMass_g:

  Mean adult body mass (grams) for mammals and birds, from
  `traitdata::pantheria()` or `traitdata::australian_birds()`.

- bodyMass_source:

  DOI of the source body mass dataset.

- epbc_category:

  EPBC Act threat status category for Australian fauna. One of:
  "conservation_dependent", "critically_endangered", "endangered",
  "extinct", "extinct_in_the_wild", "vulnerable", or "not_listed".

- epbc_location:

  The state level acronym where the EPBC Act threat status category is
  applicable, with several states concatenated together as needed, or NA
  values if not applicable.

## Source

Data were compiled and verified against:

- HomeRange Database
  ([doi:10.6084/m9.figshare.16698184](https://doi.org/10.6084/m9.figshare.16698184)
  )

- Pantheria Database
  ([doi:10.1890/08-1494.1](https://doi.org/10.1890/08-1494.1) )

- Australian Birds Trait Database
  ([doi:10.1038/s41597-022-01372-2](https://doi.org/10.1038/s41597-022-01372-2)
  )

- Environmental Protection and Biodiversity Conservation Act 1999
  (<https://www.legislation.gov.au/Series/C2004A00485>)

Home range data from HomeRange package; body mass data from traitdata
package; EPBC status from Australian Department of Climate Change,
Energy, the Environment and Water.

## Details

A data.frame of species-level traits for verified taxa. Each row
corresponds to a verified species.

Species trait data helps enable comparative and functional ecological
analyses and is used to obscure threatend species location information.

## Examples

``` r
# Load data explicitly
data(species_traits)
species_traits
#> # A tibble: 541 × 14
#>    binomial_verified            uri      phylum class order family genus species
#>    <chr>                        <chr>    <chr>  <chr> <chr> <chr>  <chr> <chr>  
#>  1 Acanthagenys rufogularis     https:/… Chord… Aves  Pass… Melip… Acan… Acanth…
#>  2 Acanthiza apicalis           https:/… Chord… Aves  Pass… Acant… Acan… Acanth…
#>  3 Acanthiza chrysorrhoa        https:/… Chord… Aves  Pass… Acant… Acan… Acanth…
#>  4 Acanthiza lineata            https:/… Chord… Aves  Pass… Acant… Acan… Acanth…
#>  5 Acanthiza nana               https:/… Chord… Aves  Pass… Acant… Acan… Acanth…
#>  6 Acanthiza pusilla            https:/… Chord… Aves  Pass… Acant… Acan… Acanth…
#>  7 Acanthiza reguloides         https:/… Chord… Aves  Pass… Acant… Acan… Acanth…
#>  8 Acanthorhynchus tenuirostris https:/… Chord… Aves  Pass… Melip… Acan… Acanth…
#>  9 Accipiter cirrocephalus      https:/… Chord… Aves  Acci… Accip… Acci… Accipi…
#> 10 Accipiter fasciatus          https:/… Chord… Aves  Acci… Accip… Acci… Accipi…
#> # ℹ 531 more rows
#> # ℹ 6 more variables: home_range_km2 <dbl>, home_range_source <chr>,
#> #   AdultBodyMass_g <dbl>, bodyMass_source <chr>, epbc_category <fct>,
#> #   epbc_location <chr>
```
