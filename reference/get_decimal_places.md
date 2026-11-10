# Get Decimal Places from Numeric Values

Counts the significant decimal places in a numeric value, ignoring
trailing zeros. Used to derive coordinate precision from supplied
latitude and longitude values, which in turn feeds
`coordinateUncertainty`.

## Usage

``` r
get_decimal_places(x)
```

## Arguments

- x:

  A numeric vector.

## Value

An integer vector giving the number of significant decimal places for
each element of `x`. `NA` elements return `NA`.

## Details

The value is converted with
[`as.character()`](https://rdrr.io/r/base/character.html), everything up
to and including the decimal point is dropped, trailing zeros are
stripped, and the remaining characters are counted. Three consequences
are worth knowing:

1.  Whole numbers return `0`, because there are no decimals to count.

2.  The sign is ignored, since the minus sign is removed along with the
    integer part.

3.  Values that
    [`as.character()`](https://rdrr.io/r/base/character.html) renders in
    scientific notation return `0`. `1e-05` becomes the string
    `"1e-05"`, which contains no decimal point, so the result is `0`
    rather than `5`. Convert such values with
    [`format()`](https://rdrr.io/r/base/format.html) first if you need
    the true precision.

`NA` input returns `NA`. The function is vectorised over `x`.

## Author

Zachary Amir

## Examples

``` r
# Typical decimals
get_decimal_places(1.25)
#> [1] 2

# Whole numbers have no decimal places
get_decimal_places(100)
#> [1] 0

# The sign is ignored
get_decimal_places(-1.5)
#> [1] 1

# Trailing zeros do not count
get_decimal_places(1.10)
#> [1] 1

# Vectorised over the input
get_decimal_places(c(1.5, 2.25, 3))
#> [1] 1 2 0
```
