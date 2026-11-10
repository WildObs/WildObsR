# Extract classification information from taxize

This function facilitates extracting taxonomic classification
infromation from taxize objects. This function helps create the WildObs
verified taxonomy database by wrangling the taxize objects to match our
standard.

## Usage

``` r
extract_classif(
  uids = NULL,
  classif = NULL,
  request = c("species", "genus", "family", "order", "class")
)
```

## Arguments

- uids:

  This is a taxize object created by running the function
  taxize::get_ids() to obtain verified species identifications

- classif:

  This is a taxize object by running the function
  taxize::classification() to obtain verified species classifications

- request:

  The specific classification information requested by the function.
  Defaults to species, genus, family, order, class.

## Value

Creates a dataframe that matches the format of the WildObs verified
taxonomy database.

## Author

Nathan Travell (I think?)
