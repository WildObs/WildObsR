# Fetch Every Record Matching a Filter, One Page at a Time

Downloads a WildObs API collection in pages ordered by a key column,
asking each time for the rows whose key sorts after the last one
received (keyset paging). One unpaged request for many projects is too
large for the API gateway to answer before it times out, and the API
ignores `skip`, so paging on a key is the only way to keep every request
small.

Rows whose key matches `shared_key_pattern` are fetched separately, in
one request, and left out of the paging. This is for obscured
threatened-species observations, which all share a placeholder
`observationID` such as `obscured_for_endangered_species`: paging on a
key that repeats would skip the repeats that fall after a page boundary.

Once every page is in, the number of rows is compared with the server's
own count of the filter, and a shortfall stops with an error rather than
returning part of the table.

## Usage

``` r
.wildobs_api_find_paged(
  url,
  api_key,
  collection,
  filter,
  key,
  page_size,
  shared_key_pattern = NULL
)
```

## Arguments

- url:

  Character. The API find endpoint.

- api_key:

  Character. The user's WildObs API key.

- collection:

  Character. The collection to fetch, e.g. `"observations"`.

- filter:

  List. The project filter for this download.

- key:

  Character. The column to page on; it must be unique apart from rows
  matched by `shared_key_pattern`.

- page_size:

  Integer. Rows per request.

- shared_key_pattern:

  Character regular expression, or `NULL`. Keys matching it are fetched
  separately and excluded from paging.

## Value

A data frame of every matching record, or an empty list if there are
none. Stops if the rows fetched do not match the server's count.

## Author

Zachary Amir
