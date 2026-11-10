# Count the Records Matching a Filter in a WildObs API Collection

Asks the WildObs API aggregation endpoint how many documents in one
collection match a filter, without downloading them. Used to report
download progress and to confirm a paged download fetched every row.

## Usage

``` r
.wildobs_api_count(url, api_key, collection, filter)
```

## Arguments

- url:

  Character. The API find endpoint; the aggregation endpoint is derived
  from it.

- api_key:

  Character. The user's WildObs API key.

- collection:

  Character. The collection to count, e.g. `"observations"`.

- filter:

  List. The same filter the download uses.

## Value

A single integer, or `NA` if the count could not be obtained. Never
stops, because a failed count should not prevent the download itself.

## Author

Zachary Amir
