# Send One Request to the WildObs API and Return Its Records

Posts one find request to the WildObs API and returns the records it
holds. A request that fails, returns a non-200 status, or returns a body
without records is retried, pausing 2 then 4 seconds, because one
dropped request among the hundreds a large media download makes should
not discard the whole download. A rejected API key (401 or 403) is not
retried, since trying again cannot help.

## Usage

``` r
.wildobs_api_find(url, api_key, body, what, attempts = 3)
```

## Arguments

- url:

  Character. The API find endpoint.

- api_key:

  Character. The user's WildObs API key.

- body:

  List. The request body: collection, filter and any sort or limit.

- what:

  Character. A short description of the request, used in the error.

- attempts:

  Integer. How many times to try before giving up.

## Value

The records returned by the API, usually a data frame, with MongoDB date
fields as POSIXct in UTC. Stops with a message naming the request, the
HTTP status and the server's reason if every attempt fails.
