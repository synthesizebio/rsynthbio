# Poll Model Query

Internal function to poll the status endpoint until ready/failed or
timeout

## Usage

``` r
poll_model_query(api_base_url, model_query_id, poll_interval, timeout_seconds)
```

## Arguments

- api_base_url:

  The base URL for the API

- model_query_id:

  The model query ID to poll

- poll_interval:

  Seconds between polling attempts

- timeout_seconds:

  Maximum total seconds to wait

## Value

A list with status and payload
