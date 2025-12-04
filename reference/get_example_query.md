# Get Example Query for Model

Retrieves an example query structure for a specific model. This provides
a template that can be modified for your specific needs.

## Usage

``` r
get_example_query(model_id, api_base_url = API_BASE_URL)
```

## Arguments

- model_id:

  Character string specifying the model ID (e.g., "gem-1-bulk",
  "gem-1-sc").

- api_base_url:

  The base URL for the API server. Default is API_BASE_URL.

## Value

A list representing a valid query structure for the specified model.

## Examples

``` r
if (FALSE) { # \dontrun{
# Get example query for bulk RNA-seq model
query <- get_example_query(model_id = "gem-1-bulk")

# Get example query for single-cell model
query_sc <- get_example_query(model_id = "gem-1-sc")

# Modify the query structure
query$inputs[[1]]$num_samples <- 10
} # }
```
