# List Available Models

Returns a list of all models available in the Synthesize Bio API. Each
model has a unique ID that can be used with predict_query() and
get_example_query().

## Usage

``` r
list_models(api_base_url = API_BASE_URL)
```

## Arguments

- api_base_url:

  The base URL for the API server. Default is API_BASE_URL.

## Value

A list or data frame containing available models with their IDs and
metadata.

## Examples

``` r
if (FALSE) { # \dontrun{
# Get all available models
models <- list_models()
print(models)
} # }
```
