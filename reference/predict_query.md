# Predict Gene Expression

Sends a query to the Synthesize Bio API for prediction and retrieves
gene expression samples. This function sends the query to the API and
processes the response into usable data frames.

## Usage

``` r
predict_query(
  query,
  model_id,
  api_base_url = API_BASE_URL,
  poll_interval_seconds = DEFAULT_POLL_INTERVAL_SECONDS,
  poll_timeout_seconds = DEFAULT_POLL_TIMEOUT_SECONDS,
  return_download_url = FALSE
)
```

## Arguments

- query:

  A list representing the query data to send to the API. Use
  \`get_example_query()\` to generate an example. The query supports
  additional optional fields:

  - \`total_count\` (integer): Library size used when converting
    predicted log CPM back to raw counts. Higher values scale counts up
    proportionally.

  - \`deterministic_latents\` (logical): If TRUE, the model uses the
    mean of each latent distribution instead of sampling, producing
    deterministic outputs for the same inputs. Useful for
    reproducibility.

  - \`seed\` (integer): Random seed for reproducibility.

- model_id:

  Character string specifying the model ID (e.g., "gem-1-bulk",
  "gem-1-sc"). Use \`list_models()\` to see available models.

- api_base_url:

  The base URL for the API server. Default is API_BASE_URL.

- poll_interval_seconds:

  Seconds between polling attempts of the status endpoint. Default is
  DEFAULT_POLL_INTERVAL_SECONDS (2).

- poll_timeout_seconds:

  Maximum total seconds to wait before timing out. Default is
  DEFAULT_POLL_TIMEOUT_SECONDS (900 = 15 minutes).

- return_download_url:

  Logical, if TRUE, returns a list containing the signed download URL
  instead of parsing into data frames. Default is FALSE.

## Value

A list. If \`return_download_url\` is \`FALSE\` (default), the list
contains two data frames: \`metadata\` and \`expression\`. If \`TRUE\`,
the list contains \`download_url\` and empty \`metadata\` and
\`expression\` data frames.

## Examples

``` r
# Set your API key (in practice, use a more secure method)
if (FALSE) { # \dontrun{

# To start using rsynthbio, first you need to have an account with synthesize.bio.
# Go here to create one: https://app.synthesize.bio/

set_synthesize_token()

# Get available models
models <- list_models()

# Create a query for a specific model
query <- get_example_query(model_id = "gem-1-bulk")

# Request raw counts
result <- predict_query(query, model_id = "gem-1-bulk")

# Access the results
metadata <- result$metadata
expression <- result$expression

# Explore the top expressed genes in the first sample
head(sort(expression[1, ], decreasing = TRUE))

# Use deterministic latents for reproducible results
query$deterministic_latents <- TRUE
result_det <- predict_query(query, model_id = "gem-1-bulk")

# Specify a custom total count (library size)
query$total_count <- 5000000
result_custom <- predict_query(query, model_id = "gem-1-bulk")
} # }
```
