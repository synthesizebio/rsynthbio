# Getting Started

`rsynthbio` is an R package that provides a convenient interface to the
[Synthesize Bio](https://www.synthesize.bio/) API, allowing users to
generate realistic gene expression data based on specified biological
conditions. This package enables researchers to easily access
AI-generated transcriptomic data for various modalities including bulk
RNA-seq and single-cell RNA-seq.

Alternatively, you can AI generate datasets from our [web
platform](https://app.synthesize.bio/datasets/).

## How to install

You can install `rsynthbio` from CRAN:

``` r
install.packages("rsynthbio")
```

If you want the development version, you can install using the `remotes`
package to install from GitHub:

``` r
if (!("remotes" %in% installed.packages())) {
  install.packages("remotes")
}
remotes::install_github("synthesizebio/rsynthbio")
```

Once installed, load the package:

``` r
library(rsynthbio)
```

## Authentication

Before using the Synthesize Bio API, you need to set up your API token.
The package provides a secure way to handle authentication:

``` r
# Securely prompt for and store your API token
# The token will not be visible in the console
set_synthesize_token()

# You can also store the token in your system keyring for persistence
# across R sessions (requires the 'keyring' package)
set_synthesize_token(use_keyring = TRUE)
```

Loading your API key for a session.

``` r
# In future sessions, load the stored token
load_synthesize_token_from_keyring()

# Check if a token is already set
has_synthesize_token()
```

You can manually set the token, but don’t commit it to version control!

``` r
set_synthesize_token(token = "your-token-here")
```

You can obtain an API token by registering at [Synthesize
Bio](https://app.synthesize.bio).

## Available Model Types

Synthesize Bio provides several types of models for different use cases:

### Baseline Models

Generate synthetic gene expression data from metadata alone. You
describe the biological conditions (tissue type, disease state,
perturbations, etc.) and the model generates realistic expression
profiles.

- **`gem-1-bulk`**: Bulk RNA-seq baseline model
- **`gem-1-sc`**: Single-cell RNA-seq baseline model

See the [Baseline
Models](https://synthesizebio.github.io/rsynthbio/articles/baseline.md)
vignette for detailed usage.

### Reference Conditioning Models

Generate expression data conditioned on a real reference sample. This
allows you to “anchor” to an existing expression profile while applying
perturbations or modifications.

- **`gem-1-bulk_reference-conditioning`**: Bulk RNA-seq reference
  conditioning model
- **`gem-1-sc_reference-conditioning`**: Single-cell RNA-seq reference
  conditioning model

See the [Reference
Conditioning](https://synthesizebio.github.io/rsynthbio/articles/reference-conditioning.md)
vignette for detailed usage.

### Metadata Prediction Models

Infer metadata from observed expression data. Given a gene expression
profile, predict the likely biological characteristics (cell type,
tissue, disease state, etc.).

- **`gem-1-bulk_predict-metadata`**: Bulk RNA-seq metadata prediction
  model
- **`gem-1-sc_predict-metadata`**: Single-cell RNA-seq metadata
  prediction model

See the [Metadata
Prediction](https://synthesizebio.github.io/rsynthbio/articles/metadata-prediction.md)
vignette for detailed usage.

Only baseline models are available to all users. You can check which
models are available programmatically, use
[`list_models()`](https://synthesizebio.github.io/rsynthbio/reference/list_models.md).
Contact us at <support@synthesize.bio> if you have any questions.

### Listing Available Models

You can check which models are available programmatically:

``` r
# Check available models
list_models()
```

## Quick Start

Here’s a quick example using a baseline model:

``` r
# Get an example query structure
query <- get_example_query(model_id = "gem-1-bulk")

# Submit the query and get results
result <- predict_query(query, model_id = "gem-1-bulk")

# Access the results
metadata <- result$metadata
expression <- result$expression
```

For more detailed examples and advanced usage, see the model-specific
vignettes linked above.

## Session info

``` r
sessionInfo()
```

## Additional Resources

- [Package Source Code](https://github.com/synthesizebio/rsynthbio)
- [File Bug Reports](https://github.com/synthesizebio/rsynthbio/issues)
