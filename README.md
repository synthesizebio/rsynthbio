# <img src="https://assets.synthesize.bio/logomark.png" style="width: 30px; height: 30px;" alt="Logomark"> rsynthbio

`rsynthbio` is an R package that provides a convenient interface to the [Synthesize Bio](https://www.synthesize.bio/) API, allowing users to generate realistic gene expression data based on specified biological conditions. This package enables researchers to easily access AI-generated transcriptomic data for various modalities, including bulk RNA-seq and single-cell RNA-seq.

To generate datasets without code, use our [web platform](https://app.synthesize.bio/datasets/).

[Get started](https://docs.synthesize.bio/r-sdk/getting-started) | [Full R SDK docs](https://docs.synthesize.bio/r-sdk)

User-facing documentation lives in [`synthesizebio/mintlify-docs`](https://github.com/synthesizebio/mintlify-docs) under [`r-sdk/`](https://github.com/synthesizebio/mintlify-docs/tree/main/r-sdk). To update the docs, open a PR there.

For function-level reference, use R's built-in help (`?predict_query`, `help(package = "rsynthbio")`) or the [CRAN reference manual PDF](https://cran.r-project.org/web/packages/rsynthbio/rsynthbio.pdf).

For questions, suggestions, and support, email us at [support@synthesize.bio](mailto:support@synthesize.bio).

## How to install

You can install `rsynthbio` from CRAN:

```r
install.packages("rsynthbio")
```

To install the development version from GitHub:

```r
remotes::install_github("synthesizebio/rsynthbio")
```

## Authentication

Set up your API token (obtain one at [app.synthesize.bio](https://app.synthesize.bio)):

```r
library(rsynthbio)

# Securely prompt for your token (recommended)
set_synthesize_token()

# Or store in system keyring for persistence across sessions
set_synthesize_token(use_keyring = TRUE)

# Load from keyring in future sessions
load_synthesize_token_from_keyring()
```

## Quick start

```r
library(rsynthbio)

# See available models
list_models()

# Get an example query for bulk RNA-seq
query <- get_example_query(model_id = "gem-1-bulk")$example_query

# Generate synthetic expression data
result <- predict_query(query, model_id = "gem-1-bulk")

# Access the results
metadata <- result$metadata
expression <- result$expression
```

## Available models

| Model type                 | Bulk                                  | Single-cell                          | Description                                       |
| -------------------------- | ------------------------------------- | ------------------------------------ | ------------------------------------------------- |
| **Baseline**               | `gem-1-bulk`                          | `gem-1-sc`                           | Generate expression from metadata                 |
| **Reference conditioning** | `gem-1-bulk_reference-conditioning`   | `gem-1-sc_reference-conditioning`    | Generate expression anchored to a reference sample|
| **Metadata prediction**    | `gem-1-bulk_predict-metadata`         | `gem-1-sc_predict-metadata`          | Predict metadata from expression                  |

Only baseline models are available to all users. Check programmatically with `list_models()`. Contact us at [support@synthesize.bio](mailto:support@synthesize.bio) if you have any questions.

## Documentation

For detailed usage and guides, see the [R SDK section of the Synthesize Bio docs](https://docs.synthesize.bio/r-sdk):

- [Getting started](https://docs.synthesize.bio/r-sdk/getting-started) — Installation, authentication, and overview
- [Baseline models](https://docs.synthesize.bio/r-sdk/models/baseline) — Generate expression from metadata
- [Reference conditioning](https://docs.synthesize.bio/r-sdk/models/reference-conditioning) — Condition on real expression data
- [Metadata prediction](https://docs.synthesize.bio/r-sdk/models/metadata-prediction) — Infer metadata from expression
- [Function reference](https://docs.synthesize.bio/r-sdk/reference) — All exported functions

## Rate limits

Free usage of Synthesize Bio is limited. If you exceed this limit, you'll receive an error message. To generate more samples, please contact us at [support@synthesize.bio](mailto:support@synthesize.bio).
