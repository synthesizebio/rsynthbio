# <img src="https://assets.synthesize.bio/logomark.png" style="width: 30px; height: 30px;" alt="Logomark"> rsynthbio

`rsynthbio` is an R package that provides a convenient interface to the [Synthesize Bio](https://www.synthesize.bio/) API, allowing users to generate realistic gene expression data based on specified biological conditions. This package enables researchers to easily access AI-generated transcriptomic data for various modalities, including bulk RNA-seq and single-cell RNA-seq.

To generate datasets without code, use our [web platform](https://app.synthesize.bio/datasets/).

[See the full documentation here](https://synthesizebio.github.io/rsynthbio/).

For questions, suggestions, and support, email us at [support@synthesize.bio](mailto:support@synthesize.bio).

## How to Install

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

## Quick Start

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

## Available Models

| Model Type | Bulk | Single-Cell | Description |
|------------|------|-------------|-------------|
| **Baseline** | `gem-1-bulk` | `gem-1-sc` | Generate expression from metadata |
| **Reference Conditioning** | `gem-1-bulk_reference-conditioning` | `gem-1-sc_reference-conditioning` | Generate expression anchored to a reference sample |
| **Metadata Prediction** | `gem-1-bulk_predict-metadata` | `gem-1-sc_predict-metadata` | Predict metadata from expression |

Only baseline models are available to all users. You can check which models are available programmatically, use `list_models()`. Contact us at support@synthesize.bio if you have any questions.

## Documentation

For detailed usage instructions, see the vignettes:

- [Getting Started](https://synthesizebio.github.io/rsynthbio/articles/getting-started.html) — Installation, authentication, and overview
- [Baseline Models](https://synthesizebio.github.io/rsynthbio/articles/baseline.html) — Generate expression from metadata
- [Reference Conditioning](https://synthesizebio.github.io/rsynthbio/articles/reference-conditioning.html) — Condition on real expression data
- [Metadata Prediction](https://synthesizebio.github.io/rsynthbio/articles/metadata-prediction.html) — Infer metadata from expression

## Mintlify source

This repo now also includes a Mintlify-compatible docs source under `docs-mintlify/` so the shared docs site can aggregate the R SDK docs directly from `rsynthbio`.

- `vignettes/*.Rmd` and `_pkgdown.yml` remain the primary authored docs inputs
- `man/*.Rd` remains the source for generated function-level reference docs
- `scripts/generate_mintlify_docs.py` regenerates the committed Mintlify output tree

Regenerate the Mintlify docs after changing vignettes, package exports, or roxygen docs:

```bash
python3 scripts/generate_mintlify_docs.py
```

The generated pages are committed in `docs-mintlify/` so changes are reviewable in PRs. `pkgdown` stays in place during the transition, and GitHub Pages remains the current public docs host until the shared docs site is ready to switch over.

## Rate Limits

Free usage of Synthesize Bio is limited. If you exceed this limit, you will receive an error message. To generate more samples, please contact us at [support@synthesize.bio](mailto:support@synthesize.bio).
