# <img src="https://assets.synthesize.bio/logomark.png" style="width: 30px; height: 30px;" alt="Logomark"> rsynthbio

`rsynthbio` is an R package that provides a convenient interface to the [Synthesize Bio](https://www.synthesize.bio/) API, allowing users to generate realistic gene expression data based on specified biological conditions. This package enables researchers to easily access AI-generated transcriptomic data for various modalities, including bulk RNA-seq and single-cell RNA-seq.

To generate datasets without code, use our [web platform](https://app.synthesize.bio/datasets/).

[Get started](https://docs.synthesize.bio/rsynthbio/getting-started) | [Full R SDK docs](https://docs.synthesize.bio/rsynthbio)

For function-level reference, use R's built-in help (`?predict_query`, `help(package = "rsynthbio")`) or the [CRAN reference manual](https://CRAN.R-project.org/package=rsynthbio).

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

## Self-hosted models

Partners who run Synthesize models inside their own environment can point the
same client at a self-hosted container. Self-hosted mode sends a single
synchronous request and decodes the Apache Arrow IPC stream response into the
same data frames as the hosted path (no polling, no download URL). It requires
the optional `arrow` package and does not require an API key.

Self-hosted deployment is a model deployment option available within a
Synthesize Bio partnership. To learn more or request access, contact
[partnerships@synthesize.bio](mailto:partnerships@synthesize.bio).

```r
install.packages("arrow")

# Point each model at its own container once (per-model environment variables)
Sys.setenv(
  SYNTHESIZE_SELF_HOSTED = "1",
  SYNTHESIZE_API_BASE_URL__GEM_1_BULK = "https://gem-1-bulk.internal.example",
  SYNTHESIZE_API_BASE_URL__GEM_1_SC   = "https://gem-1-sc.internal.example"
)

query <- get_example_query("gem-1-bulk", self_hosted = TRUE)$example_query
result <- predict_query(query, model_id = "gem-1-bulk", self_hosted = TRUE)
result$expression
```

You can also pass `api_base_url` explicitly per call instead of using the
environment variables. Base-URL resolution precedence is: explicit
`api_base_url` -> per-model `SYNTHESIZE_API_BASE_URL__<MODEL>` -> global
`SYNTHESIZE_API_BASE_URL` -> production default. If the container is started
with authentication enabled, set `SYNTHESIZE_API_KEY` and the client will send
it as a bearer token. See the [Self-Hosted Models](https://docs.synthesize.bio/rsynthbio/self-hosted)
vignette for details.

## Documentation

For detailed usage and guides, see the [R SDK section of the Synthesize Bio docs](https://docs.synthesize.bio/rsynthbio):

- [Getting started](https://docs.synthesize.bio/rsynthbio/getting-started) — Installation, authentication, and overview
- [Available metadata](https://docs.synthesize.bio/rsynthbio/available-metadata) — Metadata fields you can query
- [Baseline models](https://docs.synthesize.bio/rsynthbio/models/baseline) — Generate expression from metadata
- [Reference conditioning](https://docs.synthesize.bio/rsynthbio/models/reference-conditioning) — Condition on real expression data
- [Metadata prediction](https://docs.synthesize.bio/rsynthbio/models/metadata-prediction) — Infer metadata from expression
- [Self-hosted models](https://docs.synthesize.bio/rsynthbio/self-hosted) — Run models in your own environment via Arrow streaming
- [Function reference](https://docs.synthesize.bio/rsynthbio/reference) — All exported functions

The legacy pkgdown site at `synthesizebio.github.io/rsynthbio` redirects to the
canonical docs above; see `docs-redirect/` and the `deploy-docs-redirect`
workflow for how those redirects are served.

## Authoring the Mintlify docs

The shared docs site at
[docs.synthesize.bio](https://docs.synthesize.bio) aggregates the R SDK
section directly from this repo into `/rsynthbio/...` — there is no
separate docs repo to update.

Authoring sources:

- Long-form pages: `vignettes/*.Rmd` (also shipped as R vignettes for offline
  `vignette(package = "rsynthbio")` browsing)
- Function reference: `man/*.Rd` (generated from `R/*.R` roxygen comments)
- Generation script: `scripts/generate_mintlify_docs.py`

Regenerate the Mintlify output in `docs-external/` after changing vignettes,
roxygen docs, or the package surface:

```bash
python3 scripts/generate_mintlify_docs.py
```

The generated pages are committed in `docs-external/` so changes are
reviewable in PRs.

## Rate limits

Free usage of Synthesize Bio is limited. If you exceed this limit, you'll receive an error message. To generate more samples, please contact us at [support@synthesize.bio](mailto:support@synthesize.bio).
