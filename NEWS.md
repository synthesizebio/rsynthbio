# rsynthbio 4.2.3

- Format hosted GEM-2 baseline, reference-based expression, metadata prediction
  and reconstruction results through the standard prediction client.
- Preserve nested metadata, classifier labels/probabilities and full numeric
  precision when reading prediction results.
- Keep requested input/output column order and missing values aligned.

# rsynthbio 4.2.2

- Updated the maintainer to Alex David <alex@synthesize.bio>, retaining
  Synthesize Bio as an author.

# rsynthbio 4.2.1

- Hosted prediction HTTP 400/422 errors now expose `status_code`, `response_body` and
  `response_json` on a catchable `synthesize_validation_error` condition.
  Structured field corrections display one path and message per line; legacy
  response text and nonvalidation HTTP errors retain their existing behavior.

# rsynthbio 4.2.0

## New Features

- Added self-hosted model support: `predict_query()` and `get_example_query()`
  gain a `self_hosted` argument (also resolved from the `SYNTHESIZE_SELF_HOSTED`
  environment variable). When enabled, the client sends a single synchronous
  request to a partner-hosted model container and decodes the Apache Arrow IPC
  stream response into the same data frames as the production path (no polling,
  no download URL). Requires the optional `arrow` package; no API key is
  required (one is only sent when `SYNTHESIZE_API_KEY` is set).
- Added per-model base-URL resolution: set `SYNTHESIZE_API_BASE_URL__<MODEL>`
  (e.g. `SYNTHESIZE_API_BASE_URL__GEM_1_BULK`) to point each model at its own
  self-hosted container once and omit `api_base_url` on every call. Resolution
  precedence is: explicit `api_base_url` -> per-model variable -> global
  `SYNTHESIZE_API_BASE_URL` -> production default.
- Self-hosted predictions use a longer 600s request timeout to accommodate
  large sample counts on partner GPU hosts.

# rsynthbio 4.1.0

## Breaking Changes

- `predict_query()` now raises an error when called with a `model_id` that has no registered output transformer. Pass `raw_response = TRUE` to get the unformatted JSON response.
- Metadata prediction models (`gem-1-bulk_predict-metadata`, `gem-1-sc_predict-metadata`) now return a named list with `metadata`, `latents`, `classifier_probs`, and `expression` data.frames instead of the raw outputs list.

## New Features

- Added output transformer for `gem-1-bulk_predict-metadata` and `gem-1-sc_predict-metadata` that converts results to data.frames.
- Added output transformer for `gem-1-bulk_condition-on-sample-ids`.
