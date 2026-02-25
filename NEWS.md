# rsynthbio 4.1.0

## Breaking Changes

- `predict_query()` now raises an error when called with a `model_id` that has no registered output transformer. Pass `raw_response = TRUE` to get the unformatted JSON response.
- Metadata prediction models (`gem-1-bulk_predict-metadata`, `gem-1-sc_predict-metadata`) now return a named list with `metadata`, `latents`, `classifier_probs`, and `expression` data.frames instead of the raw outputs list.

## New Features

- Added output transformer for `gem-1-bulk_predict-metadata` and `gem-1-sc_predict-metadata` that converts results to data.frames.
- Added output transformer for `gem-1-bulk_condition-on-sample-ids`.
