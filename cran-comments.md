## R CMD check results

0 errors | 0 warnings | 0 notes

## Release summary

This is a major version release (4.0.0) with significant architectural changes:

- Updated client to be model-agnostic to support all current and future Synthesize Bio models
- `model_id` is now passed directly to the API, removing the need for package updates for new models
- Simplified API by removing hardcoded model validations

## Breaking changes

This release includes intentional breaking changes to support the model-agnostic design:

- `predict_query()` now requires a `model_id` parameter
- `get_example_query(model_id)` replaces `get_valid_query()`
- Removed `get_valid_query()`, `get_valid_modalities()`, and `validate_modality()`

## Test environments

- local macOS install, R 4.x
- GitHub Actions (ubuntu-latest, windows-latest, macOS-latest), R release and devel

## Downstream dependencies

There are currently no downstream dependencies for this package.
