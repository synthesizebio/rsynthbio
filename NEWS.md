# rsynthbio 4.0.0

## Major Changes

- **Model Agnostic Client**: The client has been updated to support any model ID via the `model_id` parameter, enabling support for all current and future Synthesize Bio models without package updates.
- **Dynamic Model Support**: Removed hardcoded model lists and validations to allow flexible usage of new models.

## Breaking Changes

- **`predict_query()`**: Now requires `model_id` parameter.
- **New Query Helper**: `get_example_query(model_id)` replaces `get_valid_query()`.
- **Removed Functions**: `get_valid_query()`, `get_valid_modalities()`, and `validate_modality()` have been removed in favor of dynamic model support.
