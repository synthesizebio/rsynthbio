# rsynthbio 2.3.0 (Development)

## Major Changes

- **Async API Support**: Migrated to async API model matching the Python `pysynthbio` implementation
  - `predict_query()` now uses an asynchronous workflow: submit → poll → download
  - Added automatic status polling with configurable intervals and timeouts
  - New parameters: `poll_interval_seconds` (default: 2), `poll_timeout_seconds` (default: 900)
- **Single-Cell Support**: Added support for single-cell RNA-seq data via the "czi" modality
  - `get_valid_query()` now accepts a `modality` parameter ("bulk" or "czi")
  - `get_valid_modalities()` now returns both "bulk" and "czi"
  - Updated `MODEL_MODALITIES` to include both modalities

## Breaking Changes

- **API Base URL Changed**: `API_BASE_URL` is now `https://app.synthesize.bio` (base URL only, not versioned endpoint)
- **Parameter Changes in `predict_query()`**:
  - Removed `raw_response` parameter
  - Removed `url` parameter (replaced with `api_base_url`)
  - Added `poll_interval_seconds` parameter
  - Added `poll_timeout_seconds` parameter
  - Added `return_download_url` parameter

## New Functions

- `resolve_api_slug()`: Maps modality to API endpoint slug (internal)
- `start_model_query()`: Initiates async model query (internal)
- `poll_model_query()`: Polls status endpoint until completion (internal)
- `get_json()`: Fetches results from signed download URL (internal)
- `transform_result_to_frames()`: Converts JSON to data frames (internal)

## Bug Fixes

- Fixed metadata handling for queries with empty metadata fields
- Improved error messages for failed queries (now shows error message instead of error URL)
- Better handling of single-cell count data format (dict vs list)

## Documentation

- Updated README.md to explain async API behavior
- Updated vignettes with async workflow examples
- Added examples for both bulk and single-cell modalities
- Clarified polling timeout and interval configuration

## Tests

- Added comprehensive test suite mirroring Python implementation
- Added live API tests for both bulk and single-cell modalities
- Added tests for invalid metadata validation
- Added mocked async workflow tests (success, failure, timeout scenarios)

# rsynthbio 2.2.1

- rsynthbio now works with the GEM-1 model from Synthesize Bio.
- older versions of Synthesize Bio models are no longer supported.
- updated documentation to reflect the new model.
- more flexibility for specifying the url for the Synthesize Bio API.
