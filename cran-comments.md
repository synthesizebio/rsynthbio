## R CMD check results

0 errors | 0 warnings | 0 notes

## Release summary

This is a major version release (3.0.2) with significant updates:

- Migrated to async API model to match Synthesize Bio's new API architecture
- Added support for single-cell RNA-seq data (new "single-cell" modality)
- Updated to work with GEM-1 model from Synthesize Bio
- Contains breaking changes to `predict_query()` function parameters

## Breaking changes

This release includes intentional breaking changes:

- Changed API base URL from versioned endpoint to base URL
- Removed `raw_response` and `url` parameters from `predict_query()`
- Added new polling-related parameters for async API support

These changes align the R package with the Python implementation and support the new API architecture.

## Test environments

- local macOS install, R 4.x
- GitHub Actions (ubuntu-latest, windows-latest, macOS-latest), R release and devel

## Downstream dependencies

There are currently no downstream dependencies for this package.
