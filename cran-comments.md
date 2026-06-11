## R CMD check results

0 errors | 0 warnings | 0 notes

## Release summary

This is a minor release (4.2.0):

- Added self-hosted model support: `predict_query()` and `get_example_query()`
  gain a `self_hosted` argument (also resolved from the `SYNTHESIZE_SELF_HOSTED`
  environment variable) for querying partner-hosted model containers that
  return an Apache Arrow IPC stream.
- Added per-model base-URL resolution via `SYNTHESIZE_API_BASE_URL__<MODEL>`.
- Self-hosted predictions use a longer 600s request timeout.

## Test environments

- local macOS (Sequoia 15.7.3), R 4.5.1
- win-builder (R-devel and R-release)
- GitHub Actions (ubuntu-24.04, windows-latest, macOS-latest), R release

## Downstream dependencies

There are currently no downstream dependencies for this package.
