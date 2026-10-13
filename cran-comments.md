## Release summary

This patch release (4.2.1) makes hosted request-validation errors readable while retaining structured response details and existing error compatibility.

- Adds regression coverage for structured, malformed, and legacy error responses.
- Uses GEM-1 in public SDK examples; GEM-2 remains staff-only.
- Marks two existing authenticated API vignette examples as nonexecuting so CRAN checks do not require credentials or a live API.

## Check results

0 errors | 0 warnings | 0 notes

R CMD check --as-cran --no-manual passed, including vignette rebuilding.
All 227 testthat assertions passed. API credentials are not required for checks.

## Test environments

- Local macOS, R 4.5.1.
- GitHub Actions: Linux, macOS, and Windows (release R) passed on the merged error-handling revision. Release-candidate CI is tracked in the release PR.
