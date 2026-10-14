## Release summary

This is a resubmission as version 4.2.2 following CRAN's request that the new
maintainer submit a new version with updated maintainer information.

The designated maintainer is now Alex David <alex@synthesize.bio>, replacing
Synthesize Bio <candace@synthesize.bio>. Synthesize Bio remains credited as an
author. The maintainer transfer has been discussed with CRAN in the existing
correspondence. No runtime code has changed since the 4.2.1 submission.

The changes from 4.2.1, not yet published on CRAN, are included:

- Makes hosted request-validation errors readable while retaining structured response details and existing error compatibility.
- Adds regression coverage for structured, malformed, and legacy error responses.
- Uses GEM-1 in public SDK examples; GEM-2 remains staff-only.
- Marks two existing authenticated API vignette examples as nonexecuting so CRAN checks do not require credentials or a live API.

## Check results

All 227 testthat assertions passed locally with no failures, warnings, or skips.
R CMD check validation of the 4.2.2 source package is pending. API credentials
are not required for checks.

## Test environments

- Local macOS, R 4.4.2.
- GitHub Actions: Linux, macOS, and Windows (release R); results pending.
