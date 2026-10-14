## Release summary

This is a patch release as version 4.2.2 following CRAN's request that the new
maintainer submit a new version with updated maintainer information.

The designated maintainer is now Alex David <alex@synthesize.bio>, replacing
Synthesize Bio <candace@synthesize.bio>. Synthesize Bio remains credited as an
author. The maintainer transfer has been discussed with CRAN in the existing
correspondence. Version 4.2.1 was published on 2026-10-13 with the previous
maintainer's details. No runtime code or dependencies have changed since 4.2.1.

## Check results

All 227 testthat assertions passed locally with no failures, warnings, or skips.
API credentials are not required for checks.

Local R CMD check --as-cran --no-manual completed with 0 errors, 0 warnings,
and 2 notes, including successful vignette rebuilding. The incoming-check note
reports the maintainer change and the one-day interval since the last release;
this update is being submitted at CRAN's request. The other note is the local
environment's inability to verify the current time.

GitHub Actions on Linux, macOS, and Windows with R 4.6.1 completed with 0 errors,
0 warnings, and 0 notes. All 227 testthat assertions passed on each platform. CI uses
--as-cran --no-manual --no-build-vignettes; rendered vignettes are included in
the source package. The submission tarball was built and checked with R 4.6.1
on macOS. R-devel was not available in the configured validation environments.

## Test environments

- Local macOS 26.5, R 4.4.2.
- GitHub Actions: Ubuntu 24.04 and macOS (release R 4.6.1).
- GitHub Actions: Windows Server 2022 (release R 4.6.1).
