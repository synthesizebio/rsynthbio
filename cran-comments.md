## Release summary

This patch release fixes hosted GEM-2 result formatting for baseline expression,
reference-based expression, metadata prediction and reconstruction. It preserves
nested metadata, aligns classifier labels/probabilities and avoids rounding
prediction values while reading JSON results.

Version 4.2.2 was published on 2026-10-14 to complete the maintainer transfer.
The short interval for 4.2.3 is intentional: this is a corrective patch for
existing GEM-2 operations, including metadata results that could not previously
be formatted correctly. Maintainer details and package dependencies are unchanged.

## Check results

All 340 testthat assertions passed locally with no failures, warnings or skips.
No API credentials are required for the checks.

R CMD build completed successfully, including rebuilding all vignettes.
R CMD check --as-cran --no-manual completed with 0 errors, 0 warnings and 2 notes:

- The incoming feasibility check reports two days since the previous update;
  the reason for this corrective release is explained above.
- The local environment could not verify the current time.

## Test environment

- Local macOS 26.5, R 4.4.2, arm64.
