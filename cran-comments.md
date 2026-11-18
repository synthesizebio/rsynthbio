## Release summary

This minor release adds a client for Synthesize Bio's VERDICT service:
`verdict_client()` runs single model calls and complete virtual trials, and the
new `Trial`, `TrialSet` and `Treatment` classes build, import, edit, review and
vary trial designs. Existing functions are unchanged.

R6 is a new import, and IRdisplay is a new suggested package used only for
notebook display.

## Check results

All 595 testthat assertions passed locally with no failures, warnings or skips.
No API credentials are required for the checks.

R CMD build completed successfully, including rebuilding all vignettes.
R CMD check --as-cran --no-manual completed with 0 errors, 0 warnings and 1 note:

- The local environment could not verify the current time.

## Test environment

- Local macOS 26.5, R 4.4.2, arm64.
