# Release Process

## Branching Strategy

We follow a **stable mainline** approach where `main` always reflects the latest published version on CRAN.
Note there is a small delay between when a release is merged to `main` and when it is published to CRAN.

### Workflow

1. Create a pre-release branch from `main` (e.g., `release/v3.1.0`)
1. Internal testing by installing from GitHub branch
  - `devtools::install_github("synthesizebio/rsynthbio@release/v3.1.0")`
1. Merge to `main` when validated
1. Publish to CRAN
  - `publish_to_cran` github action
