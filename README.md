# rsynthbio <img src="assets/logomark.png" style="width: 80px;" alt="Logomark">

`rsynthbio` is an R package that provides a convenient interface to the [Synthesize Bio](https://www.synthesize.bio/) API, allowing users to generate realistic gene expression data based on specified biological conditions. This package enables researchers to easily access AI-generated transcriptomic data for various modalities, including bulk RNA-seq and single-cell RNA-seq.

To generate datasets without code, use our [web platform](https://app.synthesize.bio/datasets/).

[See the full documentation here](https://synthesizebio.github.io/rsynthbio/).

For questions, suggestions, and support, email us at [support@synthesize.bio](mailto:support@synthesize.bio).

## How to Install

You can install `rsynthbio` from CRAN:

```
install.packages("rsynthbio")
```

To install the development version from GitHub, use the `remotes` package:

```
if (!("remotes" %in% installed.packages())) {
  install.packages("remotes")
}
remotes::install_github("synthesizebio/rsynthbio")
```

Once installed, load the package:

```
library(rsynthbio)
```

## Authentication

Before using the Synthesize Bio API, you need to set up your API token. The package provides a secure way to handle authentication:

```
# Securely prompt for and store your API token
# The token will not be visible in the console
set_synthesize_token()

# You can also store the token in your system keyring for persistence
# across R sessions (requires the 'keyring' package)
set_synthesize_token(use_keyring = TRUE)
```

Load your API token for a session:

```
# In future sessions, load the stored token
load_synthesize_token_from_keyring()

# Check if a token is already set
has_synthesize_token()
```

You can obtain an API token by registering at [Synthesize Bio](https://app.synthesize.bio).

### Security Best Practices

For security reasons, remember to clear your token when you're done:

```
# Clear token from current session
clear_synthesize_token()

# Clear token from both session and keyring
clear_synthesize_token(remove_from_keyring = TRUE)
```

Never hard-code your token in scripts that will be shared or committed to version control.

## Basic Usage

Please see the [Getting Started guide](https://synthesizebio.github.io/rsynthbio/articles/getting-started.html) for comprehensive documentation.

### Designing Queries

#### Choosing a Modality

The modality (data type to generate) is specified when creating a query:

```
# Check available modalities
get_valid_modalities()
# Returns: "bulk" "single-cell"

# Create a bulk RNA-seq query
bulk_query <- get_valid_query(modality = "bulk")

# Create a single-cell RNA-seq query
sc_query <- get_valid_query(modality = "single-cell")
```

Currently supported modalities:

- **`bulk`**: Bulk RNA-seq data (supports both "sample generation" and "mean estimation" modes)
- **`single-cell`**: Single-cell RNA-seq data (supports "mean estimation" mode only)

#### Query Structure

The `get_valid_query()` function returns a correctly structured example query:

```
# Get a sample query
query <- get_valid_query(modality = "bulk")

# Inspect the query structure
str(query)
```

The query consists of:

1. **`modality`**: The type of gene expression data to generate ("bulk" or "single-cell")
2. **`mode`**: The prediction mode that controls how expression data is generated
   - **"sample generation"**: Realistic synthetic data with measurement error (bulk only)
   - **"mean estimation"**: Stable mean estimates (bulk and single-cell)
3. **`inputs`**: A list of biological conditions to generate data for

Each input contains:

- **`metadata`**: A list of metadata key-value pairs describing the biological sample
- **`num_samples`**: The number of samples to generate for this condition

See the [Query Parameters](#query-parameters) section below for detailed information on mode and other optional fields.

### Making Predictions

The API uses an **asynchronous model**: the query is submitted, the system polls for completion, and results are downloaded when ready. This happens automatically:

```
# Request raw counts data
result <- predict_query(query, as_counts = TRUE)

# The function will automatically:
# 1. Submit your query to the API
# 2. Poll for completion (default: checks every 2 seconds)
# 3. Download and parse results when ready
# 4. Return formatted data frames

# Access the results
metadata <- result$metadata
expression <- result$expression
```

### Advanced Options

You can customize the polling behavior and model parameters:

```
# Adjust polling timeout (default: 15 minutes)
result <- predict_query(
  query,
  poll_timeout_seconds = 1800,  # 30 minutes
  poll_interval_seconds = 5      # Check every 5 seconds
)

# Get log-transformed CPM instead of raw counts
result_log <- predict_query(query, as_counts = FALSE)

# Use deterministic latents for reproducible results
# (removes randomness from latent sampling)
query$deterministic_latents <- TRUE
result_det <- predict_query(query)

# Specify custom library size (total count)
query$total_count <- 5000000
result_custom <- predict_query(query)

# Combine multiple options in the query
query_combined <- get_valid_query()
query_combined$total_count <- 8000000
query_combined$deterministic_latents <- TRUE
result_combined <- predict_query(query_combined, as_counts = TRUE)
```

#### Query Parameters

In addition to the required fields, queries support several optional parameters:

**mode** (character, required)

Controls the type of prediction the model generates:

- **"sample generation"**: The model creates a biological distribution, samples to predict gene expression distribution capturing measurement error, and then samples that distribution to generate realistic-looking synthetic data. This mode mimics real experimental measurements. **Available for bulk queries only.**
- **"mean estimation"**: The model creates a biological distribution, samples to predict gene expression distribution capturing measurement error, and returns the mean as the prediction. Provides stable estimates. **Available for bulk and single-cell queries.**

> **Note:** Single-cell queries only support "mean estimation" mode.

**total_count** (integer, optional)

- Library size for converting log CPM to raw counts

**deterministic_latents** (logical, optional)

- If `TRUE`, uses mean of latent distributions instead of sampling
- Produces deterministic, reproducible outputs
- Default: `FALSE`

**seed** (integer, optional)

- Random seed for reproducibility with stochastic sampling

See the [Getting Started guide](https://synthesizebio.github.io/rsynthbio/articles/getting-started.html) for detailed documentation on all available metadata fields and parameters.

## Rate Limits

Free usage of Synthesize Bio is limited.
If you exceed this limit, you will receive an error message stating that you've exceeded your limit.
To generate more samples, please contact us at [support@synthesize.bio](mailto:support@synthesize.bio).
