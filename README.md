# rsynthbio

`rsynthbio` is an R package that provides a convenient interface to the Synthesize Bio API, allowing users to generate realistic gene expression data based on specified biological conditions. This package enables researchers to easily access AI-generated transcriptomic data for various modalities including bulk RNA-seq, single-cell RNA-seq, microarray data, and more.

[See the full documentation pages here](https://synthesizebio.github.io/rsynthbio/).

## How to install

You can install `rsynthbio` from CRAN:

```{r installation, eval=FALSE}
install.packages("rsynthbio")
```

If you want the development version, you can install using the `remotes` package to install from GitHub:

```{r github-installation, eval=FALSE}
if (!("remotes" %in% installed.packages())) {
  install.packages("remotes")
}
remotes::install_github("synthesizebio/rsynthbio")
```

Once installed, load the package:

```{r setup}
library(rsynthbio)
```

## Authentication

Before using the Synthesize Bio API, you need to set up your API token. The package provides a secure way to handle authentication:

```{r auth-secure}
# Securely prompt for and store your API token
# The token will not be visible in the console
set_synthesize_token()

# You can also store the token in your system keyring for persistence
# across R sessions (requires the 'keyring' package)
set_synthesize_token(use_keyring = TRUE)
```

Loading your API key for a session. 

```{r}
# In future sessions, load the stored token
load_synthesize_token_from_keyring()

# Check if a token is already set
has_synthesize_token()
```

You can obtain an API token by registering at [Synthesize Bio](https://app.synthesize.bio).

### Security Best Practices

For security reasons, remember to clear your token when you're done:

```{r clear-token, eval = FALSE}
# Clear token from current session
clear_synthesize_token()

# Clear token from both session and keyring
clear_synthesize_token(remove_from_keyring = TRUE)
```

Never hard-code your token in scripts that will be shared or committed to version control.

## Basic Usage

### Available Modalities

The package supports various data modalities. You can view all available modalities with:

```{r modalities}
# Check available modalities
get_valid_modalities()
```

### Creating a Query

The first step to generating AI-generated gene expression data is to create a query. The package provides a sample query that you can modify:

```{r query}
# Get a sample query
query <- get_valid_query()

# Inspect the query structure
str(query)
```

The query consists of:

1. `output_modality`: The type of gene expression data to generate
2. `mode`: The prediction mode (e.g., "mean estimation")
3. `inputs`: A list of biological conditions to generate data for


```{r predict}
# Request raw counts data
result <- predict_query(query)

# Structure of the result
result
```
