R Package API Wrapper Style Guide
This style guide outlines conventions and best practices for developing an R package that serves as an API wrapper. Adhering to these guidelines will ensure a consistent, robust, and user-friendly package that interacts gracefully with external APIs.

I. General Principles
Clarity and Readability: Code should be easy to understand by others and your future self. Prioritize clarity over cleverness.

Idiomatic R: Write R code that feels natural and familiar to R users. Leverage R's functional programming strengths.

Robustness: Anticipate and handle common API issues like network errors, rate limits, and unexpected response structures.

User-Friendliness: Provide clear functions, helpful error messages, and comprehensive documentation.

II. Package Structure and Organization
Follow standard R package directory structure.

R/: Contains all R source code files. Each file should ideally contain a single main function or a set of closely related functions.

man/: Contains Roxygen2-generated .Rd documentation files. Do not edit these manually.

tests/: Contains unit and integration tests. Organized by functionality (e.g., test-auth.R, test-data_fetch.R).

data/: (If applicable) Contains any data objects shipped with the package.

vignettes/: (Recommended) Contains long-form guides and tutorials (R Markdown).

inst/extdata/: (If applicable) For external data files not loaded into memory automatically.

DESCRIPTION: Package metadata, dependencies, and author information.

NAMESPACE: (Generated by Roxygen2) Defines imported and exported functions.

.Rbuildignore: Files to ignore during package build.

renv/: (If using renv) Contains files for reproducible environments.

Example Structure:

myapipackage/
├── R/
│   ├── api_auth.R            # Authentication functions
│   ├── data_fetch.R          # Core data fetching functions
│   ├── data_processing.R     # Helper functions for response parsing
│   └── utils.R               # General utilities
├── man/                      # Generated documentation
├── tests/
│   ├── testthat/
│   │   ├── test-auth.R
│   │   └── test-data_fetch.R
│   └── test_api_integration.R # For integration tests against live API
├── vignettes/
│   └── myapipackage-intro.Rmd
├── DESCRIPTION
├── NAMESPACE
├── .Rbuildignore
├── .renv/                    # if using renv
└── README.md

III. Naming Conventions
Adhere to the Tidyverse style guide where applicable.

Functions and Variables: snake_case (e.g., get_data_from_api(), api_key).

Arguments: snake_case.

S3 Classes: PascalCase (e.g., ApiResponse).

Package Name: Lowercase with no hyphens or underscores (e.g., myapipackage).

IV. Code Layout and Readability
Indentation: Use 2 spaces for indentation.

Line Length: Limit lines to 80 characters. Break lines gracefully for long function calls or arguments.

Spacing:

Spaces around all infix operators (<-, +, =, etc.).

No spaces after ( or before ).

One space before {.

Curly Braces: Always place the opening curly brace at the end of the line, and the closing brace on its own line.

Assignment: Use <- for assignment, not =.

Example:

# Good
my_variable <- 10
if (is.null(x)) {
  x <- 1
}

# Bad
my.variable = 10
if(is.null(x)){x=1}

V. Commenting and Documentation (Roxygen2)
Roxygen2: Use Roxygen2 for all function, data, and package documentation.

Every exported function must have a comprehensive Roxygen2 block.

Include @param for all arguments, @return for return values, @examples for runnable examples, and @export to make functions public.

Use @inheritParams to avoid duplicating parameter documentation across related functions.

Inline Comments: Use # for inline comments to explain complex logic or non-obvious steps.

Example Roxygen2 Block:

#' Fetch data from the API with specified filters.
#'
#' This function sends a GET request to the API endpoint and retrieves data
#' based on the provided query parameters. It handles pagination and
#' basic error conditions.
#'
#' @param endpoint A character string specifying the API endpoint (e.g., "users", "products").
#' @param query_params A named list of query parameters to send with the request.
#'   Defaults to `NULL` for no query parameters.
#' @param api_key A character string for authentication. If `NULL`, attempts
#'   to read from the `MYAPI_KEY` environment variable.
#' @param page_limit An integer specifying the maximum number of pages to fetch.
#'   Defaults to `NULL` for no limit.
#'
#' @return A data frame containing the fetched data, or `NULL` if an error occurs
#'   or no data is found.
#' @export
#'
#' @examples
#' \dontrun{
#'   # Assuming MYAPI_KEY is set in your .Renviron
#'   users <- get_api_data("users", query_params = list(status = "active"))
#'   products <- get_api_data("products", page_limit = 2)
#' }
get_api_data <- function(endpoint, query_params = NULL, api_key = NULL, page_limit = NULL) {
  # Function implementation
}

VI. API Wrapper Specific Guidelines
6.1. API Keys and Authentication
Environment Variables: Never hardcode API keys. Require users to set API keys as environment variables (e.g., via .Renviron). Provide clear instructions on how to do this.

Function Arguments: Allow API keys to be passed as function arguments for convenience or testing, but prioritize environment variables if the argument is NULL.

Secure Handling: Use Sys.getenv() to retrieve API keys.

Authentication Flow: Clearly define and implement the authentication process (e.g., Bearer tokens, OAuth).

Example (API Key Handling):

.get_api_key <- function(api_key = NULL) {
  if (is.null(api_key)) {
    api_key <- Sys.getenv("MYAPI_KEY")
    if (api_key == "") {
      stop("API key not found. Please set the MYAPI_KEY environment variable or pass it directly.")
    }
  }
  return(api_key)
}

6.2. HTTP Requests and Error Handling
httr Package: Use the httr package for making HTTP requests. It provides robust tools for requests, responses, and error handling.

Status Codes: Explicitly check HTTP status codes.

httr::status_code()

httr::http_error()

Provide informative error messages for non-2xx responses.

Rate Limiting: Implement a mechanism to respect API rate limits.

Use httr::RETRY() or custom backoff logic.

Parse Retry-After headers if available.

Network Errors: Handle network connectivity issues gracefully (e.g., tryCatch).

Timeout: Set reasonable timeouts for requests to prevent indefinite waits.

Example (Error Handling with httr):

  response <- httr::GET(url, httr::add_headers(Authorization = paste("Bearer", api_key)),
                        query = query_params, httr::timeout(30))

  if (httr::http_error(response)) {
    status <- httr::status_code(response)
    message <- httr::content(response, "text", encoding = "UTF-8")
    stop(paste0("API request failed with status ", status, ": ", message))
  }
  # ... further processing

6.3. Request and Response Parsing
Consistent Data Structures: Aim to return consistent R data structures (e.g., data frames, lists) regardless of the API response variations.

JSON Handling: Use jsonlite::fromJSON() for parsing JSON responses. Ensure proper handling of NULL values and nested structures.

Flattening/Unnesting: Provide helper functions to flatten nested JSON structures into usable data frames where appropriate.

Column Naming: Standardize column names in returned data frames (snake_case).

6.4. Pagination
If the API supports pagination, implement automatic fetching of all pages up to a user-defined limit or until no more data is available.

VII. Testing
Comprehensive testing is critical for API wrappers due to external dependencies.

Unit Tests (testthat):

Test individual functions in isolation.

Crucially, mock API calls using httptest or webmockr to ensure tests are fast, reliable, and don't depend on network connectivity or live API availability. This avoids hitting rate limits during development.

Integration Tests:

Run tests against a live API environment (e.g., a staging environment or a dedicated test account).

These should be run less frequently than unit tests, perhaps only in CI/CD pipelines.

Require a valid API key (e.g., from environment variables) to run.

CI/CD Integration: Automate test execution (unit and integration) using GitHub Actions, GitLab CI, or similar platforms.

Example (tests/testthat/test-data_fetch.R with mocking):

# tests/testthat/test-data_fetch.R
library(httptest) # For mocking HTTP requests
library(testthat)

with_mock_api({ # This creates a mock API server
  test_that("get_api_data returns a data frame for valid response", {
    # Define a mock response for a specific URL/request
    httptest::expect_GET("https://api.example.com/users", {
      list(
        status = 200,
        headers = list("Content-Type" = "application/json"),
        body = '[{"id": 1, "name": "Alice"}, {"id": 2, "name": "Bob"}]'
      )
    })

    # Call the function under test (it will hit the mock API, not the real one)
    data <- get_api_data("users", api_key = "dummy_key")

    expect_s3_class(data, "data.frame")
    expect_equal(nrow(data), 2)
    expect_equal(colnames(data), c("id", "name"))
  })

  test_that("get_api_data handles 404 errors", {
    httptest::expect_GET("https://api.example.com/nonexistent", {
      list(
        status = 404,
        headers = list("Content-Type" = "application/json"),
        body = '{"message": "Not Found"}'
      )
    })

    expect_error(get_api_data("nonexistent", api_key = "dummy_key"),
                 "API request failed with status 404")
  })
})

VIII. Dependency Management
DESCRIPTION File: List all package dependencies in the Imports: field of your DESCRIPTION file.

renv: (Recommended) Use renv for reproducible package environments. This captures exact package versions, ensuring consistent builds across different machines.

Run renv::init() to set up.

Run renv::snapshot() to save locked dependencies.

Run renv::restore() to install locked dependencies.

IX. Contribution Guidelines
New Branches: Create a new branch for each feature or bug fix.

Pull Requests: Submit pull requests for review.

Ensure all new code adheres to this style guide.

Include comprehensive Roxygen2 documentation for new functions.

Add or update unit and integration tests as appropriate.

Run devtools::check() before submitting to ensure package health.

Issue Tracking: Link pull requests to relevant issues.

By consistently applying these guidelines, you will build a high-quality R package API wrapper that is easy to develop, maintain, and use.
