#' @title Predict Gene Expression
#' @description Sends a query to the Synthesize Bio API (v2.0) for prediction
#' and retrieves gene expression samples. This function validates the query, sends it
#' to the API, and processes the response into usable data frames.
#'
#' @param query A list representing the query data to send to the API.
#'        Use `get_valid_query()` to generate an example.
#' @param raw_response If you do not want the gene expression data extracted from the JSON
#' response set this to FALSE. Default is to return only the expression and metadata.
#' @param as_counts passed to extract_expression() function. Logical, if FALSE,
#' transforms the predicted expression counts into logCPM (default is TRUE, returning raw counts).
#' @return A list with two data frames:
#'         - 'metadata': contains metadata for each sample
#'         - 'expression': contains expression data for each sample
#' Throws an error If the API request fails or the response structure is invalid.
#' @importFrom httr POST add_headers content http_status status_code
#' @importFrom jsonlite toJSON fromJSON
#' @examples
#' # Set your API key (in practice, use a more secure method)
#' \dontrun{
#'
#' # To start using pysynthbio, first you need to have an account with synthesize.bio.
#' # Go here to create one: https://app.synthesize.bio/
#'
#' Sys.setenv(SYNTHESIZE_API_KEY = "your_api_key_here")
#'
#' # Create a query
#' query <- get_valid_query()
#'
#' # Request raw counts
#' result <- predict_query(query, as_counts = TRUE)
#'
#' # Access the results
#' metadata <- result$metadata
#' expression <- result$expression
#'
#' # Request log CPM transformed data
#' log_result <- predict_query(query, as_counts = FALSE)
#' log_expression <- log_result$expression
#'
#' # Explore the top expressed genes in the first sample
#' head(sort(expression[1, ], decreasing = TRUE))
#' }
#' @export
predict_query <- function(query, raw_response = FALSE, as_counts = TRUE) {
  if (!has_synthesize_token()) {
    stop("Please set your API key for synthesize Bio using set_synthesize_token()")
  }

  api_url <- paste0(API_BASE_URL, "/api/model/v2.0")

  validate_query(query)

  # Convert the query list to JSON
  query_json <- toJSON(query, auto_unbox = TRUE)

  # Make the API request
  response <- POST(
    url = api_url,
    add_headers(
      Accept = "application/json",
      Authorization = paste("Bearer", Sys.getenv("SYNTHESIZE_API_KEY")),
      `Content-Type` = "application/json"
    ),
    body = query_json,
    encode = "json"
  )

  if (http_status(response)$category != "Success") {
    stop(paste0(
      "API request to ", api_url, " failed with status ",
      status_code(response), ": ", content(response, "text")
    ))
  }

  # Parse JSON response and handle errors
  parsed_content <- tryCatch(
    {
      json_text <- content(response, "text")
      parsed_content <- fromJSON(json_text, simplifyDataFrame = TRUE)
    },
    error = function(e) {
      stop(paste0("Failed to decode JSON from API response: ", e$message))
    }
  )

  # If response is a single-item list, use its contents
  if (is.list(parsed_content) && length(parsed_content) == 1 && is.list(parsed_content[[1]])) {
    parsed_content <- parsed_content[[1]]
  }

  # Check for API-reported errors
  if (!is.null(parsed_content$error)) {
    stop(paste0("API error: ", parsed_content$error))
  }
  if (!is.null(parsed_content$errors)) {
    stop(paste0("API errors: ", paste(parsed_content$errors, collapse = "; ")))
  }

  if (!raw_response) {
    result <- extract_expression_data(
      parsed_content,
      as_counts = as_counts
    )
  } else {
    result <- parsed_content
  }

  return(result)
}


