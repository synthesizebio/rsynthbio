#' @title List Available Models
#' @description Returns a list of all models available in the Synthesize Bio API.
#' Each model has a unique ID that can be used with predict_query() and get_example_query().
#'
#' @param api_base_url The base URL for the API server. Default is API_BASE_URL.
#' @return A list or data frame containing available models with their IDs and metadata.
#' @examples
#' \dontrun{
#' # Get all available models
#' models <- list_models()
#' print(models)
#' }
#' @importFrom httr GET add_headers content status_code timeout
#' @importFrom jsonlite fromJSON
#' @export
list_models <- function(api_base_url = API_BASE_URL) {
  url <- paste0(api_base_url, "/api/models")
  return(make_api_request(url, "List models"))
}

#' @title Get Example Query for Model
#' @description Retrieves an example query structure for a specific model.
#' This provides a template that can be modified for your specific needs.
#'
#' @param model_id Character string specifying the model ID (e.g., "gem-1-bulk", "gem-1-sc").
#' @param api_base_url The base URL for the API server. Default is API_BASE_URL.
#' @return A list representing a valid query structure for the specified model.
#' @examples
#' \dontrun{
#' # Get example query for bulk RNA-seq model
#' query <- get_example_query(model_id = "gem-1-bulk")$example_query
#'
#' # Get example query for single-cell model
#' query_sc <- get_example_query(model_id = "gem-1-sc")$example_query
#'
#' # Modify the query structure
#' query$inputs[[1]]$num_samples <- 10
#' }
#' @importFrom httr GET add_headers content status_code timeout
#' @importFrom jsonlite fromJSON
#' @export
get_example_query <- function(model_id, api_base_url = API_BASE_URL) {
  url <- paste0(api_base_url, "/api/models/", model_id, "/example-query")
  return(make_api_request(url, "Get example query"))
}

#' @title Start Model Query
#' @description Internal function to start an async model query
#' @param api_base_url The base URL for the API
#' @param model_id The model ID for the specific model
#' @param query The query list
#' @return The model query ID
#' @importFrom httr POST add_headers content status_code timeout
#' @importFrom jsonlite toJSON fromJSON
#' @keywords internal
start_model_query <- function(api_base_url, model_id, query) {
  url <- paste0(api_base_url, "/api/models/", model_id, "/predict")
  query_json <- toJSON(query, auto_unbox = TRUE)

  response <- tryCatch(
    {
      POST(
        url = url,
        add_headers(
          Accept = "application/json",
          Authorization = paste("Bearer", Sys.getenv("SYNTHESIZE_API_KEY")),
          `Content-Type` = "application/json"
        ),
        body = query_json,
        encode = "json",
        timeout(DEFAULT_TIMEOUT)
      )
    },
    error = function(e) {
      stop(paste0("Predict request failed due to a network issue: ", e$message))
    }
  )

  if (status_code(response) >= 400) {
    stop(paste0(
      "Predict request failed with status ",
      status_code(response), ": ", content(response, "text")
    ))
  }

  parsed_content <- tryCatch(
    {
      fromJSON(content(response, "text"), simplifyDataFrame = TRUE)
    },
    error = function(e) {
      stop(paste0("Failed to decode JSON from predict response: ", e$message))
    }
  )

  if (!is.list(parsed_content) || is.null(parsed_content$modelQueryId)) {
    stop(paste0(
      "Unexpected response from predict endpoint: ",
      paste(names(parsed_content), collapse = ", ")
    ))
  }

  return(as.character(parsed_content$modelQueryId))
}

#' @title Poll Model Query
#' @description Internal function to poll the status endpoint until ready/failed or timeout
#' @param api_base_url The base URL for the API
#' @param model_query_id The model query ID to poll
#' @param poll_interval Seconds between polling attempts
#' @param timeout_seconds Maximum total seconds to wait
#' @return A list with status and payload
#' @importFrom httr GET add_headers content status_code timeout
#' @importFrom jsonlite fromJSON
#' @keywords internal
poll_model_query <- function(api_base_url, model_query_id, poll_interval, timeout_seconds) {
  start_time <- Sys.time()
  status_url <- paste0(api_base_url, "/api/model-queries/", model_query_id, "/status")
  last_payload <- list()

  while (TRUE) {
    response <- tryCatch(
      {
        GET(
          url = status_url,
          add_headers(
            Accept = "application/json",
            Authorization = paste("Bearer", Sys.getenv("SYNTHESIZE_API_KEY"))
          ),
          timeout(DEFAULT_TIMEOUT)
        )
      },
      error = function(e) {
        stop(paste0("Status request failed due to a network issue: ", e$message))
      }
    )

    if (status_code(response) >= 400) {
      stop(paste0(
        "Status request failed with status ",
        status_code(response), ": ", content(response, "text")
      ))
    }

    payload <- tryCatch(
      {
        fromJSON(content(response, "text"), simplifyDataFrame = TRUE)
      },
      error = function(e) {
        stop(paste0("Failed to decode JSON from status response: ", e$message))
      }
    )

    if (!is.list(payload) || is.null(payload$status)) {
      stop(paste0(
        "Unexpected status response: ",
        paste(names(payload), collapse = ", ")
      ))
    }

    status <- as.character(payload$status)
    last_payload <- payload

    if (status %in% c("ready", "failed")) {
      return(list(status = status, payload = payload))
    }

    elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    if (elapsed > timeout_seconds) {
      return(list(status = status, payload = last_payload))
    }

    Sys.sleep(max(1, as.integer(poll_interval)))
  }
}

#' @title Get JSON from URL
#' @description Internal function to fetch JSON from a URL
#' @param url The URL to fetch from
#' @return The parsed JSON content
#' @importFrom httr GET content status_code timeout
#' @importFrom jsonlite fromJSON
#' @keywords internal
get_json <- function(url) {
  response <- tryCatch(
    {
      GET(url, timeout(DEFAULT_TIMEOUT))
    },
    error = function(e) {
      stop(paste0("Failed to fetch download URL due to a network issue: ", e$message))
    }
  )

  if (status_code(response) >= 400) {
    stop(paste0(
      "Download URL fetch failed with status ",
      status_code(response), ": ", content(response, "text")
    ))
  }

  parsed_content <- tryCatch(
    {
      fromJSON(
        content(response, "text", encoding = "UTF-8"),
        simplifyDataFrame = TRUE
      )
    },
    error = function(e) {
      stop(paste0("Failed to decode JSON from download URL response: ", e$message))
    }
  )

  return(parsed_content)
}

#' @title Make Authenticated API Request
#' @description Internal helper to make authenticated GET requests
#' @param url The URL to fetch
#' @param context_msg String describing the request context for error messages
#' @return The parsed JSON content
#' @importFrom httr GET add_headers content status_code timeout
#' @importFrom jsonlite fromJSON
#' @keywords internal
make_api_request <- function(url, context_msg) {
  if (!has_synthesize_token()) {
    stop("Please set your API key for Synthesize Bio using set_synthesize_token()")
  }

  response <- tryCatch(
    {
      GET(
        url = url,
        add_headers(
          Accept = "application/json",
          Authorization = paste("Bearer", Sys.getenv("SYNTHESIZE_API_KEY"))
        ),
        timeout(DEFAULT_TIMEOUT)
      )
    },
    error = function(e) {
      stop(paste0(context_msg, " request failed due to a network issue: ", e$message))
    }
  )

  if (status_code(response) >= 400) {
    stop(paste0(
      context_msg, " request failed with status ",
      status_code(response), ": ", content(response, "text")
    ))
  }

  parsed_content <- tryCatch(
    {
      fromJSON(content(response, "text"), simplifyDataFrame = TRUE)
    },
    error = function(e) {
      stop(paste0("Failed to decode JSON from ", tolower(context_msg), " response: ", e$message))
    }
  )

  return(parsed_content)
}


#' @title Predict Gene Expression
#' @description Sends a query to the Synthesize Bio API for prediction
#' and retrieves gene expression samples. This function sends the query
#' to the API and processes the response into usable data frames.
#'
#' @param query A list representing the query data to send to the API.
#'        Use `get_example_query()` to generate an example. The query supports additional
#'        optional fields:
#'        \itemize{
#'          \item `total_count` (integer): Library size used when converting predicted log CPM
#'                back to raw counts. Higher values scale counts up proportionally.
#'          \item `deterministic_latents` (logical): If TRUE, the model uses the mean of each
#'                latent distribution instead of sampling, producing deterministic outputs for
#'                the same inputs. Useful for reproducibility.
#'          \item `seed` (integer): Random seed for reproducibility.
#'        }
#' @param model_id Character string specifying the model ID (e.g., "gem-1-bulk", "gem-1-sc").
#'        Use `list_models()` to see available models.
#' @param api_base_url The base URL for the API server. Default is API_BASE_URL.
#' @param poll_interval_seconds Seconds between polling attempts of the status endpoint.
#'        Default is DEFAULT_POLL_INTERVAL_SECONDS (2).
#' @param poll_timeout_seconds Maximum total seconds to wait before timing out.
#'        Default is DEFAULT_POLL_TIMEOUT_SECONDS (900 = 15 minutes).
#' @param return_download_url Logical, if TRUE, returns a list containing the signed
#'        download URL instead of parsing into data frames. Default is FALSE.
#' @param ... Additional parameters to include in the query body. These are passed
#'        directly to the API and validated server-side.
#' @return A list. If `return_download_url` is `FALSE` (default), the list contains
#'         two data frames: `metadata` and `expression`. If `TRUE`, the list
#'         contains `download_url` and empty `metadata` and `expression` data frames.
#' @importFrom httr POST GET add_headers content http_status status_code timeout
#' @importFrom jsonlite toJSON fromJSON
#' @importFrom utils modifyList
#' @examples
#' # Set your API key (in practice, use a more secure method)
#' \dontrun{
#'
#' # To start using rsynthbio, first you need to have an account with synthesize.bio.
#' # Go here to create one: https://app.synthesize.bio/
#'
#' set_synthesize_token()
#'
#' # Get available models
#' models <- list_models()
#'
#' # Create a query for a specific model
#' query <- get_example_query(model_id = "gem-1-bulk")$example_query
#'
#' # Request raw counts
#' result <- predict_query(query, model_id = "gem-1-bulk")
#'
#' # Access the results
#' metadata <- result$metadata
#' expression <- result$expression
#'
#' # Explore the top expressed genes in the first sample
#' head(sort(expression[1, ], decreasing = TRUE))
#'
#' # Use deterministic latents for reproducible results
#' query$deterministic_latents <- TRUE
#' result_det <- predict_query(query, model_id = "gem-1-bulk")
#'
#' # Specify a custom total count (library size)
#' query$total_count <- 5000000
#' result_custom <- predict_query(query, model_id = "gem-1-bulk")
#' }
#' @export
predict_query <- function(query,
                          model_id,
                          api_base_url = API_BASE_URL,
                          poll_interval_seconds = DEFAULT_POLL_INTERVAL_SECONDS,
                          poll_timeout_seconds = DEFAULT_POLL_TIMEOUT_SECONDS,
                          return_download_url = FALSE,
                          ...) {
  if (!has_synthesize_token()) {
    stop("Please set your API key for Synthesize Bio using set_synthesize_token()")
  }

  # Validate base URL
  if (!grepl("^https?://", api_base_url)) {
    stop(
      paste0(
        "Invalid api_base_url: ",
        api_base_url,
        ". Must start with http:// or https://"
      )
    )
  }

  # Add source field for reporting
  query$source <- "rsynthbio"

  # Merge any additional ... args into the query (validated server-side)
  extra_args <- list(...)
  if (length(extra_args) > 0) {
    query <- modifyList(query, extra_args)
  }

  # Start async query
  model_query_id <- start_model_query(
    api_base_url = api_base_url,
    model_id = model_id,
    query = query
  )

  # Poll for completion
  poll_result <- poll_model_query(
    api_base_url = api_base_url,
    model_query_id = model_query_id,
    poll_interval = poll_interval_seconds,
    timeout_seconds = poll_timeout_seconds
  )

  status <- poll_result$status
  payload <- poll_result$payload

  if (status == "failed") {
    # payload contains message if available
    err_msg <- if (is.list(payload) && !is.null(payload$message)) {
      payload$message
    } else {
      NULL
    }
    stop(paste0(
      "Model query failed. ",
      if (!is.null(err_msg)) err_msg else "No error message provided."
    ))
  }

  if (status != "ready") {
    stop(paste0(
      "Model query did not complete in time (status=", status, "). ",
      "Consider increasing poll_timeout_seconds."
    ))
  }

  # When ready, payload should contain a signed downloadUrl to the final JSON
  download_url <- if (is.list(payload) && !is.null(payload$downloadUrl)) {
    payload$downloadUrl
  } else {
    NULL
  }
  if (is.null(download_url)) {
    stop("Response missing downloadUrl when status=ready")
  }

  if (return_download_url) {
    # Caller wants the URL only; return in a structured payload
    return(list(
      metadata = data.frame(),
      expression = data.frame(),
      download_url = download_url
    ))
  }

  # Fetch the final results JSON and transform to data frames
  final_json <- get_json(download_url)

  # Get appropriate transformer for this model
  transformer <- get_output_transformer(model_id)

  # Apply transformation
  result <- transformer(final_json)

  return(result)
}
