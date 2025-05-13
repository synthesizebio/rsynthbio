
#' Returns a vector of possible output modalities for the supported model.
#'
#' @return A character vector containing the valid modality strings.
get_valid_modalities <- function() {
  unlist(model_modalities$combined$"v1.0")
}

#' Generates a sample query for prediction and validation for the v1.0 model.
#'
#' @return A list representing a valid query structure for v1.0.
#' @examples
#'
#' get_valid_query()
#'
get_valid_query <- function() {
  list(
    output_modality = "sra",
    mode = "mean estimation",
    return_classifier_probs = TRUE,
    seed = 11,
    inputs = list(
      list(
        metadata = list(
          cell_line = "A-549",
          perturbation = "ABL1",
          perturbation_type = "crispr",
          perturbation_time = "96 hours",
          sample_type = "cell line"
        ),
        num_samples = 5
      ),
      list(
        metadata = list(
          disease = "gastrointestinal stromal tumor",
          age = "65 years",
          sex = "female",
          sample_type = "primary tissue",
          tissue = "stomach"
        ),
        num_samples = 5
      )
    )
  )
}

#' Validates the structure and contents of the query based on the v1.0 model.
#'
#' @param query A list containing the query data.
#' @throws error If the query structure is invalid.
validate_query <- function(query) {
  if (!is.list(query)) {
    stop(paste0("Expected `query` to be a list, but got ", class(query)))
  }

  required_keys <- c("inputs", "mode", "output_modality")

  missing_keys <- setdiff(required_keys, names(query))
  if (length(missing_keys) > 0) {
    stop(paste0(
      "Missing required keys in query: ", paste(missing_keys, collapse = ", "), ". ",
      "Use `get_valid_query()` to get an example."
    ))
  }
}

#' Validates the modality in the query is allowed for the v1.0 model.
#'
#' @param query A list containing the query data.
#' @throws error If the modality is invalid.
validate_modality <- function(query) {
  allowed_modalities <- unlist(MODEL_MODALITIES$combined$"v1.0")

  modality_key <- "output_modality"
  if (!(modality_key %in% names(query))) {
    stop(paste0("Query requires '", modality_key, "' key."))
  }

  selected_modality <- query[[modality_key]]

  if (!(selected_modality %in% allowed_modalities)) {
    stop(paste0(
      "Invalid modality '", selected_modality, "'. ",
      "Allowed modalities: ", paste(allowed_modalities, collapse = ", ")
    ))
  }
}

#' Transforms raw counts expression data into log1p(CPM).
#'
#' @param expression A data.frame containing raw counts expression data.
#' @return A data.frame containing log1p(CPM) data.
log_cpm <- function(expression) {
  # Convert to numeric and replace NA with 0
  expression_numeric <- as.data.frame(lapply(expression, function(x) {
    as.numeric(as.character(x))
  }))
  expression_numeric[is.na(expression_numeric)] <- 0
  expression_numeric[expression_numeric < 0] <- 0

  # Calculate library size
  library_size <- rowSums(expression_numeric)

  # Initialize CPM matrix
  cpm <- matrix(0, nrow = nrow(expression), ncol = ncol(expression))
  colnames(cpm) <- colnames(expression)
  cpm <- as.data.frame(cpm)

  # Calculate CPM for non-zero libraries
  non_zero_library <- library_size > 0
  if (any(non_zero_library)) {
    for (i in which(non_zero_library)) {
      cpm[i, ] <- expression_numeric[i, ] / library_size[i] * 1e6
    }
  }

  # Log transform
  log_cpm_transformed <- log1p(cpm)

  return(log_cpm_transformed)
}

#' Sends a query to the Synthesize Bio API (combined/v1.0) for prediction and retrieves samples.
#'
#' @param query A list representing the query data to send to the API.
#'        Use `get_valid_query()` to generate an example.
#' @param as_counts Logical, if FALSE, transforms the predicted expression counts into logCPM
#'        (default is TRUE, returning counts).
#' @return A list with two data frames: 'metadata' containing metadata for each sample
#'         and 'expression' containing expression data for each sample.
#' @throws error If the API request fails or the response structure is invalid.
#'
#' @examples
#'
predict_query <- function(query, as_counts = TRUE) {

  if (!exists("SYNTHESIZE_API_KEY", envir = Sys.getenv())) {
    stop("Please set the SYNTHESIZE_API_KEY environment variable")
  }

  api_url <- paste0(API_BASE_URL, "/api/model/combined/v1.0")

  validate_query(query)
  validate_modality(query)

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
    encode = "raw"
  )

  if (http_status(response)$category != "Success") {
    stop(paste0(
      "API request to ", api_url, " failed with status ",
      status_code(response), ": ", content(response, "text")
    ))
  }

  # Parse JSON response
  content <- tryCatch({
    parsed <- fromJSON(content(response, "text", encoding = "UTF-8"))
    if (is.list(parsed) && length(parsed) == 1 && is.list(parsed[[1]])) {
      parsed[[1]]
    } else if (!is.list(parsed)) {
      stop(paste0("API response is not a JSON object: ", content(response, "text")))
    } else {
      parsed
    }
  }, error = function(e) {
    stop(paste0("Failed to decode JSON from API response: ", content(response, "text")))
  })

  # Check for errors in the response
  if ("error" %in% names(content)) {
    stop(paste0("Error in response from API received: ", content$error))
  }
  if ("errors" %in% names(content)) {
    stop(paste0("Error in response from API received: ", content$errors))
  }

  # Process the response data
  if ("outputs" %in% names(content) && "gene_order" %in% names(content)) {
    # Create an empty expression data frame
    expression <- data.frame(matrix(ncol = length(content$gene_order), nrow = 0))
    colnames(expression) <- content$gene_order

    # Create an empty metadata list
    metadata_rows <- list()

    # Process each output
    for (output in content$outputs) {
      # Add expression data
      output_expression <- as.data.frame(output$expression)
      colnames(output_expression) <- content$gene_order
      expression <- rbind(expression, output_expression)

      # Add metadata
      for (i in 1:nrow(output_expression)) {
        metadata_rows <- c(metadata_rows, list(output$metadata))
      }
    }

    # Convert metadata list to data frame
    metadata <- do.call(rbind, lapply(metadata_rows, function(x) {
      as.data.frame(t(unlist(x)), stringsAsFactors = FALSE)
    }))

    # Convert expression to integers
    expression <- as.data.frame(lapply(expression, as.integer))

    # Apply log CPM transformation if requested
    if (!as_counts) {
      expression <- log_cpm(expression)
    }

    return(list(metadata = metadata, expression = expression))
  } else {
    stop(paste0(
      "Unexpected API response structure (expected 'outputs' and 'gene_order'): ",
      toJSON(content)
    ))
  }
}
