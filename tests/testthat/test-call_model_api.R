context("Synthesize Bio Gene Expression Prediction")

# Mock data that would normally be defined elsewhere in the package
MODEL_MODALITIES <- list(
  combined = list(
    "v1.0" = list("bulk_rna-seq", "sra", "sc_10x_gene", "sc_10x_feature")
  )
)

API_BASE_URL <- "https://api.synthesize.bio"

# Helper function to mock API response
mock_api_response <- function(success = TRUE, error = NULL, data = NULL) {
  if (success) {
    if (is.null(data)) {
      # Create a default successful response with mock data
      data <- list(
        metadata = list(
          list(
            sample_id = "sample1",
            cell_line = "A-549",
            perturbation = "ABL1",
            perturbation_type = "crispr",
            perturbation_time = "96 hours",
            sample_type = "cell line"
          ),
          list(
            sample_id = "sample2",
            disease = "gastrointestinal stromal tumor",
            age = "65 years",
            sex = "female",
            sample_type = "primary tissue",
            tissue = "stomach"
          )
        ),
        expression = list(
          "sample1" = setNames(runif(10, 0, 1000), paste0("gene", 1:10)),
          "sample2" = setNames(runif(10, 0, 1000), paste0("gene", 1:10))
        )
      )
    }
    response <- list(
      status_code = 200,
      headers = list(`content-type` = "application/json"),
      content = charToRaw(jsonlite::toJSON(data, auto_unbox = TRUE))
    )
    class(response) <- "response"
    return(response)
  } else {
    # Error response
    error_data <- list(error = ifelse(is.null(error), "Mock API error", error))
    response <- list(
      status_code = 400,
      headers = list(`content-type` = "application/json"),
      content = charToRaw(jsonlite::toJSON(error_data, auto_unbox = TRUE))
    )
    class(response) <- "response"
    return(response)
  }
}

# Mock for http_status
mock_http_status <- function(response) {
  if (response$status_code >= 200 && response$status_code < 300) {
    return(list(category = "Success"))
  } else {
    return(list(category = "Client error"))
  }
}

# Mock for content extraction
mock_content <- function(response, as = "text", encoding = "UTF-8") {
  if (as == "text") {
    return(rawToChar(response$content))
  } else {
    return(response$content)
  }
}

# Mock for extract_expression_data
mock_extract_expression_data <- function(parsed_content, as_counts = TRUE) {
  if (!is.list(parsed_content) || !all(c("metadata", "expression") %in% names(parsed_content))) {
    stop("Invalid response structure")
  }

  # Create metadata dataframe
  metadata_df <- do.call(rbind, lapply(parsed_content$metadata, function(m) {
    as.data.frame(m, stringsAsFactors = FALSE)
  }))

  # Create expression matrix
  genes <- unique(unlist(lapply(parsed_content$expression, names)))
  samples <- names(parsed_content$expression)
  expr_matrix <- matrix(0, nrow = length(samples), ncol = length(genes))
  rownames(expr_matrix) <- samples
  colnames(expr_matrix) <- genes

  for (s in samples) {
    sample_expr <- parsed_content$expression[[s]]
    expr_matrix[s, names(sample_expr)] <- unlist(sample_expr)
  }

  # Transform if needed
  if (!as_counts) {
    # Mock logCPM transformation
    expr_matrix <- log2(expr_matrix + 1)
  }

  return(list(
    metadata = metadata_df,
    expression = as.data.frame(expr_matrix)
  ))
}

test_that("get_valid_modalities returns expected modalities", {
  modalities <- get_valid_modalities()

  # Check the modalities match our mock data
  expect_equal(modalities, unlist(MODEL_MODALITIES$combined$"v1.0"))
  expect_true("bulk_rna-seq" %in% modalities)
  expect_true("sra" %in% modalities)
  expect_true("sc_10x_gene" %in% modalities)
  expect_true("sc_10x_feature" %in% modalities)
})

test_that("get_valid_query returns proper structure", {
  query <- get_valid_query()

  # Check the query structure
  expect_true(is.list(query))
  expect_true("output_modality" %in% names(query))
  expect_true("mode" %in% names(query))
  expect_true("inputs" %in% names(query))
  expect_true(is.list(query$inputs))
  expect_equal(length(query$inputs), 2)

  # Check first input
  first_input <- query$inputs[[1]]
  expect_true(is.list(first_input$metadata))
  expect_true("cell_line" %in% names(first_input$metadata))
  expect_true("perturbation" %in% names(first_input$metadata))
  expect_equal(first_input$num_samples, 5)

  # Check second input
  second_input <- query$inputs[[2]]
  expect_true(is.list(second_input$metadata))
  expect_true("disease" %in% names(second_input$metadata))
  expect_true("tissue" %in% names(second_input$metadata))
  expect_equal(second_input$num_samples, 5)
})

test_that("validate_query accepts valid queries", {
  # Test with a valid query
  query <- get_valid_query()
  expect_silent(validate_query(query))

  # Test with minimal valid query
  minimal_query <- list(
    inputs = list(list(metadata = list(cell_line = "HEK293"))),
    mode = "mean estimation",
    output_modality = "bulk_rna-seq"
  )
  expect_silent(validate_query(minimal_query))
})

test_that("validate_query rejects invalid queries", {
  # Test with non-list
  expect_error(validate_query("not a list"), "Expected `query` to be a list")

  # Test with missing keys
  query_missing_inputs <- list(mode = "mean estimation", output_modality = "bulk_rna-seq")
  expect_error(validate_query(query_missing_inputs), "Missing required keys in query: inputs")

  query_missing_mode <- list(inputs = list(), output_modality = "bulk_rna-seq")
  expect_error(validate_query(query_missing_mode), "Missing required keys in query: mode")

  query_missing_modality <- list(inputs = list(), mode = "mean estimation")
  expect_error(validate_query(query_missing_modality), "Missing required keys in query: output_modality")

  # Multiple missing keys
  query_multiple_missing <- list(inputs = list())
  expect_error(
    validate_query(query_multiple_missing),
    "Missing required keys in query: mode, output_modality"
  )
})

test_that("validate_modality accepts valid modalities", {
  # Test with all valid modalities
  valid_modalities <- unlist(MODEL_MODALITIES$combined$"v1.0")
  for (modality in valid_modalities) {
    query <- list(
      inputs = list(),
      mode = "mean estimation",
      output_modality = modality
    )
    expect_silent(validate_modality(query))
  }
})

test_that("validate_modality rejects invalid modalities", {
  # Test with missing modality key
  query_missing_modality <- list(inputs = list(), mode = "mean estimation")
  expect_error(validate_modality(query_missing_modality), "Query requires 'output_modality' key")

  # Test with invalid modality
  query_invalid_modality <- list(
    inputs = list(),
    mode = "mean estimation",
    output_modality = "invalid_modality"
  )
  expect_error(
    validate_modality(query_invalid_modality),
    "Invalid modality 'invalid_modality'"
  )

  # Error message should list allowed modalities
  expect_error(
    validate_modality(query_invalid_modality),
    paste("Allowed modalities:", paste(unlist(MODEL_MODALITIES$combined$"v1.0"), collapse = ", "))
  )
})

test_that("predict_query validates input query", {
  skip_if_not_installed("mockery")
  library(mockery)

  # Test with invalid query
  query_invalid <- list(mode = "mean estimation")

  # Mock has_synthesize_token to return TRUE
  m_token <- mock(TRUE)
  stub(predict_query, "has_synthesize_token", m_token)

  expect_error(predict_query(query_invalid), "Missing required keys in query: inputs, output_modality")
})

test_that("predict_query checks for API token", {
  skip_if_not_installed("mockery")
  library(mockery)

  # Mock has_synthesize_token to return FALSE
  m_token <- mock(FALSE)
  stub(predict_query, "has_synthesize_token", m_token)

  query <- get_valid_query()
  expect_error(predict_query(query), "Please set your API key")
})

test_that("predict_query handles API success correctly", {
  skip_if_not_installed("mockery")
  library(mockery)

  # Valid query
  query <- get_valid_query()

  # Mock required functions
  m_token <- mock(TRUE)
  stub(predict_query, "has_synthesize_token", m_token)

  m_validate_query <- mock(TRUE)
  stub(predict_query, "validate_query", m_validate_query)

  m_validate_modality <- mock(TRUE)
  stub(predict_query, "validate_modality", m_validate_modality)

  m_toJSON <- mock('{"mock_json":"value"}')
  stub(predict_query, "toJSON", m_toJSON)

  # Mock successful API response
  m_POST <- mock(mock_api_response())
  stub(predict_query, "POST", m_POST)

  m_http_status <- mock(list(category = "Success"))
  stub(predict_query, "http_status", m_http_status)

  m_content <- mock('{"metadata":[{"sample_id":"sample1"}],"expression":{"sample1":{"gene1":100}}}')
  stub(predict_query, "content", m_content)

  m_fromJSON <- mock(list(
    metadata = list(list(sample_id = "sample1")),
    expression = list(sample1 = list(gene1 = 100))
  ))
  stub(predict_query, "fromJSON", m_fromJSON)

  m_extract <- mock(list(
    metadata = data.frame(sample_id = "sample1"),
    expression = data.frame(gene1 = 100, row.names = "sample1")
  ))
  stub(predict_query, "extract_expression_data", m_extract)

  # Test the function
  result <- predict_query(query)

  # Verify result structure
  expect_true(is.list(result))
  expect_true("metadata" %in% names(result))
  expect_true("expression" %in% names(result))

  # Verify mocks were called
  expect_called(m_token, 1)
  expect_called(m_validate_query, 1)
  expect_called(m_validate_modality, 1)
  expect_called(m_POST, 1)
  expect_called(m_http_status, 1)
  expect_called(m_content, 1)
  expect_called(m_fromJSON, 1)
  expect_called(m_extract, 1)
})

test_that("predict_query handles API errors correctly", {
  skip_if_not_installed("mockery")
  library(mockery)

  # Valid query
  query <- get_valid_query()

  # Mock required functions
  m_token <- mock(TRUE)
  stub(predict_query, "has_synthesize_token", m_token)

  m_validate_query <- mock(TRUE)
  stub(predict_query, "validate_query", m_validate_query)

  m_validate_modality <- mock(TRUE)
  stub(predict_query, "validate_modality", m_validate_modality)

  m_toJSON <- mock('{"mock_json":"value"}')
  stub(predict_query, "toJSON", m_toJSON)

  # Mock failed API response
  m_POST <- mock(mock_api_response(success = FALSE))
  stub(predict_query, "POST", m_POST)

  m_http_status <- mock(list(category = "Client error"))
  stub(predict_query, "http_status", m_http_status)

  m_content <- mock('{"error":"API error message"}')
  stub(predict_query, "content", m_content)

  # Test the function should error
  expect_error(predict_query(query), "API request to .* failed with status")
})

test_that("predict_query handles API JSON error response", {
  skip_if_not_installed("mockery")
  library(mockery)

  # Valid query
  query <- get_valid_query()

  # Mock required functions
  m_token <- mock(TRUE)
  stub(predict_query, "has_synthesize_token", m_token)

  m_validate_query <- mock(TRUE)
  stub(predict_query, "validate_query", m_validate_query)

  m_validate_modality <- mock(TRUE)
  stub(predict_query, "validate_modality", m_validate_modality)

  m_toJSON <- mock('{"mock_json":"value"}')
  stub(predict_query, "toJSON", m_toJSON)

  # Mock successful API response but with error in content
  m_POST <- mock(mock_api_response())
  stub(predict_query, "POST", m_POST)

  m_http_status <- mock(list(category = "Success"))
  stub(predict_query, "http_status", m_http_status)

  m_content <- mock('{"error":"API error in content"}')
  stub(predict_query, "content", m_content)

  m_fromJSON <- mock(list(error = "API error in content"))
  stub(predict_query, "fromJSON", m_fromJSON)

  # Test the function should error
  expect_error(predict_query(query), "API error: API error in content")
})

test_that("predict_query handles API JSON errors array response", {
  skip_if_not_installed("mockery")
  library(mockery)

  # Valid query
  query <- get_valid_query()

  # Mock required functions
  m_token <- mock(TRUE)
  stub(predict_query, "has_synthesize_token", m_token)

  m_validate_query <- mock(TRUE)
  stub(predict_query, "validate_query", m_validate_query)

  m_validate_modality <- mock(TRUE)
  stub(predict_query, "validate_modality", m_validate_modality)

  m_toJSON <- mock('{"mock_json":"value"}')
  stub(predict_query, "toJSON", m_toJSON)

  # Mock successful API response but with errors array in content
  m_POST <- mock(mock_api_response())
  stub(predict_query, "POST", m_POST)

  m_http_status <- mock(list(category = "Success"))
  stub(predict_query, "http_status", m_http_status)

  m_content <- mock('{"errors":["Error 1", "Error 2"]}')
  stub(predict_query, "content", m_content)

  m_fromJSON <- mock(list(errors = c("Error 1", "Error 2")))
  stub(predict_query, "fromJSON", m_fromJSON)

  # Test the function should error
  expect_error(predict_query(query), "API errors: Error 1; Error 2")
})

test_that("predict_query returns raw response when requested", {
  skip_if_not_installed("mockery")
  library(mockery)

  # Valid query
  query <- get_valid_query()

  # Mock required functions
  m_token <- mock(TRUE)
  stub(predict_query, "has_synthesize_token", m_token)

  m_validate_query <- mock(TRUE)
  stub(predict_query, "validate_query", m_validate_query)

  m_validate_modality <- mock(TRUE)
  stub(predict_query, "validate_modality", m_validate_modality)

  m_toJSON <- mock('{"mock_json":"value"}')
  stub(predict_query, "toJSON", m_toJSON)

  # Mock successful API response
  m_POST <- mock(mock_api_response())
  stub(predict_query, "POST", m_POST)

  m_http_status <- mock(list(category = "Success"))
  stub(predict_query, "http_status", m_http_status)

  m_content <- mock('{"raw_data":"value"}')
  stub(predict_query, "content", m_content)

  raw_response_data <- list(raw_data = "value")
  m_fromJSON <- mock(raw_response_data)
  stub(predict_query, "fromJSON", m_fromJSON)

  # Test with raw_response=TRUE
  result <- predict_query(query, raw_response = TRUE)

  # Should return the raw response without extraction
  expect_equal(result, raw_response_data)
})

test_that("predict_query passes as_counts parameter to extract_expression_data", {
  skip_if_not_installed("mockery")
  library(mockery)

  # Valid query
  query <- get_valid_query()

  # Mock required functions
  m_token <- mock(TRUE)
  stub(predict_query, "has_synthesize_token", m_token)

  m_validate_query <- mock(TRUE)
  stub(predict_query, "validate_query", m_validate_query)

  m_validate_modality <- mock(TRUE)
  stub(predict_query, "validate_modality", m_validate_modality)

  m_toJSON <- mock('{"mock_json":"value"}')
  stub(predict_query, "toJSON", m_toJSON)

  # Mock successful API response
  m_POST <- mock(mock_api_response())
  stub(predict_query, "POST", m_POST)

  m_http_status <- mock(list(category = "Success"))
  stub(predict_query, "http_status", m_http_status)

  m_content <- mock('{"metadata":[{"sample_id":"sample1"}],"expression":{"sample1":{"gene1":100}}}')
  stub(predict_query, "content", m_content)

  response_data <- list(
    metadata = list(list(sample_id = "sample1")),
    expression = list(sample1 = list(gene1 = 100))
  )
  m_fromJSON <- mock(response_data)
  stub(predict_query, "fromJSON", m_fromJSON)

  # Create a mock that checks the as_counts parameter
  m_extract <- mock(function(parsed_content, as_counts) {
    # Return different values based on as_counts
    if (as_counts) {
      return(list(
        metadata = data.frame(sample_id = "sample1"),
        expression = data.frame(gene1 = 100, row.names = "sample1")
      ))
    } else {
      return(list(
        metadata = data.frame(sample_id = "sample1"),
        expression = data.frame(gene1 = log2(100 + 1), row.names = "sample1")
      ))
    }
  })
  stub(predict_query, "extract_expression_data", m_extract)

  # Test with default as_counts=TRUE
  result_counts <- predict_query(query)
  expect_equal(result_counts$expression$gene1, 100)

  # Test with as_counts=FALSE
  result_log <- predict_query(query, as_counts = FALSE)
  expect_equal(result_log$expression$gene1, log2(100 + 1))
})

# Integration test with mocked API
test_that("Functions work together in a full workflow", {
  skip("This test is for manual verification only")

  # This simulates a full API workflow
  # We'd mock out the API call but use real functions for everything else

  # 1. Get valid modalities
  modalities <- get_valid_modalities()
  expect_true(length(modalities) > 0)

  # 2. Get a valid query
  query <- get_valid_query()

  # 3. Validate the query
  validate_query(query)
  validate_modality(query)

})
