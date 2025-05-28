
test_that("predict_query passes as_counts parameter to extract_expression_data", {
  library(mockery)
  library(jsonlite) # Ensure jsonlite is available
  library(httr)     # Ensure httr is available for http_status and content

  # --- Helper Functions ---
  mock_api_response <- function(success = TRUE, data = NULL, error = NULL) {
    status_code <- ifelse(success, 200, 400)
    content <- if (success && !is.null(data)) {
      data  # Return the data directly, not wrapped in a list with 'data' element
    } else if (!is.null(error)) {
      list(error = error)
    } else {
      list(status = "success")
    }
    response <- list(
      status_code = status_code,
      headers = list(`content-type` = "application/json"),
      content = content
    )
    class(response) <- "response"
    return(response)
  }

  parsed_api_response <- list(
    classifier_keys = list(sex = c("female", "male"), sample_type = c("cell line", "primary tissue")),
    gene_order = c("gene1", "gene2", "gene3"),
    model_version = 2,
    outputs = list(
      classifier_probs = list(),
      expression = list(
        matrix(rep(c(100, 200, 300), each = 5), nrow = 5),
        matrix(rep(c(100, 200, 300), each = 5), nrow = 5)
      ),
      latents = list(list(), list()),
      metadata = data.frame(
        sample_id = c("sample1", "sample2"),
        cell_line = c("A-549", NA),
        sample_type = c("cell line", "primary tissue"),
        stringsAsFactors = FALSE
      )
    )
  )

  extracted_data_counts <- list(
    metadata = parsed_api_response$outputs$metadata,
    expression = data.frame(gene1 = c(100, 100), row.names = c("sample1", "sample2"))
  )

  extracted_data_log <- list(
    metadata = parsed_api_response$outputs$metadata,
    expression = data.frame(gene1 = log2(c(100, 100) + 1), row.names = c("sample1", "sample2"))
  )

  # --- Mock Setup ---
  m_token <- mock(TRUE, cycle = TRUE)  # Set cycle = TRUE
  m_validate_query <- mock(TRUE, cycle = TRUE)
  m_validate_modality <- mock(TRUE, cycle = TRUE)
  m_toJSON <- mock('{"mock_json":"value"}', cycle = TRUE)

  # Create a proper httr-like response with raw JSON content
  mock_json_text <- jsonlite::toJSON(list(data = parsed_api_response), auto_unbox = TRUE)
  mock_response <- list(
    status_code = 200,
    headers = list(`content-type` = "application/json"),
    content = charToRaw(mock_json_text)
  )
  class(mock_response) <- "response"
  m_POST <- mock(mock_response, cycle = TRUE)

  m_http_status <- mock(list(category = "Success"), cycle = TRUE)
  m_status_code <- mock(200, cycle = TRUE)

  # For extract_expression_data, we still want to track different calls with different parameters
  # so don't cycle this one
  m_extract <- mock(extracted_data_counts, extracted_data_log)

  stub(predict_query, "has_synthesize_token", m_token)
  stub(predict_query, "validate_query", m_validate_query)
  stub(predict_query, "validate_modality", m_validate_modality)
  stub(predict_query, "toJSON", m_toJSON)
  stub(predict_query, "POST", m_POST)
  stub(predict_query, "http_status", m_http_status)
  stub(predict_query, "status_code", m_status_code)

  # When content() is called with "text", return the raw JSON string
  stub(predict_query, "content", function(response, as = NULL, encoding = NULL, ...) {
    if (as == "text") {
      return(mock_json_text)
    } else {
      return(response$content)
    }
  })

  # Use the real fromJSON to parse our mock JSON
  stub(predict_query, "fromJSON", function(txt, ...) {
    result <- jsonlite::fromJSON(txt, ...)
    return(result)
  })

  stub(predict_query, "extract_expression_data", m_extract)

  # --- Test Cases ---
  query <- get_valid_query()

  # Test with default as_counts=TRUE
  result_counts <- predict_query(query)
  expect_called(m_extract, 1)
  expect_equal(mockery::mock_args(m_extract)[[1]][[2]], TRUE)
  expect_equal(result_counts$expression$gene1[1], 100)

  # Test with as_counts=FALSE
  result_log <- predict_query(query, as_counts = FALSE)
  expect_called(m_extract, 2)
  expect_equal(mockery::mock_args(m_extract)[[2]][[2]], FALSE)
  expect_equal(result_log$expression$gene1[1], log2(100 + 1))
})


test_that("predict_query calls the API and returns data when API key is set", {
  skip_if_not(!is.na(Sys.getenv("SYNTHESIZE_API_KEY")) && Sys.getenv("SYNTHESIZE_API_KEY") != "",
              "Skipping API call test because SYNTHESIZE_API_KEY is not set.")

  # Assume get_valid_query() returns a valid query structure
  query <- get_valid_query()

  # Call the predict_query function (this will hit the actual API)
  result <- predict_query(query)

  # Perform assertions on the result
  # This will depend on the expected structure of the API response
  expect_true(is.list(result), info = "API response should be a list.")
  expect_true(!is.null(result$metadata), info = "API response should contain metadata.")
  expect_true(is.data.frame(result$metadata), info = "Metadata should be a data frame.")
  expect_true(!is.null(result$expression), info = "API response should contain expression data.")
  expect_true(is.data.frame(result$expression), info = "Expression data should be a data frame.")
  expect_true(nrow(result$metadata) > 0, info = "Metadata should have at least one row.")
  expect_true(ncol(result$expression) > 0, info = "Expression data should have at least one column (gene).")

})

