test_that("predict_query passes as_counts parameter to extract_expression_data", {
  library(mockery)

  # Define mock_api_response locally
  mock_api_response <- function(success = TRUE, error = NULL, data = NULL) {
    if (success) {
      response <- list(
        status_code = 200,
        headers = list(`content-type` = "application/json"),
        content = charToRaw('{"status":"success"}')
      )
      class(response) <- "response"
      return(response)
    } else {
      response <- list(
        status_code = 400,
        headers = list(`content-type` = "application/json"),
        content = charToRaw('{"error":"Mock API error"}')
      )
      class(response) <- "response"
      return(response)
    }
  }

  # Valid query
  query <- get_valid_query()

  # Create mocks with cycle=TRUE to allow multiple calls
  m_token <- mock(TRUE, cycle = TRUE)
  m_validate_query <- mock(TRUE, cycle = TRUE)
  m_validate_modality <- mock(TRUE, cycle = TRUE)
  m_toJSON <- mock('{"mock_json":"value"}', cycle = TRUE)
  m_POST <- mock(mock_api_response(), cycle = TRUE)
  m_http_status <- mock(list(category = "Success"), cycle = TRUE)

  # We'll create two different JSON responses for the two calls
  api_response_json_counts <- '{
    "classifier_keys": {
      "sex": ["female", "male"],
      "sample_type": ["cell line", "primary tissue"]
    },
    "gene_order": ["gene1", "gene2", "gene3"],
    "model_version": 1,
    "outputs": {
      "classifier_probs": {},
      "expression": [
        [[100, 200, 300], [100, 200, 300], [100, 200, 300], [100, 200, 300], [100, 200, 300]],
        [[100, 200, 300], [100, 200, 300], [100, 200, 300], [100, 200, 300], [100, 200, 300]]
      ],
      "latents": [{}, {}],
      "metadata": {
        "sample_id": ["sample1", "sample2"],
        "cell_line": ["A-549", null],
        "sample_type": ["cell line", "primary tissue"]
      }
    }
  }'

  api_response_json_log <- '{
    "classifier_keys": {
      "sex": ["female", "male"],
      "sample_type": ["cell line", "primary tissue"]
    },
    "gene_order": ["gene1", "gene2", "gene3"],
    "model_version": 1,
    "outputs": {
      "classifier_probs": {},
      "expression": [
        [[100, 200, 300], [100, 200, 300], [100, 200, 300], [100, 200, 300], [100, 200, 300]],
        [[100, 200, 300], [100, 200, 300], [100, 200, 300], [100, 200, 300], [100, 200, 300]]
      ],
      "latents": [{}, {}],
      "metadata": {
        "sample_id": ["sample1", "sample2"],
        "cell_line": ["A-549", null],
        "sample_type": ["cell line", "primary tissue"]
      }
    }
  }'

  # Set up content to return different responses for different calls
  m_content <- mock(api_response_json_counts, api_response_json_log)

  # Create parsed responses for fromJSON
  parsed_response_counts <- list(
    classifier_keys = list(
      sex = c("female", "male"),
      sample_type = c("cell line", "primary tissue")
    ),
    gene_order = c("gene1", "gene2", "gene3"),
    model_version = 1,
    outputs = list(
      classifier_probs = list(),
      expression = list(
        matrix(c(100, 200, 300), nrow=5, ncol=3, byrow=TRUE),
        matrix(c(100, 200, 300), nrow=5, ncol=3, byrow=TRUE)
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

  parsed_response_log <- parsed_response_counts  # Same structure for both calls

  m_fromJSON <- mock(parsed_response_counts, parsed_response_log)

  # Create two responses for extract_expression_data
  counts_result <- list(
    metadata = data.frame(
      sample_id = c("sample1", "sample2"),
      cell_line = c("A-549", NA),
      sample_type = c("cell line", "primary tissue"),
      stringsAsFactors = FALSE
    ),
    expression = data.frame(
      gene1 = c(100, 100),
      row.names = c("sample1", "sample2")
    )
  )

  log_result <- list(
    metadata = data.frame(
      sample_id = c("sample1", "sample2"),
      cell_line = c("A-549", NA),
      sample_type = c("cell line", "primary tissue"),
      stringsAsFactors = FALSE
    ),
    expression = data.frame(
      gene1 = c(log2(100 + 1), log2(100 + 1)),
      row.names = c("sample1", "sample2")
    )
  )

  # Create the extract mock with two different return values
  m_extract <- mock(counts_result, log_result)

  # Stub all the functions
  stub(predict_query, "has_synthesize_token", m_token)
  stub(predict_query, "validate_query", m_validate_query)
  stub(predict_query, "validate_modality", m_validate_modality)
  stub(predict_query, "toJSON", m_toJSON)
  stub(predict_query, "POST", m_POST)
  stub(predict_query, "http_status", m_http_status)
  stub(predict_query, "content", m_content)
  stub(predict_query, "fromJSON", m_fromJSON)
  stub(predict_query, "extract_expression_data", m_extract)

  # Test with default as_counts=TRUE
  result_counts <- predict_query(query)

  # Verify extract_expression_data was called with as_counts=TRUE
  expect_called(m_extract, 1)
  expect_equal(mockery::mock_args(m_extract)[[1]][[2]], TRUE)
  expect_equal(result_counts$expression$gene1[1], 100)

  # Test with as_counts=FALSE
  result_log <- predict_query(query, as_counts = FALSE)

  # Verify extract_expression_data was called with as_counts=FALSE
  expect_called(m_extract, 2)
  expect_equal(mockery::mock_args(m_extract)[[2]][[2]], FALSE)
  expect_equal(result_log$expression$gene1[1], log2(100 + 1))
})

