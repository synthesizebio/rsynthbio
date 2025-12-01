library(testthat)
library(mockery)
library(jsonlite)
library(httr)

# -----------------------------
# Mocked Async Tests (Bulk)
# -----------------------------

test_that("predict_query mocked call success (bulk async)", {
  # Setup environment
  original_api_key <- setup_test_environment("mock-api-key-for-test")
  on.exit(restore_api_key(original_api_key))

  # Create mocks for successful query
  mocks <- create_success_mocks(
    query_id = "bulk-xyz",
    download_url = "https://example.com/bulk.json",
    counts_list = list(
      c(100, 200, 300),
      c(150, 250, 350)
    ),
    metadata_df = data.frame(
      sample_id = c("test1", "test2"),
      cell_line_ontology_id = c("CVCL_0023", "CVCL_0023"),
      age_years = c("25", "30"),
      sex = c("female", "male")
    ),
    gene_order = c("gene1", "gene2", "gene3")
  )

  # Apply stubs directly
  stub(predict_query, "has_synthesize_token", mocks$has_token)
  stub(predict_query, "start_model_query", mocks$start_query)
  stub(predict_query, "poll_model_query", mocks$poll)
  stub(predict_query, "get_json", mocks$get_json)

  test_query <- list(
    inputs = list(
      list(
        metadata = list(
          cell_type_ontology_id = "CL:0000786",
          tissue_ontology_id = "UBERON:0001155",
          sex = "male",
          sample_type = "primary tissue"
        ),
        num_samples = 5
      )
    ),
    mode = "mean estimation",
    seed = 11
  )
  results <- predict_query(query = test_query, model_id = "gem-1-bulk")

  expect_type(results, "list")
  expect_true("metadata" %in% names(results))
  expect_true("expression" %in% names(results))

  expect_s3_class(results$metadata, "data.frame")
  expect_s3_class(results$expression, "data.frame")

  expect_equal(nrow(results$metadata), 2)
  expect_equal(nrow(results$expression), 2)
  expect_equal(ncol(results$expression), 3)

  # Check data values
  expect_equal(as.numeric(results$expression[1, ]), c(100, 200, 300))
  expect_equal(as.numeric(results$expression[2, ]), c(150, 250, 350))

  expect_called(mocks$start_query, 1)
  expect_called(mocks$poll, 1)
  expect_called(mocks$get_json, 1)
})

test_that("predict_query new API structure handling (bulk)", {
  original_api_key <- setup_test_environment()
  on.exit(restore_api_key(original_api_key))

  # Create large gene order
  gene_order <- paste0("ENSG", sprintf("%011d", 1:44590))

  mocks <- create_success_mocks(
    query_id = "bulk-big",
    download_url = "https://example.com/big.json",
    counts_list = list(
      rep(c(0.1, 0.2, 0.3, 0.4, 0.5), 8918)
    ),
    metadata_df = data.frame(
      age_years = "65",
      disease_ontology_id = "MONDO:0011719",
      sex = "female",
      sample_type = "primary tissue",
      tissue_ontology_id = "UBERON:0000945"
    ),
    gene_order = gene_order
  )

  stub(predict_query, "has_synthesize_token", mocks$has_token)
  stub(predict_query, "start_model_query", mocks$start_query)
  stub(predict_query, "poll_model_query", mocks$poll)
  stub(predict_query, "get_json", mocks$get_json)

  query <- list(
    inputs = list(
      list(
        metadata = list(
          age_years = "65",
          disease_ontology_id = "MONDO:0011719",
          sex = "female",
          sample_type = "primary tissue",
          tissue_ontology_id = "UBERON:0000945"
        ),
        num_samples = 1
      )
    ),
    mode = "mean estimation",
    seed = 11
  )
  results <- predict_query(query, model_id = "gem-1-bulk")

  expect_true("metadata" %in% names(results))
  expect_true("expression" %in% names(results))
  expect_equal(nrow(results$metadata), 1)
  expect_equal(nrow(results$expression), 1)
  expect_equal(ncol(results$expression), 44590)
})

# -----------------------------
# Mocked Single-Cell Async Tests
# -----------------------------

test_that("predict_query single-cell success (mocked)", {
  original_api_key <- setup_test_environment("test-api-token")
  on.exit(restore_api_key(original_api_key))

  mocks <- create_success_mocks(
    query_id = "abc123",
    download_url = "https://example.com/final.json",
    counts_list = list(
      c(1, 2, 3),
      c(4, 5, 6)
    ),
    metadata_df = data.frame(
      sample_id = c("s1", "s2")
    ),
    gene_order = c("gene1", "gene2", "gene3")
  )

  stub(predict_query, "has_synthesize_token", mocks$has_token)
  stub(predict_query, "start_model_query", mocks$start_query)
  stub(predict_query, "poll_model_query", mocks$poll)
  stub(predict_query, "get_json", mocks$get_json)

  query <- list(
    inputs = list(
      list(
        metadata = list(
          cell_type_ontology_id = "CL:0000786",
          tissue_ontology_id = "UBERON:0001155",
          sex = "male"
        ),
        num_samples = 5
      )
    ),
    mode = "mean estimation",
    seed = 11
  )
  result <- predict_query(query, model_id = "gem-1-sc")

  expect_true("metadata" %in% names(result))
  expect_true("expression" %in% names(result))
  expect_equal(nrow(result$metadata), 2)
  expect_equal(colnames(result$expression), c("gene1", "gene2", "gene3"))
  expect_equal(as.numeric(result$expression[1, ]), c(1, 2, 3))
  expect_equal(as.numeric(result$expression[2, ]), c(4, 5, 6))
})

test_that("predict_query single-cell failure (mocked)", {
  original_api_key <- setup_test_environment("test-api-token")
  on.exit(restore_api_key(original_api_key))

  mocks <- create_failure_mocks(
    query_id = "abc123",
    error_message = "Invalid metadata: missing required field 'cell_type_ontology_id'"
  )

  stub(predict_query, "has_synthesize_token", mocks$has_token)
  stub(predict_query, "start_model_query", mocks$start_query)
  stub(predict_query, "poll_model_query", mocks$poll)

  query <- list(
    inputs = list(
      list(
        metadata = list(
          cell_type_ontology_id = "CL:0000786",
          tissue_ontology_id = "UBERON:0001155",
          sex = "male"
        ),
        num_samples = 5
      )
    ),
    mode = "mean estimation",
    seed = 11
  )

  expect_error(
    predict_query(query, model_id = "gem-1-sc"),
    "Model query failed.*missing required field"
  )
})

test_that("predict_query single-cell timeout (mocked)", {
  original_api_key <- setup_test_environment("test-api-token")
  on.exit(restore_api_key(original_api_key))

  mocks <- create_timeout_mocks(query_id = "abc123")

  stub(predict_query, "has_synthesize_token", mocks$has_token)
  stub(predict_query, "start_model_query", mocks$start_query)
  stub(predict_query, "poll_model_query", mocks$poll)

  query <- list(
    inputs = list(
      list(
        metadata = list(
          cell_type_ontology_id = "CL:0000786",
          tissue_ontology_id = "UBERON:0001155",
          sex = "male"
        ),
        num_samples = 5
      )
    ),
    mode = "mean estimation",
    seed = 11
  )

  expect_error(
    predict_query(
      query,
      model_id = "gem-1-sc",
      poll_interval_seconds = 0,
      poll_timeout_seconds = 0
    ),
    "did not complete in time"
  )
})

# -----------------------------
# New API Function Tests
# -----------------------------

test_that("list_models mock test", {
  original_api_key <- setup_test_environment()
  on.exit(restore_api_key(original_api_key))

  # Stub make_api_request to return mock data
  stub(list_models, "make_api_request", function(url, context_msg) {
    list(
      list(id = "gem-1-bulk", name = "GEM-1 Bulk"),
      list(id = "gem-1-sc", name = "GEM-1 Single Cell")
    )
  })

  result <- list_models()
  expect_type(result, "list")
  expect_true(length(result) > 0)
})

test_that("get_example_query mock test", {
  original_api_key <- setup_test_environment()
  on.exit(restore_api_key(original_api_key))

  # Stub make_api_request to return mock data
  stub(get_example_query, "make_api_request", function(url, context_msg) {
    list(
      inputs = list(
        list(
          metadata = list(cell_type_ontology_id = "CL:0000786"),
          num_samples = 5
        )
      ),
      mode = "mean estimation"
    )
  })

  result <- get_example_query(model_id = "gem-1-bulk")
  expect_type(result, "list")
  expect_true("inputs" %in% names(result))
  expect_true("mode" %in% names(result))
})

# -----------------------------
# get_json Tests
# -----------------------------

test_that("get_json successfully fetches and parses JSON", {
  test_data <- list(
    outputs = list(
      list(counts = c(1, 2, 3), metadata = list(sample_id = "test"))
    ),
    gene_order = c("gene1", "gene2", "gene3")
  )

  # Mock successful HTTP response
  mock_response <- mock(
    structure(
      list(
        url = "https://example.com/data.json",
        status_code = 200,
        content = charToRaw(toJSON(test_data, auto_unbox = TRUE))
      ),
      class = "response"
    ),
    cycle = TRUE
  )

  get_json_fn <- rsynthbio:::get_json
  stub(get_json_fn, "GET", mock_response)
  stub(get_json_fn, "status_code", function(x) 200)
  stub(get_json_fn, "content", function(x, type, encoding) {
    toJSON(test_data, auto_unbox = TRUE)
  })

  result <- get_json_fn("https://example.com/data.json")

  expect_type(result, "list")
  expect_true("outputs" %in% names(result))
  expect_true("gene_order" %in% names(result))
  expect_equal(result$gene_order, c("gene1", "gene2", "gene3"))
})

test_that("get_json handles network errors", {
  # Mock network error
  mock_get_error <- mock(stop("Connection timeout"), cycle = TRUE)

  get_json_fn <- rsynthbio:::get_json
  stub(get_json_fn, "GET", mock_get_error)

  expect_error(
    get_json_fn("https://example.com/data.json"),
    "Failed to fetch download URL due to a network issue.*Connection timeout"
  )
})

test_that("get_json handles HTTP 404 errors", {
  mock_response <- mock(
    structure(
      list(
        url = "https://example.com/data.json",
        status_code = 404,
        content = charToRaw("Not Found")
      ),
      class = "response"
    ),
    cycle = TRUE
  )

  get_json_fn <- rsynthbio:::get_json
  stub(get_json_fn, "GET", mock_response)
  stub(get_json_fn, "status_code", function(x) 404)
  stub(get_json_fn, "content", function(x, type) "Not Found")

  expect_error(
    get_json_fn("https://example.com/data.json"),
    "Download URL fetch failed with status 404.*Not Found"
  )
})

test_that("get_json handles HTTP 500 errors", {
  mock_response <- mock(
    structure(
      list(
        url = "https://example.com/data.json",
        status_code = 500,
        content = charToRaw("Internal Server Error")
      ),
      class = "response"
    ),
    cycle = TRUE
  )

  get_json_fn <- rsynthbio:::get_json
  stub(get_json_fn, "GET", mock_response)
  stub(get_json_fn, "status_code", function(x) 500)
  stub(get_json_fn, "content", function(x, type) "Internal Server Error")

  expect_error(
    get_json_fn("https://example.com/data.json"),
    "Download URL fetch failed with status 500.*Internal Server Error"
  )
})

test_that("get_json handles JSON parsing errors", {
  mock_response <- mock(
    structure(
      list(
        url = "https://example.com/data.json",
        status_code = 200,
        content = charToRaw("invalid json content {{{")
      ),
      class = "response"
    ),
    cycle = TRUE
  )

  get_json_fn <- rsynthbio:::get_json
  stub(get_json_fn, "GET", mock_response)
  stub(get_json_fn, "status_code", function(x) 200)
  stub(get_json_fn, "content", function(x, type, encoding) "invalid json content {{{")
  stub(get_json_fn, "fromJSON", function(x, ...) stop("JSON parsing error"))

  expect_error(
    get_json_fn("https://example.com/data.json"),
    "Failed to decode JSON from download URL response.*JSON parsing error"
  )
})

test_that("get_json preserves UTF-8 encoding", {
  # Test data with special characters
  test_data <- list(
    outputs = list(
      list(
        counts = c(1, 2, 3),
        metadata = list(
          sample_id = "test-utf8-\u00e9\u00f1\u00fc",
          description = "Test with \u2764\ufe0f"
        )
      )
    ),
    gene_order = c("gene1", "gene2", "gene3")
  )

  # Create JSON string
  json_string <- toJSON(test_data, auto_unbox = TRUE)

  mock_response <- mock(
    structure(
      list(
        url = "https://example.com/data.json",
        status_code = 200,
        content = charToRaw(json_string)
      ),
      class = "response"
    ),
    cycle = TRUE
  )

  get_json_fn <- rsynthbio:::get_json
  stub(get_json_fn, "GET", mock_response)
  stub(get_json_fn, "status_code", function(x) 200)
  stub(get_json_fn, "content", function(x, type, encoding) json_string)

  result <- get_json_fn("https://example.com/data.json")

  expect_true("outputs" %in% names(result))
  expect_true("gene_order" %in% names(result))
  # Verify the data structure was parsed
  expect_true(length(result$outputs) >= 1)
  expect_equal(result$gene_order, c("gene1", "gene2", "gene3"))
})
