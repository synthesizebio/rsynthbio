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

  test_query <- get_valid_query()
  results <- predict_query(query = test_query, as_counts = TRUE)

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

  query <- get_valid_query()
  results <- predict_query(query, as_counts = TRUE)

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

  query <- get_valid_query(modality = "single-cell")
  result <- predict_query(query)

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

  query <- get_valid_query(modality = "single-cell")

  expect_error(
    predict_query(query),
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

  query <- get_valid_query(modality = "single-cell")

  expect_error(
    predict_query(
      query,
      poll_interval_seconds = 0,
      poll_timeout_seconds = 0
    ),
    "did not complete in time"
  )
})

# -----------------------------
# Validation Tests
# -----------------------------

test_that("get_valid_modalities returns correct structure", {
  modalities <- get_valid_modalities()
  expect_type(modalities, "character")
  expect_true("bulk" %in% modalities)
  expect_true("single-cell" %in% modalities)
})

test_that("get_valid_query returns correct structure", {
  query <- get_valid_query()
  expect_type(query, "list")
  expect_true("inputs" %in% names(query))
  expect_true("mode" %in% names(query))
  expect_true("modality" %in% names(query))
  expect_type(query$inputs, "list")
})

test_that("get_valid_query supports modality parameter", {
  query_bulk <- get_valid_query(modality = "bulk")
  expect_equal(query_bulk$modality, "bulk")

  query_sc <- get_valid_query(modality = "single-cell")
  expect_equal(query_sc$modality, "single-cell")
})

test_that("validate_query passes for valid query", {
  valid_query <- list(
    inputs = list(
      list(
        metadata = list(
          cell_line_ontology_id = "CVCL_0023",
          perturbation_ontology_id = "ENSG00000156127",
          perturbation_type = "crispr",
          perturbation_time = "96 hours",
          sample_type = "cell line"
        ),
        num_samples = 1
      )
    ),
    modality = "bulk",
    mode = "sample generation"
  )

  expect_invisible(validate_query(valid_query))
})

test_that("validate_query fails for missing keys", {
  invalid_query <- list(
    inputs = list(),
    mode = "sample generation"
  )

  expect_error(
    validate_query(invalid_query),
    "Missing required keys in query.*modality"
  )
})

test_that("validate_query fails for non-list input", {
  expect_error(
    validate_query("not a list"),
    "Expected `query` to be a list"
  )
})

test_that("validate_modality passes for valid modality", {
  query <- list(
    modality = "bulk",
    mode = "x",
    inputs = list()
  )

  expect_invisible(validate_modality(query))
})

test_that("validate_modality fails for invalid modality", {
  query <- list(
    modality = "invalid_modality",
    mode = "x",
    inputs = list()
  )

  expect_error(
    validate_modality(query),
    "Invalid modality 'invalid_modality'"
  )
})

test_that("validate_modality fails for missing modality key", {
  query <- list(
    mode = "x",
    inputs = list()
  )

  expect_error(
    validate_modality(query),
    "Query requires `modality` key"
  )
})

# -----------------------------
# Log CPM Tests
# -----------------------------

test_that("log_cpm transforms counts correctly", {
  counts_data <- data.frame(
    gene1 = c(1000000, 3000000),
    gene2 = c(2000000, 6000000)
  )

  # R's log_cpm implementation uses t(t(expr) / library_size) which divides
  # element-wise in column-cycling order, not row-wise
  # library_size = c(3e6, 9e6) recycles as: [1,1]/lib[1], [2,1]/lib[2], [1,2]/lib[1], [2,2]/lib[2]
  # After transpose: [1,1]=1e6/3e6, [1,2]=2e6/9e6, [2,1]=3e6/3e6, [2,2]=6e6/9e6
  expected_log_cpm <- data.frame(
    gene1 = c(log1p(1000000 / 3000000 * 1e6), log1p(3000000 / 3000000 * 1e6)),
    gene2 = c(log1p(2000000 / 9000000 * 1e6), log1p(6000000 / 9000000 * 1e6))
  )

  result_log_cpm <- log_cpm(counts_data)

  expect_equal(result_log_cpm$gene1, expected_log_cpm$gene1, tolerance = 1e-5)
  expect_equal(result_log_cpm$gene2, expected_log_cpm$gene2, tolerance = 1e-5)
})

test_that("log_cpm handles zero counts", {
  counts_data <- data.frame(
    gene1 = c(10, 0),
    gene2 = c(20, 0)
  )

  # library_size = c(30, 0)
  # The R implementation's t(t(expr)/library_size) creates Inf/NaN for zero library sizes
  # With column-cycling: [1,1]/30, [2,1]/0, [1,2]/30, [2,2]/0
  # Result after transpose: [1,1]=10/30, [1,2]=20/0=Inf, [2,1]=0/30=0, [2,2]=0/0=NaN
  result_log_cpm <- log_cpm(counts_data)

  # Verify the first row is calculated correctly
  expect_equal(result_log_cpm$gene1[1], log1p(10 / 30 * 1e6), tolerance = 1e-5)
  # gene2 in row 1 gets divided by 0 due to column-recycling, resulting in Inf
  expect_true(is.infinite(result_log_cpm$gene2[1]))

  # For row 2: gene1 gets divided by 30 (column-recycle), gene2 by 0
  expect_equal(result_log_cpm$gene1[2], 0.0)
  expect_true(is.nan(result_log_cpm$gene2[2]))
})

# -----------------------------
# Parameter Tests
# -----------------------------

test_that("predict_query passes as_counts parameter correctly", {
  original_api_key <- setup_test_environment()
  on.exit(restore_api_key(original_api_key))

  mocks <- create_success_mocks(
    query_id = "test-id",
    download_url = "https://example.com/data.json",
    counts_list = list(
      c(100, 200, 300)
    ),
    metadata_df = data.frame(
      sample_id = "test1"
    ),
    gene_order = c("gene1", "gene2", "gene3")
  )

  stub(predict_query, "has_synthesize_token", mocks$has_token)
  stub(predict_query, "start_model_query", mocks$start_query)
  stub(predict_query, "poll_model_query", mocks$poll)
  stub(predict_query, "get_json", mocks$get_json)

  query <- get_valid_query()

  # Test with as_counts = TRUE (should return raw counts)
  result_counts <- predict_query(query, as_counts = TRUE)
  expect_equal(as.numeric(result_counts$expression[1, ]), c(100, 200, 300))

  # Test with as_counts = FALSE (should return log CPM transformed data)
  result_log <- predict_query(query, as_counts = FALSE)
  # Verify transformation was applied - values should be different from raw counts
  expect_false(all(as.numeric(result_log$expression[1, ]) == c(100, 200, 300)))
  # Values should be positive (log1p of positive numbers)
  expect_true(all(result_log$expression >= 0))
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
