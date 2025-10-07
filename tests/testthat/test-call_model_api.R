library(testthat)
library(mockery)
library(jsonlite)
library(httr)

# Helper function to check if API key is available
api_key_available <- function() {
  !is.na(Sys.getenv("SYNTHESIZE_API_KEY")) &&
    Sys.getenv("SYNTHESIZE_API_KEY") != ""
}

# -----------------------------
# Live API Tests
# -----------------------------

test_that("predict_query live call success (bulk)", {
  skip_if_not(
    api_key_available(),
    "Skipping live API test because SYNTHESIZE_API_KEY is not set."
  )

  message("\nTesting live predict_query call for bulk modality...")

  test_query <- get_valid_query()

  results <- predict_query(
    query = test_query,
    as_counts = TRUE,
  )

  expect_type(results, "list")
  expect_true("metadata" %in% names(results))
  expect_true("expression" %in% names(results))

  expect_s3_class(results$metadata, "data.frame")
  expect_s3_class(results$expression, "data.frame")

  expect_true(nrow(results$metadata) > 0)
  expect_true(nrow(results$expression) > 0)
  expect_true(ncol(results$expression) > 0)

  message("Live bulk API test passed.")
})

test_that("predict_query live call success (single-cell)", {
  skip_if_not(
    api_key_available(),
    "Skipping live API test because SYNTHESIZE_API_KEY is not set."
  )

  message("\nTesting live predict_query call for single-cell modality...")

  test_query <- get_valid_query(modality = "czi")

  results <- predict_query(
    query = test_query,
    as_counts = TRUE,
  )

  expect_type(results, "list")
  expect_true("metadata" %in% names(results))
  expect_true("expression" %in% names(results))

  expect_s3_class(results$metadata, "data.frame")
  expect_s3_class(results$expression, "data.frame")

  expect_true(nrow(results$metadata) > 0)
  expect_true(nrow(results$expression) > 0)
  expect_true(ncol(results$expression) > 0)

  message("Live single-cell API test passed.")
})

test_that("predict_query live call invalid UBERON (bulk)", {
  skip_if_not(
    api_key_available(),
    "Skipping live API test because SYNTHESIZE_API_KEY is not set."
  )

  message("\nTesting live predict_query with invalid UBERON ID for bulk...")

  # Create a query with an invalid UBERON ID
  invalid_query <- list(
    inputs = list(
      list(
        metadata = list(
          tissue_ontology_id = "UBERON:9999999", # Invalid ID
          age_years = "65",
          sex = "female",
          sample_type = "primary tissue"
        ),
        num_samples = 1
      )
    ),
    modality = "bulk",
    mode = "sample generation"
  )

  # The API should reject this with an error
  expect_error(
    predict_query(
      query = invalid_query,
      as_counts = TRUE
    ),
    "Model query failed"
  )

  # Verify the error contains validation details
  error_result <- tryCatch(
    predict_query(query = invalid_query, as_counts = TRUE),
    error = function(e) e$message
  )

  message(paste("API correctly rejected invalid UBERON ID with error:", error_result))

  # The error message should contain the validation details
  expect_true(
    grepl("UBERON:9999999", error_result),
    info = paste("Error message should mention the invalid UBERON ID. Got:", error_result)
  )
  expect_true(
    grepl("bad values|invalid", error_result, ignore.case = TRUE),
    info = paste("Error message should indicate validation failure. Got:", error_result)
  )

  message("Successfully validated error message contains UBERON validation details")
})

test_that("predict_query live call invalid UBERON (single-cell)", {
  skip_if_not(
    api_key_available(),
    "Skipping live API test because SYNTHESIZE_API_KEY is not set."
  )

  message("\nTesting live predict_query (single-cell) with invalid UBERON ID...")

  # Create a single-cell query with an invalid UBERON ID
  invalid_query <- list(
    inputs = list(
      list(
        metadata = list(
          cell_type_ontology_id = "CL:0000786",
          tissue_ontology_id = "UBERON:9999999", # Invalid ID
          sex = "male"
        ),
        num_samples = 1
      )
    ),
    modality = "czi",
    mode = "sample generation",
    return_classifier_probs = TRUE,
    seed = 42
  )

  # The API should reject this with an error
  expect_error(
    predict_query(
      query = invalid_query,
      as_counts = TRUE
    ),
    "Model query failed"
  )

  # Verify the error contains validation details
  error_result <- tryCatch(
    predict_query(query = invalid_query, as_counts = TRUE),
    error = function(e) e$message
  )

  message(paste("API correctly rejected invalid UBERON ID (single-cell) with error:", error_result))

  # The error message should contain the validation details
  expect_true(
    grepl("UBERON:9999999", error_result),
    info = paste("Error message should mention the invalid UBERON ID. Got:", error_result)
  )
  expect_true(
    grepl("bad values|invalid", error_result, ignore.case = TRUE),
    info = paste("Error message should indicate validation failure. Got:", error_result)
  )

  message("Successfully validated error message contains UBERON validation details (single-cell)")
})

# -----------------------------
# Mocked Async Tests (Bulk)
# -----------------------------

test_that("predict_query mocked call success (bulk async)", {
  original_api_key <- Sys.getenv("SYNTHESIZE_API_KEY")
  on.exit({
    if (original_api_key != "") {
      Sys.setenv(SYNTHESIZE_API_KEY = original_api_key)
    } else {
      Sys.unsetenv("SYNTHESIZE_API_KEY")
    }
  })

  Sys.setenv(SYNTHESIZE_API_KEY = "mock-api-key-for-test")

  # Mock has_synthesize_token
  m_has_token <- mock(TRUE, cycle = TRUE)

  # Mock start_model_query to return modelQueryId
  m_start_query <- mock("bulk-xyz", cycle = TRUE)

  # Mock poll_model_query to return ready status with downloadUrl
  m_poll <- mock(
    list(
      status = "ready",
      payload = list(
        status = "ready",
        downloadUrl = "https://example.com/bulk.json"
      )
    ),
    cycle = TRUE
  )

  # Mock get_json to return final results
  m_get_json <- mock(
    list(
      outputs = list(
        list(
          counts = c(100, 200, 300),
          metadata = list(
            sample_id = "test1",
            cell_line_ontology_id = "CVCL_0023",
            age_years = "25",
            sex = "female"
          )
        ),
        list(
          counts = c(150, 250, 350),
          metadata = list(
            sample_id = "test2",
            cell_line_ontology_id = "CVCL_0023",
            age_years = "30",
            sex = "male"
          )
        )
      ),
      gene_order = c("gene1", "gene2", "gene3"),
      model_version = 2
    ),
    cycle = TRUE
  )

  # Apply stubs
  stub(predict_query, "has_synthesize_token", m_has_token)
  stub(predict_query, "start_model_query", m_start_query)
  stub(predict_query, "poll_model_query", m_poll)
  stub(predict_query, "get_json", m_get_json)

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

  expect_called(m_start_query, 1)
  expect_called(m_poll, 1)
  expect_called(m_get_json, 1)
})

test_that("predict_query new API structure handling (bulk)", {
  original_api_key <- Sys.getenv("SYNTHESIZE_API_KEY")
  on.exit({
    if (original_api_key != "") {
      Sys.setenv(SYNTHESIZE_API_KEY = original_api_key)
    } else {
      Sys.unsetenv("SYNTHESIZE_API_KEY")
    }
  })

  Sys.setenv(SYNTHESIZE_API_KEY = "mock-api-key-for-test")

  m_has_token <- mock(TRUE, cycle = TRUE)
  m_start_query <- mock("bulk-big", cycle = TRUE)

  m_poll <- mock(
    list(
      status = "ready",
      payload = list(
        status = "ready",
        downloadUrl = "https://example.com/big.json"
      )
    ),
    cycle = TRUE
  )

  # Create large gene order
  gene_order <- paste0("ENSG", sprintf("%011d", 1:44590))

  m_get_json <- mock(
    list(
      outputs = list(
        list(
          counts = rep(c(0.1, 0.2, 0.3, 0.4, 0.5), 8918),
          classifier_probs = list(
            sex = list(female = 0.7, male = 0.3),
            age_years = list(`60-70` = 0.8, `70-80` = 0.2),
            tissue_ontology_id = list(UBERON_0000945 = 0.9)
          ),
          metadata = list(
            age_years = "65",
            disease_ontology_id = "MONDO:0011719",
            sex = "female",
            sample_type = "primary tissue",
            tissue_ontology_id = "UBERON:0000945"
          )
        )
      ),
      gene_order = gene_order,
      model_version = 2
    ),
    cycle = TRUE
  )

  stub(predict_query, "has_synthesize_token", m_has_token)
  stub(predict_query, "start_model_query", m_start_query)
  stub(predict_query, "poll_model_query", m_poll)
  stub(predict_query, "get_json", m_get_json)

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
  original_api_key <- Sys.getenv("SYNTHESIZE_API_KEY")
  on.exit({
    if (original_api_key != "") {
      Sys.setenv(SYNTHESIZE_API_KEY = original_api_key)
    } else {
      Sys.unsetenv("SYNTHESIZE_API_KEY")
    }
  })

  Sys.setenv(SYNTHESIZE_API_KEY = "test-api-token")

  m_has_token <- mock(TRUE, cycle = TRUE)
  m_start_query <- mock("abc123", cycle = TRUE)

  # First poll returns running, but we'll just return ready immediately for simplicity
  m_poll <- mock(
    list(
      status = "ready",
      payload = list(
        status = "ready",
        downloadUrl = "https://example.com/final.json"
      )
    ),
    cycle = TRUE
  )

  m_get_json <- mock(
    list(
      outputs = list(
        list(
          counts = c(1, 2, 3),
          metadata = list(sample_id = "s1")
        ),
        list(
          counts = c(4, 5, 6),
          metadata = list(sample_id = "s2")
        )
      ),
      gene_order = c("gene1", "gene2", "gene3")
    ),
    cycle = TRUE
  )

  stub(predict_query, "has_synthesize_token", m_has_token)
  stub(predict_query, "start_model_query", m_start_query)
  stub(predict_query, "poll_model_query", m_poll)
  stub(predict_query, "get_json", m_get_json)

  query <- get_valid_query(modality = "czi")
  result <- predict_query(query)

  expect_true("metadata" %in% names(result))
  expect_true("expression" %in% names(result))
  expect_equal(nrow(result$metadata), 2)
  expect_equal(colnames(result$expression), c("gene1", "gene2", "gene3"))
  expect_equal(as.numeric(result$expression[1, ]), c(1, 2, 3))
  expect_equal(as.numeric(result$expression[2, ]), c(4, 5, 6))
})

test_that("predict_query single-cell failure (mocked)", {
  original_api_key <- Sys.getenv("SYNTHESIZE_API_KEY")
  on.exit({
    if (original_api_key != "") {
      Sys.setenv(SYNTHESIZE_API_KEY = original_api_key)
    } else {
      Sys.unsetenv("SYNTHESIZE_API_KEY")
    }
  })

  Sys.setenv(SYNTHESIZE_API_KEY = "test-api-token")

  m_has_token <- mock(TRUE, cycle = TRUE)
  m_start_query <- mock("abc123", cycle = TRUE)

  m_poll <- mock(
    list(
      status = "failed",
      payload = list(
        status = "failed",
        message = "Invalid metadata: missing required field 'cell_type_ontology_id'"
      )
    ),
    cycle = TRUE
  )

  stub(predict_query, "has_synthesize_token", m_has_token)
  stub(predict_query, "start_model_query", m_start_query)
  stub(predict_query, "poll_model_query", m_poll)

  query <- get_valid_query(modality = "czi")

  expect_error(
    predict_query(query),
    "Model query failed.*missing required field"
  )
})

test_that("predict_query single-cell timeout (mocked)", {
  original_api_key <- Sys.getenv("SYNTHESIZE_API_KEY")
  on.exit({
    if (original_api_key != "") {
      Sys.setenv(SYNTHESIZE_API_KEY = original_api_key)
    } else {
      Sys.unsetenv("SYNTHESIZE_API_KEY")
    }
  })

  Sys.setenv(SYNTHESIZE_API_KEY = "test-api-token")

  m_has_token <- mock(TRUE, cycle = TRUE)
  m_start_query <- mock("abc123", cycle = TRUE)

  # Poll always returns running (timeout scenario)
  m_poll <- mock(
    list(
      status = "running",
      payload = list(status = "running")
    ),
    cycle = TRUE
  )

  stub(predict_query, "has_synthesize_token", m_has_token)
  stub(predict_query, "start_model_query", m_start_query)
  stub(predict_query, "poll_model_query", m_poll)

  query <- get_valid_query(modality = "czi")

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
  expect_true("czi" %in% modalities)
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

  query_czi <- get_valid_query(modality = "czi")
  expect_equal(query_czi$modality, "czi")
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
    "Query requires 'modality' key"
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
  original_api_key <- Sys.getenv("SYNTHESIZE_API_KEY")
  on.exit({
    if (original_api_key != "") {
      Sys.setenv(SYNTHESIZE_API_KEY = original_api_key)
    } else {
      Sys.unsetenv("SYNTHESIZE_API_KEY")
    }
  })

  Sys.setenv(SYNTHESIZE_API_KEY = "mock-api-key")

  m_has_token <- mock(TRUE, cycle = TRUE)
  m_start_query <- mock("test-id", cycle = TRUE)
  m_poll <- mock(
    list(
      status = "ready",
      payload = list(
        status = "ready",
        downloadUrl = "https://example.com/data.json"
      )
    ),
    cycle = TRUE
  )

  m_get_json <- mock(
    list(
      outputs = list(
        list(
          counts = c(100, 200, 300),
          metadata = list(sample_id = "test1")
        )
      ),
      gene_order = c("gene1", "gene2", "gene3")
    ),
    cycle = TRUE
  )

  # Mock log_cpm to track if it's called
  m_log_cpm <- mock(
    data.frame(
      gene1 = log1p(100),
      gene2 = log1p(200),
      gene3 = log1p(300)
    ),
    cycle = TRUE
  )

  stub(predict_query, "has_synthesize_token", m_has_token)
  stub(predict_query, "start_model_query", m_start_query)
  stub(predict_query, "poll_model_query", m_poll)
  stub(predict_query, "get_json", m_get_json)
  stub(predict_query, "log_cpm", m_log_cpm)

  query <- get_valid_query()

  # Test with as_counts = TRUE (log_cpm should not be called)
  result_counts <- predict_query(query, as_counts = TRUE)
  expect_called(m_log_cpm, 0)

  # Test with as_counts = FALSE (log_cpm should be called)
  result_log <- predict_query(query, as_counts = FALSE)
  expect_called(m_log_cpm, 1)
})
