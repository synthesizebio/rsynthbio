library(dplyr)
library(purrr)
library(tibble)
library(tidyr)

# Test file for log_cpm and extract_expression_data functions

# Tests for log_cpm function
test_that("log_cpm transforms data correctly", {
  # Create sample raw counts
  raw_counts <- data.frame(
    gene1 = c(100, 200, 300),
    gene2 = c(50, 100, 150),
    gene3 = c(10, 20, 30)
  )

  # Transform to log CPM
  result <- log_cpm(raw_counts)

  # Manually calculate expected values for first row
  row1_lib_size <- sum(raw_counts[1, ])  # 100 + 50 + 10 = 160
  expected_gene1_cpm <- (100 / 160) * 1e6  # 625000
  expected_gene2_cpm <- (50 / 160) * 1e6   # 312500
  expected_gene3_cpm <- (10 / 160) * 1e6   # 62500

  # Log1p of expected values
  expected_gene1_log <- log1p(expected_gene1_cpm)
  expected_gene2_log <- log1p(expected_gene2_cpm)
  expected_gene3_log <- log1p(expected_gene3_cpm)

  # Check column names have _cpm suffix
  expect_true(all(grepl("_cpm$", colnames(result))))

  # Check values for first row (with tolerance for floating point differences)
  expect_equal(result$gene1_cpm[1], expected_gene1_log, tolerance = 1e-5)
  expect_equal(result$gene2_cpm[1], expected_gene2_log, tolerance = 1e-5)
  expect_equal(result$gene3_cpm[1], expected_gene3_log, tolerance = 1e-5)

  # Check dimensions
  expect_equal(nrow(result), nrow(raw_counts))
  expect_equal(ncol(result), ncol(raw_counts))
})

test_that("log_cpm handles edge cases correctly", {
  # Test with matrix input
  matrix_input <- matrix(c(100, 200, 50, 100, 10, 20), nrow = 2)
  colnames(matrix_input) <- c("ENSG00001", "ENSG00002", "ENSG00003")
  expect_error(log_cpm(matrix_input), NA) # Should not error

  # Test with zero counts
  zero_counts <- data.frame(
    gene1 = c(0, 200, 300),
    gene2 = c(50, 0, 150),
    gene3 = c(10, 20, 0)
  )
  result_zeros <- log_cpm(zero_counts)
  expect_false(any(is.na(result_zeros)))

  # Test with negative values (should be converted to 0)
  neg_counts <- data.frame(
    gene1 = c(-10, 200, 300),
    gene2 = c(50, -20, 150),
    gene3 = c(10, 20, -30)
  )
  result_neg <- log_cpm(neg_counts)
  expect_false(any(is.na(result_neg)))
})

test_that("log_cpm handles invalid inputs correctly", {
  # Test with non-data frame/matrix
  expect_error(log_cpm(list(a = 1:3, b = 4:6)), "Input must be a data frame or matrix")

  # Test with empty data frame
  expect_error(log_cpm(data.frame()), "Input must have at least one row and one column")

  # Test with data frame with no columns
  empty_df <- data.frame(x = integer(0))[, FALSE]
  expect_error(log_cpm(empty_df), "Input must have at least one row and one column")
})

# Tests for extract_expression_data function
test_that("extract_expression_data processes API response correctly", {
  # Create mock API response
  mock_api_response <- list(
    gene_order = c("ENSG00001", "ENSG00002", "ENSG00003"),
    outputs = list(
      expression = list(
        matrix(c(100, 200, 50, 100, 10, 20), nrow = 2),
        matrix(c(300, 400, 150, 200, 30, 40), nrow = 2)
      ),
      metadata = data.frame(
        condition = c("control", "treatment"),
        batch = c(1, 2)
      )
    )
  )

  # Test with as_counts = TRUE (default)
  result_counts <- extract_expression_data(mock_api_response)

  # Check structure
  expect_type(result_counts, "list")
  expect_named(result_counts, c("metadata", "expression"))

  # Check metadata
  expect_s3_class(result_counts$metadata, "tbl_df")
  expect_equal(nrow(result_counts$metadata), 4)  # 2 samples per group, 2 groups
  expect_true("condition" %in% colnames(result_counts$metadata))
  expect_true("batch" %in% colnames(result_counts$metadata))
  expect_true("sample_id" %in% colnames(result_counts$metadata))

  # Check expression data
  expect_s3_class(result_counts$expression, "data.frame")
  expect_equal(nrow(result_counts$expression), 4)  # 2 samples per group, 2 groups
  expect_equal(ncol(result_counts$expression), 3)  # 3 genes
  expect_equal(colnames(result_counts$expression), c("ENSG00001", "ENSG00002", "ENSG00003"))

  # Test integer conversion
  expect_true(all(sapply(result_counts$expression, is.integer)))

  # Test with as_counts = FALSE (log CPM transformation)
  result_logcpm <- extract_expression_data(mock_api_response, as_counts = FALSE)

  # Check expression data has been transformed (no longer integers)
  expect_false(all(sapply(result_logcpm$expression, is.integer)))
})


test_that("extract_expression_data correctly assigns sample IDs", {
  # Create mock API response with multiple groups
  mock_api_groups <- list(
    gene_order = c("GENE1", "GENE2"),
    outputs = list(
      expression = list(
        matrix(c(100, 200, 50, 100), nrow = 2),
        matrix(c(300, 400, 150, 200), nrow = 2),
        matrix(c(500, 600, 250, 300), nrow = 2)
      ),
      metadata = data.frame(
        condition = c("control", "treatment", "mutant"),
        batch = c(1, 2, 3)
      )
    )
  )

  # Test sample ID generation
  result_groups <- extract_expression_data(mock_api_groups)

  # Check sample IDs are unique
  expect_equal(length(unique(result_groups$metadata$sample_id)), 6)  # 2 samples per group, 3 groups

  # Check sample IDs match between metadata and expression
  expect_equal(
    sort(result_groups$metadata$sample_id),
    sort(rownames(result_groups$expression))
  )
})
