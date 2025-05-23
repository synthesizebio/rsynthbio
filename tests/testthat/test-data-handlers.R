
# Test file for log_cpm and extract_expression_data functions
mock_api_response <- list(
  classifier_keys = list(
    sex = c("female", "male"),
    tissue = c("Brodmann (1909) area 4", "adipose tissue", "adrenal gland", "adrenal tissue",
               "lung", "liver", "brain", "heart", "kidney", "stomach", "breast",
               # Add more tissues to reach realistic numbers
               paste0("tissue_", 1:275)),
    disease = c(" B-cell", " M3 (promyelocytic)", " M5 (monocytic)", " T-cell",
                "cancer", "diabetes", "normal", "inflammation", "gastrointestinal stromal tumor",
                # Add more diseases to reach realistic numbers
                paste0("disease_", 1:184)),
    cell_line = c("22Rv1", "A-375", "A-549", "A-673", "HeLa", "MCF-7", "HEK293",
                  # Add more cell lines to reach 65 total
                  paste0("cell_line_", 1:58)),
    cell_type = c("A-375", "A-549", "ASC", "B cell", "epithelial", "fibroblast",
                  "stem cell", "immune cell",
                  # Add more cell types to reach 595 total
                  paste0("cell_type_", 1:587)),
    sample_type = c("cell line", "differentiated", "iPSC", "organoid", "primary tissue",
                    "tissue", "blood", "serum"),
    perturbation_type = c("compound", "control", "crispr", "drug", "genetic", "knockout",
                          "overexpression", "small_molecule", "viral", "chemical")
  ),
  gene_order = c("ENSG00000000003", "ENSG00000000005", "ENSG00000000419", "ENSG00000000457",
                 "ENSG00000000460", "ENSG00000000938",
                 # Add more genes to reach 44,592 total
                 paste0("ENSG0000000", sprintf("%04d", 1:44586))),
  model_version = 1,
  outputs = list(
    classifier_probs = data.frame(
      cell_line = I(list(
        matrix(runif(5*65, 0, 1e-10), nrow = 5, ncol = 65),
        matrix(runif(5*65, 0, 1e-10), nrow = 5, ncol = 65)
      )),
      cell_type = I(list(
        matrix(runif(5*595, 0, 1e-8), nrow = 5, ncol = 595),
        matrix(runif(5*595, 0, 1e-8), nrow = 5, ncol = 595)
      )),
      disease = I(list(
        matrix(runif(5*193, 0, 1e-15), nrow = 5, ncol = 193),
        matrix(runif(5*193, 0, 1e-15), nrow = 5, ncol = 193)
      )),
      perturbation_type = I(list(
        matrix(runif(5*10, 0, 1e-12), nrow = 5, ncol = 10),
        matrix(c(rep(0.981, 5), runif(5*9, 0, 1e-10)), nrow = 5, ncol = 10)
      )),
      sample_type = I(list(
        matrix(c(rep(1, 5), runif(5*7, 0, 1e-10)), nrow = 5, ncol = 8),
        matrix(c(runif(5*1, 0, 1e-13), rep(1, 5*7)), nrow = 5, ncol = 8)
      )),
      sex = I(list(
        matrix(rep(1, 5*2), nrow = 5, ncol = 2),
        matrix(rep(1, 5*2), nrow = 5, ncol = 2)
      )),
      tissue = I(list(
        matrix(runif(5*286, 0, 1e-9), nrow = 5, ncol = 286),
        matrix(runif(5*286, 0, 1e-20), nrow = 5, ncol = 286)
      ))
    ),
    expression = I(list(
      matrix(as.integer(c(rep(1015, 5), rep(6, 5), runif(5*44590, 0, 2000))), nrow = 5, ncol = 44592),
      matrix(as.integer(c(rep(372, 5), rep(1, 5), runif(5*44590, 0, 2000))), nrow = 5, ncol = 44592)
    )),
    latents = I(list(
      list(),
      list()
    )),
    metadata = data.frame(
      cell_line = c("A-549", NA),
      perturbation = c("ABL1", NA),
      perturbation_type = c("crispr", NA),
      perturbation_time = c("96 hours", NA),
      sample_type = c("cell line", "primary tissue"),
      disease = c(NA, "gastrointestinal stromal tumor"),
      age = c(NA, "65 years"),
      sex = c(NA, "female"),
      tissue = c(NA, "stomach"),
      stringsAsFactors = FALSE
    )
  )
)

# Tests for log_cpm function
test_that("log_cpm transforms data correctly", {
  # Create sample raw counts
  raw_counts <- data.frame(
    sample_id = c("A", "B", "C"),
    gene1 = c(100, 200, 300),
    gene2 = c(50, 100, 150),
    gene3 = c(10, 20, 30)
  )

  # Transform to log CPM
  result <- log_cpm(raw_counts)

  # Manually calculate expected values for first row
  row1_lib_size <- sum(raw_counts[1, -1 ])  # 100 + 50 + 10 = 160
  expected_gene1_cpm <- (100 / 160) * 1e6  # 625000
  expected_gene2_cpm <- (50 / 160) * 1e6   # 312500
  expected_gene3_cpm <- (10 / 160) * 1e6   # 62500

  # Log1p of expected values
  expected_gene1_log <- log1p(expected_gene1_cpm)
  expected_gene2_log <- log1p(expected_gene2_cpm)
  expected_gene3_log <- log1p(expected_gene3_cpm)

  # Check column names have _cpm suffix
  expect_true(all(grepl("_cpm$", colnames(result[-1]))))

  # Check values for first row (with tolerance for floating point differences)
  expect_equal(result$gene1_cpm[1], expected_gene1_log, tolerance = 1e-5)
  expect_equal(result$gene2_cpm[2], expected_gene2_log, tolerance = 1e-5)
  expect_equal(result$gene3_cpm[3], expected_gene3_log, tolerance = 1e-5)

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
    sample_id = c("A", "B", "C"),
    gene1 = c(0, 200, 300),
    gene2 = c(50, 0, 150),
    gene3 = c(10, 20, 0)
  )
  result_zeros <- log_cpm(zero_counts)
  expect_false(any(is.na(result_zeros)))

  # Test with negative values (should be converted to 0)
  neg_counts <- data.frame(
    sample_id = c("A", "B", "C"),
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

  # Test with as_counts = TRUE (default)
  result_counts <- extract_expression_data(mock_api_response)

  # Check structure
  expect_type(result_counts, "list")
  expect_named(result_counts, c("metadata", "expression"))

  # Check metadata
  expect_s3_class(result_counts$metadata, "data.frame")
  expect_equal(nrow(result_counts$metadata), 10)

  # Check expression data
  expect_s3_class(result_counts$expression, "data.frame")
  expect_equal(nrow(result_counts$expression), 10)
  expect_equal(colnames(result_counts$expression)[1:4],
               c("sample_id", "ENSG00000000003", "ENSG00000000005", "ENSG00000000419"))

  # Test with as_counts = FALSE (log CPM transformation)
  result_logcpm <- extract_expression_data(mock_api_response, as_counts = FALSE)

  # Check expression data has been transformed (no longer integers)
  expect_false(all(sapply(result_logcpm$expression, is.integer)))
})


test_that("extract_expression_data correctly assigns sample IDs", {

  # Test sample ID generation
  result <- extract_expression_data(mock_api_response)

  # Check sample IDs match between metadata and expression
  expect_equal(
    nrow(result$metadata),
    nrow(result$expression))

  # sample ids should match
  expect_equal(result$metadata$sample_id, result$expression$sample_id)
})

