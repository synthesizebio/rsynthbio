
# Updated mock API response to match the actual v2.0 structure
mock_api_response <- list(
  # Model version
  model_version = 2,

  # Gene order (unchanged)
  gene_order = c("ENSG00000000003", "ENSG00000000005", "ENSG00000000419", "ENSG00000000457",
                 "ENSG00000000460", "ENSG00000000938",
                 # Add more genes to reach 44,592 total
                 paste0("ENSG", sprintf("%011d", 1:44586))),

  # NEW STRUCTURE: outputs is now a data.frame with list columns
  outputs = data.frame(
    # counts column - list of integer vectors (one per sample)
    counts = I(list(
      as.integer(c(904, 0, 539, 115, 239, 0, 1976, 817, 2422, 372, runif(44582, 0, 2000))),
      as.integer(c(1350, 0, 343, 120, 151, 5, 937, 947, 1439, 344, runif(44582, 0, 2000))),
      as.integer(c(1082, 0, 471, 144, 230, 4, 924, 861, 2093, 179, runif(44582, 0, 2000))),
      as.integer(c(851, 5, 423, 147, 139, 3, 725, 1592, 5669, 407, runif(44582, 0, 2000))),
      as.integer(c(339, 0, 356, 170, 91, 0, 1119, 748, 2459, 314, runif(44582, 0, 2000))),
      as.integer(c(337, 0, 545, 174, 106, 28, 734, 770, 570, 651, runif(44582, 0, 2000))),
      as.integer(c(822, 0, 440, 578, 60, 20, 1411, 915, 1004, 500, runif(44582, 0, 2000))),
      as.integer(c(591, 0, 360, 212, 104, 53, 883, 1021, 826, 1439, runif(44582, 0, 2000))),
      as.integer(c(999, 0, 844, 228, 61, 30, 786, 977, 446, 516, runif(44582, 0, 2000))),
      as.integer(c(638, 1, 578, 194, 92, 64, 828, 416, 605, 613, runif(44582, 0, 2000)))
    )),

    # classifier_probs - data.frame with nested data.frames for each classifier
    classifier_probs = I(data.frame(
      # Sex probabilities
      sex = I(data.frame(
        female = c(0.0734, 0.1465, 0.1251, 0.0948, 0.2374, 0.1897, 0.2156, 0.1634, 0.0892, 0.1749),
        male = c(0.927, 0.853, 0.875, 0.905, 0.763, 0.810, 0.784, 0.837, 0.911, 0.825)
      )),

      # Age years probabilities (96 age categories)
      age_years = I(data.frame(
        matrix(runif(10 * 96, 0, 0.05), nrow = 10, ncol = 96,
               dimnames = list(NULL, as.character(0:95)))
      )),

      # Tissue ontology probabilities (442 categories)
      tissue_ontology_id = I(data.frame(
        matrix(runif(10 * 442, 0, 1e-3), nrow = 10, ncol = 442,
               dimnames = list(NULL, c("CL:0000000", "CL:0000030", "CL:0000031",
                                       paste0("UBERON:", sprintf("%07d", 1:439)))))
      )),

      # Sample type probabilities
      sample_type = I(data.frame(
        "cell line" = c(0.937, 0.966, 0.929, 0.909, 0.95, 0.943, 0.901, 0.934, 0.956, 0.912),
        organoid = c(0.01129, 0.00439, 0.01091, 0.01375, 0.00753, 0.00891, 0.01234, 0.00876, 0.00543, 0.01098),
        other = c(0.01282, 0.00688, 0.01579, 0.01839, 0.01271, 0.01456, 0.01789, 0.01345, 0.00987, 0.01567),
        primary = c(0.00893, 0.00553, 0.00975, 0.01319, 0.00601, 0.00834, 0.01287, 0.00756, 0.00612, 0.00945),
        "primary cells" = c(0.01198, 0.00467, 0.00902, 0.01929, 0.00837, 0.01023, 0.01567, 0.00934, 0.00723, 0.01134),
        "primary tissue" = c(0.00706, 0.00682, 0.00779, 0.0176, 0.00782, 0.00934, 0.01456, 0.00812, 0.00678, 0.00945),
        xenograft = c(0.01084, 0.00555, 0.01784, 0.00864, 0.00804, 0.01123, 0.01678, 0.00923, 0.00734, 0.01234),
        check.names = FALSE
      )),

      # Disease ontology probabilities (589 categories)
      disease_ontology_id = I(data.frame(
        matrix(runif(10 * 589, 0, 1e-3), nrow = 10, ncol = 589,
               dimnames = list(NULL, c("CL:0000623", "CL:0017002", "HGNC:11474",
                                       paste0("MONDO:", sprintf("%07d", 1:586)))))
      )),

      # Cell type ontology probabilities (392 categories)
      cell_type_ontology_id = I(data.frame(
        matrix(runif(10 * 392, 0, 1e-3), nrow = 10, ncol = 392,
               dimnames = list(NULL, paste0("CL:", sprintf("%07d", 1:392))))
      )),

      # Cell line ontology probabilities (763 categories) - CVCL_0023 dominates
      cell_line_ontology_id = I(data.frame(
        CVCL_0023 = c(0.862, 0.826, 0.875, 0.836, 0.854, 0.843, 0.821, 0.867, 0.891, 0.838),
        matrix(runif(10 * 762, 0, 1e-3), nrow = 10, ncol = 762,
               dimnames = list(NULL, paste0("CVCL_", sprintf("%04d", 1:762))))
      ))
    )),

    # latents - data.frame with list columns for each latent type
    latents = I(data.frame(
      biological = I(list(
        runif(1024, -3, 3), runif(1024, -3, 3), runif(1024, -3, 3), runif(1024, -3, 3), runif(1024, -3, 3),
        runif(1024, -3, 3), runif(1024, -3, 3), runif(1024, -3, 3), runif(1024, -3, 3), runif(1024, -3, 3)
      )),
      technical = I(list(
        c(-0.0271, 0.518, -1.8222, -0.0959, 26.1482, runif(27, -5, 30)),
        c(-1.66, 2.05, -3.95, 1.12, 17.84, runif(27, -5, 30)),
        c(-0.446, 1.245, -4.278, 0.279, 20.811, runif(27, -5, 30)),
        c(-1.73, -1.001, -2.311, 0.122, 13.508, runif(27, -5, 30)),
        c(-0.631, 1.919, -2.409, 0.481, 10.967, runif(27, -5, 30)),
        c(-1.282, -1.58, -4.221, 0.799, 12.121, runif(27, -5, 30)),
        c(-0.823, 0.58, -3.437, 1.953, 17.658, runif(27, -5, 30)),
        c(-0.0634, 1.2, -3.6599, 1.7813, 19.5482, runif(27, -5, 30)),
        c(-1.018, -1.78, -3.108, -0.326, 15.316, runif(27, -5, 30)),
        c(0.267, -0.548, -2.999, 1.454, 13.085, runif(27, -5, 30))
      )),
      perturbation = I(list(
        runif(512, -2, 2), runif(512, -2, 2), runif(512, -2, 2), runif(512, -2, 2), runif(512, -2, 2),
        runif(512, -2, 2), runif(512, -2, 2), runif(512, -2, 2), runif(512, -2, 2), runif(512, -2, 2)
      ))
    )),

    # metadata - data.frame with sample metadata
    metadata = I(data.frame(
      age_years = c("", "", "", "", "", "65", "65", "65", "65", "65"),
      cell_line_ontology_id = c(rep("CVCL_0023", 5), rep("", 5)),
      cell_type_ontology_id = rep("", 10),
      developmental_stage = rep("", 10),
      disease_ontology_id = c(rep("", 5), rep("MONDO:0011719", 5)),
      ethnicity = rep("", 10),
      genotype = rep("", 10),
      library_layout = rep("", 10),
      library_selection = rep("", 10),
      modality = rep("bulk", 10),
      perturbation_dose = rep("", 10),
      perturbation_ontology_id = c(rep("ENSG00000156127", 5), rep("", 5)),
      perturbation_time = c(rep("96 hours", 5), rep("", 5)),
      perturbation_type = c(rep("crispr", 5), rep("", 5)),
      platform = rep("", 10),
      race = rep("", 10),
      sample_type = c(rep("cell line", 5), rep("primary tissue", 5)),
      sex = c(rep("", 5), rep("female", 5)),
      study = rep("", 10),
      tissue_ontology_id = c(rep("", 5), rep("UBERON:0000945", 5)),
      stringsAsFactors = FALSE
    )),

    stringsAsFactors = FALSE
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

