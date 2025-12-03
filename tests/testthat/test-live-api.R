library(testthat)
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

test_that("list_models live API call", {
    skip_if_not(
        api_key_available(),
        "Skipping live API test because SYNTHESIZE_API_KEY is not set."
    )

    message("\nTesting live list_models call...")

    models <- list_models()

    # Verify we got a valid response
    expect_type(models, "list")
    expect_true(length(models) > 0, info = "Should return at least one model")

    # Check that expected models are present
    # API returns structure: list(models = data.frame(model_id = ...))
    model_ids <- sapply(models, function(m) m$model_id)

    expect_true("gem-1-bulk" %in% model_ids,
        info = "Should include gem-1-bulk model"
    )
    expect_true("gem-1-sc" %in% model_ids,
        info = "Should include gem-1-sc model"
    )

    message(sprintf("Successfully retrieved %d models", length(model_ids)))
})

test_that("get_example_query live API call", {
    skip_if_not(
        api_key_available(),
        "Skipping live API test because SYNTHESIZE_API_KEY is not set."
    )

    message("\nTesting live get_example_query call...")

    # Test for bulk model
    bulk_response <- get_example_query(model_id = "gem-1-bulk")
    expect_type(bulk_response, "list")
    expect_true("example_query" %in% names(bulk_response),
        info = "Response should contain example_query field"
    )
    expect_true("inputs" %in% names(bulk_response$example_query),
        info = "Bulk query should contain inputs"
    )

    # Test for single-cell model
    sc_response <- get_example_query(model_id = "gem-1-sc")
    expect_type(sc_response, "list")
    expect_true("example_query" %in% names(sc_response),
        info = "Response should contain example_query field"
    )
    expect_true("inputs" %in% names(sc_response$example_query),
        info = "Single-cell query should contain inputs"
    )

    message("Successfully retrieved example queries for both models")
})

test_that("predict_query live call success (bulk)", {
    skip_if_not(
        api_key_available(),
        "Skipping live API test because SYNTHESIZE_API_KEY is not set."
    )

    message("\nTesting live predict_query call for bulk modality...")

    test_query <- get_example_query(model_id = "gem-1-bulk")$example_query

    results <- predict_query(
        query = test_query,
        model_id = "gem-1-bulk",
    )

    expect_type(results, "list")
    expect_true("metadata" %in% names(results))
    expect_true("expression" %in% names(results))

    expect_s3_class(results$metadata, "data.frame")
    expect_s3_class(results$expression, "data.frame")

    expect_true(nrow(results$metadata) > 0)
    expect_true(nrow(results$expression) > 0)
    expect_true(ncol(results$expression) > 0)

    # Verify get_json returned valid data structure
    # Check that expression data contains numeric values
    expect_true(all(sapply(results$expression, is.numeric)),
        info = "Expression data should contain numeric values"
    )

    # Check that metadata and expression have matching row counts
    expect_equal(nrow(results$metadata), nrow(results$expression),
        info = "Metadata and expression should have same number of rows"
    )

    # Verify expression values are non-negative (counts should be >= 0)
    expect_true(all(results$expression >= 0, na.rm = TRUE),
        info = "Expression counts should be non-negative"
    )

    # Verify gene names exist as column names
    expect_true(length(colnames(results$expression)) > 0,
        info = "Expression data should have gene names as column names"
    )

    # Verify all gene names are non-empty strings
    expect_true(all(nchar(colnames(results$expression)) > 0),
        info = "All gene names should be non-empty strings"
    )

    message("Live bulk API test passed with data validation.")
})

test_that("predict_query live call success (single-cell)", {
    skip_if_not(
        api_key_available(),
        "Skipping live API test because SYNTHESIZE_API_KEY is not set."
    )

    message("\nTesting live predict_query call for single-cell modality...")

    test_query <- get_example_query(model_id = "gem-1-sc")$example_query

    results <- predict_query(
        query = test_query,
        model_id = "gem-1-sc",
    )

    expect_type(results, "list")
    expect_true("metadata" %in% names(results))
    expect_true("expression" %in% names(results))

    expect_s3_class(results$metadata, "data.frame")
    expect_s3_class(results$expression, "data.frame")

    expect_true(nrow(results$metadata) > 0)
    expect_true(nrow(results$expression) > 0)
    expect_true(ncol(results$expression) > 0)

    # Verify get_json returned valid data structure
    # Check that expression data contains numeric values
    expect_true(all(sapply(results$expression, is.numeric)),
        info = "Expression data should contain numeric values"
    )

    # Check that metadata and expression have matching row counts
    expect_equal(nrow(results$metadata), nrow(results$expression),
        info = "Metadata and expression should have same number of rows"
    )

    # Verify expression values are non-negative (counts should be >= 0)
    expect_true(all(results$expression >= 0, na.rm = TRUE),
        info = "Expression counts should be non-negative"
    )

    # Verify gene names exist as column names
    expect_true(length(colnames(results$expression)) > 0,
        info = "Expression data should have gene names as column names"
    )

    # Verify all gene names are non-empty strings
    expect_true(all(nchar(colnames(results$expression)) > 0),
        info = "All gene names should be non-empty strings"
    )

    # Verify metadata contains expected fields for single-cell
    metadata_cols <- colnames(results$metadata)
    expect_true(length(metadata_cols) > 0,
        info = "Metadata should have at least one column"
    )

    message("Live single-cell API test passed with data validation.")
})

test_that("predict_query live call invalid UBERON (bulk)", {
    skip_if_not(
        api_key_available(),
        "Skipping live API test because SYNTHESIZE_API_KEY is not set."
    )

    message("\nTesting live predict_query with invalid UBERON ID for bulk...")

    # Start with a valid example query and modify it to have an invalid UBERON ID
    invalid_query <- get_example_query(model_id = "gem-1-bulk")$example_query
    invalid_query$inputs[[1]]$tissue_ontology_id <- "UBERON:999999" # Invalid ID
    invalid_query$inputs[[1]]$num_samples <- 1

    # The API should reject this with an error
    expect_error(
        predict_query(
            query = invalid_query,
            model_id = "gem-1-bulk",
        ),
        "Model query failed"
    )

    # Verify the error contains validation details
    error_result <- tryCatch(
        predict_query(query = invalid_query, model_id = "gem-1-bulk"),
        error = function(e) e
    )

    message(paste("API correctly rejected invalid UBERON ID with error:", error_result))

    # The error message should contain the validation details
    expect_true(
        grepl("UBERON:999999", error_result),
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

    # Start with a valid example query and modify it to have an invalid UBERON ID
    invalid_query <- get_example_query(model_id = "gem-1-sc")$example_query
invalid_query$inputs[[1]]$tissue_ontology_id <- "UBERON:999999" # Invalid ID
    invalid_query$inputs[[1]]$num_samples <- 1
    invalid_query$seed <- 42

    # The API should reject this with an error
    expect_error(
        predict_query(
            query = invalid_query,
            model_id = "gem-1-sc",
        ),
        "Model query failed"
    )

    # Verify the error contains validation details
    error_result <- tryCatch(
        predict_query(query = invalid_query, model_id = "gem-1-sc"),
        error = function(e) e
    )

    message(paste("API correctly rejected invalid UBERON ID (single-cell) with error:", error_result))

    # The error message should contain the validation details
    expect_true(
        grepl("UBERON:999999", error_result),
        info = paste("Error message should mention the invalid UBERON ID. Got:", error_result)
    )
    expect_true(
        grepl("bad values|invalid", error_result, ignore.case = TRUE),
        info = paste("Error message should indicate validation failure. Got:", error_result)
    )

    message("Successfully validated error message contains UBERON validation details (single-cell)")
})

test_that("predict_query download URL flow works correctly", {
    skip_if_not(
        api_key_available(),
        "Skipping live API test because SYNTHESIZE_API_KEY is not set."
    )

    message("\nTesting download URL flow with return_download_url=TRUE...")

    test_query <- get_example_query(model_id = "gem-1-bulk")$example_query

    # First get the download URL without parsing
    result_with_url <- predict_query(
        query = test_query,
        model_id = "gem-1-bulk",
        return_download_url = TRUE
    )

    expect_type(result_with_url, "list")
    expect_true("download_url" %in% names(result_with_url))
    expect_true("metadata" %in% names(result_with_url))
    expect_true("expression" %in% names(result_with_url))

    # Verify download URL is a valid URL
    download_url <- result_with_url$download_url
    expect_type(download_url, "character")
    expect_true(nchar(download_url) > 0)
    expect_true(grepl("^https?://", download_url),
        info = "Download URL should start with http:// or https://"
    )

    # Verify empty data frames when return_download_url=TRUE
    expect_equal(nrow(result_with_url$metadata), 0)
    expect_equal(nrow(result_with_url$expression), 0)

    # Now manually fetch the URL using get_json to verify it works
    message("Testing get_json with real download URL...")
    json_result <- rsynthbio:::get_json(download_url)

    expect_type(json_result, "list")
    expect_true("outputs" %in% names(json_result) || "gene_order" %in% names(json_result),
        info = "JSON result should contain expected fields"
    )

    # Verify we got valid data
    if ("outputs" %in% names(json_result)) {
        expect_true(length(json_result$outputs) > 0,
            info = "Outputs should contain at least one element"
        )
    }

    if ("gene_order" %in% names(json_result)) {
        expect_true(length(json_result$gene_order) > 0,
            info = "Gene order should contain gene names"
        )
    }

    message("Download URL flow and get_json validation passed.")
})

test_that("predict_query returns biologically valid expression data (differential expression)", {
    skip_if_not(
        api_key_available(),
        "Skipping live API test because SYNTHESIZE_API_KEY is not set."
    )

    message("\nTesting biological validity with simple differential expression analysis...")

    # Start with a valid example query and modify it for differential expression test
    de_query <- get_example_query(model_id = "gem-1-bulk")$example_query

    # Create query with two distinct conditions
    de_query$inputs <- list(
        # Condition 1: One cell type
        list(
            metadata = list(
                cell_type_ontology_id = "CL:0000786", # Plasmacytoid dendritic cell
                tissue_ontology_id = "UBERON:0002371", # bone marrow
                sex = "female",
                sample_type = "primary tissue"
            ),
            num_samples = 5
        ),
        # Condition 2: Different cell type
        list(
            metadata = list(
                cell_type_ontology_id = "CL:0000763", # Myeloid cell
                tissue_ontology_id = "UBERON:0002371", # bone marrow
                sex = "female",
                sample_type = "primary tissue"
            ),
            num_samples = 5
        )
    )
    de_query$sampling_strategy <- "sample generation"
    de_query$seed <- 42

    results <- predict_query(query = de_query, model_id = "gem-1-bulk")

    # Split samples by condition
    group1_idx <- 1:5
    group2_idx <- 6:10

    expr_group1 <- results$expression[group1_idx, ]
    expr_group2 <- results$expression[group2_idx, ]

    # Calculate basic statistics for each gene
    n_genes <- ncol(results$expression)

    # Calculate mean expression for each group
    mean_group1 <- colMeans(expr_group1)
    mean_group2 <- colMeans(expr_group2)

    # Calculate fold changes (using pseudocount to avoid division by zero)
    pseudocount <- 1
    fold_changes <- log2((mean_group2 + pseudocount) / (mean_group1 + pseudocount))

    # Perform t-tests for each gene
    p_values <- sapply(1:n_genes, function(i) {
        tryCatch(
            {
                t.test(expr_group1[, i], expr_group2[, i])$p.value
            },
            error = function(e) {
                NA
            }
        )
    })

    # Basic validation of differential expression results
    message("Validating differential expression statistics...")

    # 1. Check that we have valid p-values
    valid_pvals <- !is.na(p_values)
    expect_true(sum(valid_pvals) > n_genes * 0.9,
        info = "At least 90% of genes should have valid p-values"
    )

    # 2. P-values should be distributed between 0 and 1
    expect_true(all(p_values[valid_pvals] >= 0 & p_values[valid_pvals] <= 1),
        info = "All p-values should be between 0 and 1"
    )

    # 3. Not all p-values should be identical (showing variation)
    expect_true(length(unique(p_values[valid_pvals])) > 100,
        info = "P-values should show variation across genes"
    )

    # 4. Fold changes should be reasonable (not all zero, not all extreme)
    expect_true(sd(fold_changes, na.rm = TRUE) > 0,
        info = "Fold changes should show variation"
    )
    expect_true(abs(median(fold_changes, na.rm = TRUE)) < 10,
        info = "Median fold change should be reasonable (|log2FC| < 10)"
    )

    # 5. Check for differentially expressed genes (p < 0.05)
    de_genes <- which(p_values < 0.05)
    expect_true(length(de_genes) > 0,
        info = "Should detect some differentially expressed genes"
    )
    expect_true(length(de_genes) < n_genes * 0.5,
        info = "Not all genes should be differentially expressed"
    )

    # 6. Variance should exist within groups (biological variation)
    var_group1 <- apply(expr_group1, 2, var)
    var_group2 <- apply(expr_group2, 2, var)
    expect_true(median(var_group1, na.rm = TRUE) > 0,
        info = "Group 1 should show within-group variance"
    )
    expect_true(median(var_group2, na.rm = TRUE) > 0,
        info = "Group 2 should show within-group variance"
    )

    # 7. Expression levels should be reasonable for count data
    overall_mean <- mean(as.matrix(results$expression), na.rm = TRUE)
    expect_true(overall_mean > 0,
        info = "Mean expression should be positive"
    )
    expect_true(overall_mean < 1e6,
        info = "Mean expression should be in reasonable range"
    )

    message(sprintf(
        "DE analysis complete: %d DE genes (p<0.05) out of %d tested",
        length(de_genes), sum(valid_pvals)
    ))
    message(sprintf("Median fold change: %.3f (log2)", median(fold_changes, na.rm = TRUE)))
    message(sprintf(
        "Expression range: %.1f to %.1f",
        min(results$expression, na.rm = TRUE),
        max(results$expression, na.rm = TRUE)
    ))

    message("Biological validity tests passed!")
})

test_that("predict_query returns biologically valid single-cell expression data (differential expression)", {
    skip_if_not(
        api_key_available(),
        "Skipping live API test because SYNTHESIZE_API_KEY is not set."
    )

    message("\nTesting single-cell biological validity with differential expression analysis...")

    # Start with a valid example query and modify it for differential expression test
    sc_de_query <- get_example_query(model_id = "gem-1-sc")$example_query

    # Create query with two distinct cell types
    sc_de_query$inputs <- list(
        # Condition 1: T cells
        list(
            metadata = list(
                cell_type_ontology_id = "CL:0000084", # T cell
                tissue_ontology_id = "UBERON:0002371", # bone marrow
                sex = "female"
            ),
            num_samples = 10
        ),
        # Condition 2: B cells
        list(
            metadata = list(
                cell_type_ontology_id = "CL:0000236", # B cell
                tissue_ontology_id = "UBERON:0002371", # bone marrow
                sex = "female"
            ),
            num_samples = 10
        )
    )
    sc_de_query$sampling_strategy <- "mean estimation"
    sc_de_query$seed <- 123

    results <- predict_query(query = sc_de_query, model_id = "gem-1-sc")

    # Split samples by condition
    group1_idx <- 1:10
    group2_idx <- 11:20

    expr_group1 <- results$expression[group1_idx, ]
    expr_group2 <- results$expression[group2_idx, ]

    # Calculate basic statistics for each gene
    n_genes <- ncol(results$expression)
    n_cells <- nrow(results$expression)

    message(sprintf("Analyzing %d cells across %d genes...", n_cells, n_genes))

    # Single-cell specific metrics
    # 1. Calculate sparsity (proportion of zeros)
    sparsity_group1 <- sum(expr_group1 == 0) / (nrow(expr_group1) * ncol(expr_group1))
    sparsity_group2 <- sum(expr_group2 == 0) / (nrow(expr_group2) * ncol(expr_group2))

    expect_true(sparsity_group1 > 0.3,
        info = "Single-cell data should show sparsity (>30% zeros)"
    )
    expect_true(sparsity_group1 < 0.95,
        info = "Single-cell data should not be too sparse (<95% zeros)"
    )

    message(sprintf(
        "Sparsity: Group1 = %.1f%%, Group2 = %.1f%%",
        sparsity_group1 * 100, sparsity_group2 * 100
    ))

    # 2. Calculate mean expression for each gene
    mean_group1 <- colMeans(expr_group1)
    mean_group2 <- colMeans(expr_group2)

    # 3. Calculate fold changes (using pseudocount for sparse data)
    pseudocount <- 0.1
    fold_changes <- log2((mean_group2 + pseudocount) / (mean_group1 + pseudocount))

    # 4. Perform Wilcoxon rank-sum tests (better for sparse/non-normal single-cell data)
    # Use exact = FALSE to avoid warnings about ties (common with sparse data)
    p_values <- sapply(1:n_genes, function(i) {
        tryCatch(
            {
                wilcox.test(expr_group1[, i], expr_group2[, i], exact = FALSE)$p.value
            },
            error = function(e) {
                NA
            }
        )
    })

    # Validation of single-cell differential expression results
    message("Validating single-cell differential expression statistics...")

    # 1. Check that we have valid p-values
    # Note: Very sparse single-cell data may have many genes with all zeros
    valid_pvals <- !is.na(p_values)
    n_valid <- sum(valid_pvals)
    expect_true(n_valid > 100,
        info = sprintf("Should have at least 100 testable genes (got %d)", n_valid)
    )

    # 2. P-values should be distributed between 0 and 1
    expect_true(all(p_values[valid_pvals] >= 0 & p_values[valid_pvals] <= 1),
        info = "All p-values should be between 0 and 1"
    )

    # 3. P-values should show variation (not all the same)
    expect_true(length(unique(p_values[valid_pvals])) > 100,
        info = "P-values should show variation across genes"
    )

    # 4. Fold changes should show variation
    expect_true(sd(fold_changes, na.rm = TRUE) > 0,
        info = "Fold changes should show variation"
    )
    expect_true(abs(median(fold_changes, na.rm = TRUE)) < 15,
        info = "Median fold change should be reasonable for single-cell"
    )

    # 5. Check for differentially expressed genes
    de_genes <- which(p_values < 0.05)
    expect_true(length(de_genes) > 0,
        info = "Should detect some differentially expressed genes"
    )
    expect_true(length(de_genes) < n_genes * 0.6,
        info = "Not all genes should be differentially expressed"
    )

    # 6. Check for genes with expression in at least some cells
    genes_expressed <- colSums(results$expression > 0)
    pct_expressed_genes <- mean(genes_expressed > 0) * 100
    expect_true(pct_expressed_genes > 5,
        info = sprintf(
            "At least 5%% of genes should be expressed in some cells (got %.1f%%)",
            pct_expressed_genes
        )
    )

    # 7. Single-cell specific: check for variance in expressed genes
    # Only calculate CV for expressed genes (not all zeros)
    expressed_genes <- mean_group1 > 0 | mean_group2 > 0
    n_expressed <- sum(expressed_genes)

    if (n_expressed > 10) {
        var_group1 <- apply(expr_group1[, expressed_genes, drop = FALSE], 2, var)
        cv_group1 <- sqrt(var_group1) / (colMeans(expr_group1[, expressed_genes, drop = FALSE]) + 1e-6)

        # For expressed genes, some should show variation
        expect_true(sum(cv_group1 > 0.1, na.rm = TRUE) > 10,
            info = sprintf("Expressed genes should show variation (testing %d genes)", n_expressed)
        )
    }

    # 8. Expression levels should be reasonable for single-cell count data
    overall_mean <- mean(as.matrix(results$expression), na.rm = TRUE)
    expect_true(overall_mean > 0,
        info = "Mean expression should be positive"
    )
    expect_true(overall_mean < 1e5,
        info = "Mean expression should be in reasonable single-cell range"
    )

    # 9. Check that cell type markers might be differential
    # For T cells vs B cells, we'd expect some strong differences
    strong_de <- sum(abs(fold_changes) > 2 & p_values < 0.01, na.rm = TRUE)
    expect_true(strong_de > 10,
        info = "Should detect some strongly DE genes between T and B cells"
    )

    message(sprintf(
        "DE analysis complete: %d DE genes (p<0.05) out of %d tested",
        length(de_genes), sum(valid_pvals)
    ))
    message(sprintf("Strongly DE genes (|log2FC|>2, p<0.01): %d", strong_de))
    message(sprintf("Median fold change: %.3f (log2)", median(fold_changes, na.rm = TRUE)))
    message(sprintf(
        "Sparsity: %.1f%% (Group1), %.1f%% (Group2)",
        sparsity_group1 * 100, sparsity_group2 * 100
    ))
    message(sprintf(
        "Expression range: %.1f to %.1f",
        min(results$expression, na.rm = TRUE),
        max(results$expression, na.rm = TRUE)
    ))

    message("Single-cell biological validity tests passed!")
})
