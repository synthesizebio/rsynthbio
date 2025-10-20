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

    # Verify get_json returned valid data structure
    # Check that expression data contains numeric values
    expect_true(all(sapply(results$expression, is.numeric)),
                info = "Expression data should contain numeric values")

    # Check that metadata and expression have matching row counts
    expect_equal(nrow(results$metadata), nrow(results$expression),
                 info = "Metadata and expression should have same number of rows")

    # Verify expression values are non-negative (counts should be >= 0)
    expect_true(all(results$expression >= 0, na.rm = TRUE),
                info = "Expression counts should be non-negative")

    # Verify gene names exist as column names
    expect_true(length(colnames(results$expression)) > 0,
                info = "Expression data should have gene names as column names")

    # Verify all gene names are non-empty strings
    expect_true(all(nchar(colnames(results$expression)) > 0),
                info = "All gene names should be non-empty strings")

    message("Live bulk API test passed with data validation.")
})

test_that("predict_query live call success (single-cell)", {
    skip_if_not(
        api_key_available(),
        "Skipping live API test because SYNTHESIZE_API_KEY is not set."
    )

    message("\nTesting live predict_query call for single-cell modality...")

    test_query <- get_valid_query(modality = "single-cell")

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

    # Verify get_json returned valid data structure
    # Check that expression data contains numeric values
    expect_true(all(sapply(results$expression, is.numeric)),
                info = "Expression data should contain numeric values")

    # Check that metadata and expression have matching row counts
    expect_equal(nrow(results$metadata), nrow(results$expression),
                 info = "Metadata and expression should have same number of rows")

    # Verify expression values are non-negative (counts should be >= 0)
    expect_true(all(results$expression >= 0, na.rm = TRUE),
                info = "Expression counts should be non-negative")

    # Verify gene names exist as column names
    expect_true(length(colnames(results$expression)) > 0,
                info = "Expression data should have gene names as column names")

    # Verify all gene names are non-empty strings
    expect_true(all(nchar(colnames(results$expression)) > 0),
                info = "All gene names should be non-empty strings")

    # Verify metadata contains expected fields for single-cell
    metadata_cols <- colnames(results$metadata)
    expect_true(length(metadata_cols) > 0,
                info = "Metadata should have at least one column")

    message("Live single-cell API test passed with data validation.")
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
        modality = "single-cell",
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

test_that("predict_query download URL flow works correctly", {
    skip_if_not(
        api_key_available(),
        "Skipping live API test because SYNTHESIZE_API_KEY is not set."
    )

    message("\nTesting download URL flow with return_download_url=TRUE...")

    test_query <- get_valid_query()

    # First get the download URL without parsing
    result_with_url <- predict_query(
        query = test_query,
        as_counts = TRUE,
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
                info = "Download URL should start with http:// or https://")

    # Verify empty data frames when return_download_url=TRUE
    expect_equal(nrow(result_with_url$metadata), 0)
    expect_equal(nrow(result_with_url$expression), 0)

    # Now manually fetch the URL using get_json to verify it works
    message("Testing get_json with real download URL...")
    json_result <- rsynthbio:::get_json(download_url)

    expect_type(json_result, "list")
    expect_true("outputs" %in% names(json_result) || "gene_order" %in% names(json_result),
                info = "JSON result should contain expected fields")

    # Verify we got valid data
    if ("outputs" %in% names(json_result)) {
        expect_true(length(json_result$outputs) > 0,
                    info = "Outputs should contain at least one element")
    }

    if ("gene_order" %in% names(json_result)) {
        expect_true(length(json_result$gene_order) > 0,
                    info = "Gene order should contain gene names")
    }

    message("Download URL flow and get_json validation passed.")
})
