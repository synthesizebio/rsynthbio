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

    message("Live bulk API test passed.")
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
