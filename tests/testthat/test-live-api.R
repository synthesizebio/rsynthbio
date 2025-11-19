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
        as_counts = TRUE
    )

    expect_type(results, "list")
    expect_true("metadata" %in% names(results))
    expect_true("expression" %in% names(results))

    expect_s3_class(results$metadata, "data.frame")
    expect_s3_class(results$expression, "data.frame")

    expect_true(nrow(results$metadata) > 0)
    expect_true(nrow(results$expression) > 0)
    expect_true(ncol(results$expression) > 0)

    # Check that metadata and expression have matching row counts
    expect_equal(nrow(results$metadata), nrow(results$expression),
        info = "Metadata and expression should have same number of rows"
    )

    # Verify gene names exist as column names
    expect_true(length(colnames(results$expression)) > 0,
        info = "Expression data should have gene names as column names"
    )

    message("Live bulk API test passed with structural validation.")
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
        as_counts = TRUE
    )

    expect_type(results, "list")
    expect_true("metadata" %in% names(results))
    expect_true("expression" %in% names(results))

    expect_s3_class(results$metadata, "data.frame")
    expect_s3_class(results$expression, "data.frame")

    expect_true(nrow(results$metadata) > 0)
    expect_true(nrow(results$expression) > 0)
    expect_true(ncol(results$expression) > 0)

    # Check that metadata and expression have matching row counts
    expect_equal(nrow(results$metadata), nrow(results$expression),
        info = "Metadata and expression should have same number of rows"
    )

    message("Live single-cell API test passed with structural validation.")
})

test_that("predict_query live call invalid UBERON (bulk)", {
    skip_if_not(
        api_key_available(),
        "Skipping live API test because SYNTHESIZE_API_KEY is not set."
    )

    message("\nTesting live predict_query with invalid UBERON ID for bulk...")

    # Start with a valid example query and modify it to have an invalid UBERON ID
    invalid_query <- get_example_query(model_id = "gem-1-bulk")$example_query
    invalid_query$inputs[[1]]$metadata$tissue_ontology_id <- "UBERON:9999999" # Invalid ID
    invalid_query$inputs[[1]]$num_samples <- 1

    # The API should reject this with an error
    expect_error(
        predict_query(
            query = invalid_query,
            model_id = "gem-1-bulk",
            as_counts = TRUE
        ),
        "Model query failed"
    )
})

test_that("predict_query live call invalid UBERON (single-cell)", {
    skip_if_not(
        api_key_available(),
        "Skipping live API test because SYNTHESIZE_API_KEY is not set."
    )

    message("\nTesting live predict_query (single-cell) with invalid UBERON ID...")

    # Start with a valid example query and modify it to have an invalid UBERON ID
    invalid_query <- get_example_query(model_id = "gem-1-sc")$example_query
    invalid_query$inputs[[1]]$metadata$tissue_ontology_id <- "UBERON:9999999" # Invalid ID
    invalid_query$inputs[[1]]$num_samples <- 1
    invalid_query$seed <- 42

    # The API should reject this with an error
    expect_error(
        predict_query(
            query = invalid_query,
            model_id = "gem-1-sc",
            as_counts = TRUE
        ),
        "Model query failed"
    )
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

    message("Download URL flow and get_json validation passed.")
})

test_that("predict_query with total_count parameter works correctly", {
    skip_if_not(
        api_key_available(),
        "Skipping live API test because SYNTHESIZE_API_KEY is not set."
    )

    message("\nTesting predict_query with custom total_count parameter...")

    test_query <- get_example_query(model_id = "gem-1-bulk")$example_query
    test_query$total_count <- 5000000

    # Test with custom total_count
    results <- predict_query(
        query = test_query,
        model_id = "gem-1-bulk",
        as_counts = TRUE
    )

    expect_type(results, "list")
    expect_true("metadata" %in% names(results))
    expect_true("expression" %in% names(results))

    expect_s3_class(results$metadata, "data.frame")
    expect_s3_class(results$expression, "data.frame")

    expect_true(nrow(results$metadata) > 0)
    expect_true(nrow(results$expression) > 0)
    expect_true(ncol(results$expression) > 0)

    message("total_count parameter test passed.")
})

test_that("predict_query with deterministic_latents produces reproducible results", {
    skip_if_not(
        api_key_available(),
        "Skipping live API test because SYNTHESIZE_API_KEY is not set."
    )

    message("\nTesting predict_query with deterministic_latents for reproducibility...")

    test_query <- get_example_query(model_id = "gem-1-bulk")$example_query
    test_query$seed <- 12345 # Set a seed for consistency
    test_query$deterministic_latents <- TRUE

    # First call with deterministic_latents = TRUE
    results1 <- predict_query(
        query = test_query,
        model_id = "gem-1-bulk",
        as_counts = TRUE
    )

    # Second call with same query and deterministic_latents = TRUE
    results2 <- predict_query(
        query = test_query,
        model_id = "gem-1-bulk",
        as_counts = TRUE
    )

    expect_type(results1, "list")
    expect_type(results2, "list")

    # With deterministic_latents, results should be identical
    expect_equal(dim(results1$expression), dim(results2$expression),
        info = "Expression dimensions should match"
    )

    # Check that at least some values are identical (allowing for potential minor differences)
    # In practice, deterministic_latents should make results highly similar if not identical
    correlation <- cor(
        as.vector(as.matrix(results1$expression)),
        as.vector(as.matrix(results2$expression))
    )
    expect_true(correlation > 0.99,
        info = sprintf("With deterministic_latents, results should be highly correlated (got r=%.4f)", correlation)
    )

    message(sprintf("Deterministic latents test passed (correlation: %.6f)", correlation))
})

test_that("predict_query with deterministic_latents FALSE shows variation", {
    skip_if_not(
        api_key_available(),
        "Skipping live API test because SYNTHESIZE_API_KEY is not set."
    )

    message("\nTesting predict_query with deterministic_latents=FALSE shows variation...")

    test_query <- get_example_query(model_id = "gem-1-bulk")$example_query
    test_query$seed <- NULL # Remove seed to allow variation
    test_query$deterministic_latents <- FALSE

    # First call with deterministic_latents = FALSE
    results1 <- predict_query(
        query = test_query,
        model_id = "gem-1-bulk",
        as_counts = TRUE
    )

    # Second call with deterministic_latents = FALSE
    results2 <- predict_query(
        query = test_query,
        model_id = "gem-1-bulk",
        as_counts = TRUE
    )

    expect_type(results1, "list")
    expect_type(results2, "list")

    # With deterministic_latents = FALSE and no seed, results should show some variation
    expect_equal(dim(results1$expression), dim(results2$expression),
        info = "Expression dimensions should match"
    )

    # Calculate correlation - should be high but not perfect
    correlation <- cor(
        as.vector(as.matrix(results1$expression)),
        as.vector(as.matrix(results2$expression))
    )

    # Results should still be similar (same biological context) but not identical
    expect_true(correlation < 1.0,
        info = sprintf("Without deterministic_latents, results should show some variation (got r=%.4f)", correlation)
    )
    expect_true(correlation > 0.8,
        info = sprintf("Results should still be reasonably correlated (got r=%.4f)", correlation)
    )

    message(sprintf("Stochastic latents test passed (correlation: %.4f, showing expected variation)", correlation))
})

test_that("predict_query with total_count for single-cell", {
    skip_if_not(
        api_key_available(),
        "Skipping live API test because SYNTHESIZE_API_KEY is not set."
    )

    message("\nTesting predict_query with custom total_count for single-cell...")

    test_query <- get_example_query(model_id = "gem-1-sc")$example_query
    test_query$total_count <- 5000

    # Test with custom total_count (typical single-cell library size)
    results <- predict_query(
        query = test_query,
        model_id = "gem-1-sc",
        as_counts = TRUE
    )

    expect_type(results, "list")
    expect_true("metadata" %in% names(results))
    expect_true("expression" %in% names(results))

    expect_s3_class(results$metadata, "data.frame")
    expect_s3_class(results$expression, "data.frame")

    expect_true(nrow(results$metadata) > 0)
    expect_true(nrow(results$expression) > 0)
    expect_true(ncol(results$expression) > 0)

    message("Single-cell total_count parameter test passed.")
})

test_that("predict_query with both total_count and deterministic_latents", {
    skip_if_not(
        api_key_available(),
        "Skipping live API test because SYNTHESIZE_API_KEY is not set."
    )

    message("\nTesting predict_query with both total_count and deterministic_latents...")

    test_query <- get_example_query(model_id = "gem-1-bulk")$example_query
    test_query$total_count <- 8000000
    test_query$deterministic_latents <- TRUE

    # Test with both parameters
    results <- predict_query(
        query = test_query,
        model_id = "gem-1-bulk",
        as_counts = TRUE
    )

    expect_type(results, "list")
    expect_true("metadata" %in% names(results))
    expect_true("expression" %in% names(results))

    expect_s3_class(results$metadata, "data.frame")
    expect_s3_class(results$expression, "data.frame")

    expect_true(nrow(results$metadata) > 0)
    expect_true(nrow(results$expression) > 0)
    expect_true(ncol(results$expression) > 0)

    message("Combined parameters test passed.")
})
