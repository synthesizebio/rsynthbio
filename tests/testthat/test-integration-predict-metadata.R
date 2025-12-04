library(testthat)
library(jsonlite)
library(httr)

# Helper function to check if API key is available
api_key_available <- function() {
    !is.na(Sys.getenv("SYNTHESIZE_API_KEY")) &&
        Sys.getenv("SYNTHESIZE_API_KEY") != ""
}

run_slow_tests <- function() {
    # This endpoint is scaled to zero, so only run this test when the env var is set
    tolower(Sys.getenv("RUN_SLOW_TESTS")) == "true"
}

test_that("predict metadata endpoint returns valid data", {
    skip_if_not(
        run_slow_tests(),
        "Skipping slow test because RUN_SLOW_TESTS is not set."
    )
    skip_if_not(
        api_key_available(),
        "Skipping live API test because SYNTHESIZE_API_KEY is not set."
    )

    query <- get_example_query(model_id = "gem-1-bulk_predict-metadata")$example_query
    results <- predict_query(query = query, model_id = "gem-1-bulk_predict-metadata")

    expect_type(results, "list")
    expect_true("outputs" %in% names(results))
    expect_type(results$outputs, "list")

    outputs <- results$outputs
    expect_true("classifier_probs" %in% names(outputs))
    expect_type(outputs$classifier_probs, "list")
    expect_s3_class(outputs$classifier_probs, "data.frame")
    expect_true(nrow(outputs$classifier_probs) > 0)
})
