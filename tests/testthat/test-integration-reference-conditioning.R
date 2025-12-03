library(testthat)
library(jsonlite)
library(httr)

# Helper function to check if API key is available
api_key_available <- function() {
    !is.na(Sys.getenv("SYNTHESIZE_API_KEY")) &&
        Sys.getenv("SYNTHESIZE_API_KEY") != ""
}

run_slow_tests <- function() {
    # this endpoint is scaled to zero, so only run this test when the env var is set
    tolower(Sys.getenv("RUN_SLOW_TESTS")) == "true"
}

test_that("reference conditioning endpoint returns valid data", {
    skip_if_not(
        run_slow_tests(),
        "Skipping slow test because RUN_SLOW_TESTS is not set."
    )
    run_slow_tests()
    skip_if_not(
        api_key_available(),
        "Skipping live API test because SYNTHESIZE_API_KEY is not set."
    )

    query <- get_example_query(model_id = "gem-1-bulk_reference-conditioning")$example_query
    results <- predict_query(query = query, model_id = "gem-1-bulk_reference-conditioning")

    expect_type(results, "list")

    expect_true("metadata" %in% names(results))
    expect_type(results$metadata, "list")
    expect_s3_class(results$metadata, "data.frame")
    expect_true(nrow(results$metadata) > 0)
    expect_true("latents" %in% names(results))
    expect_type(results$latents, "list")
    expect_s3_class(results$latents, "data.frame")
    expect_true(nrow(results$latents) > 0)
    expect_true(ncol(results$latents) > 0)
})
