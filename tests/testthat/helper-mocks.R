# Helper functions for mocking API calls in tests

#' Setup temporary API key for testing
#'
#' @param api_key The API key to set (default: "mock-api-key")
#' @return The original API key value (to be used in cleanup)
setup_mock_api_key <- function(api_key = "mock-api-key") {
    original_api_key <- Sys.getenv("SYNTHESIZE_API_KEY")
    Sys.setenv(SYNTHESIZE_API_KEY = api_key)
    original_api_key
}

#' Restore original API key
#'
#' @param original_api_key The original API key to restore
restore_api_key <- function(original_api_key) {
    if (original_api_key != "") {
        Sys.setenv(SYNTHESIZE_API_KEY = original_api_key)
    } else {
        Sys.unsetenv("SYNTHESIZE_API_KEY")
    }
}

#' Create standard mocks for a successful query
#'
#' @param query_id The query ID to return from start_model_query
#' @param download_url The download URL to return from poll
#' @param counts_list A list of count vectors, one per sample
#' @param metadata_df A data frame of metadata, one row per sample
#' @param gene_order The gene order to return from get_json
#' @return A list of mocks: has_token, start_query, poll, get_json
create_success_mocks <- function(query_id = "test-id",
                                 download_url = "https://example.com/data.json",
                                 counts_list = list(c(100, 200, 300)),
                                 metadata_df = data.frame(sample_id = "test1"),
                                 gene_order = c("gene1", "gene2", "gene3")) {
    list(
        has_token = mockery::mock(TRUE, cycle = TRUE),
        start_query = mockery::mock(query_id, cycle = TRUE),
        poll = mockery::mock(
            list(
                status = "ready",
                payload = list(
                    status = "ready",
                    downloadUrl = download_url
                )
            ),
            cycle = TRUE
        ),
        get_json = mockery::mock(
            list(
                outputs = list(
                    counts = list(counts = counts_list),
                    metadata = metadata_df
                ),
                gene_order = gene_order,
                model_version = 2
            ),
            cycle = TRUE
        )
    )
}

#' Create mocks for a failed query
#'
#' @param query_id The query ID to return from start_model_query
#' @param error_message The error message to return
#' @return A list of mocks: has_token, start_query, poll
create_failure_mocks <- function(query_id = "test-id",
                                 error_message = "Query failed") {
    list(
        has_token = mockery::mock(TRUE, cycle = TRUE),
        start_query = mockery::mock(query_id, cycle = TRUE),
        poll = mockery::mock(
            list(
                status = "failed",
                payload = list(
                    status = "failed",
                    message = error_message
                )
            ),
            cycle = TRUE
        )
    )
}

#' Create mocks for a timeout scenario
#'
#' @param query_id The query ID to return from start_model_query
#' @return A list of mocks: has_token, start_query, poll
create_timeout_mocks <- function(query_id = "test-id") {
    list(
        has_token = mockery::mock(TRUE, cycle = TRUE),
        start_query = mockery::mock(query_id, cycle = TRUE),
        poll = mockery::mock(
            list(
                status = "running",
                payload = list(status = "running")
            ),
            cycle = TRUE
        )
    )
}

#' Helper to setup environment for a test
#'
#' @param api_key The API key to set
#' @return The original API key (for cleanup)
setup_test_environment <- function(api_key = "mock-api-key") {
    setup_mock_api_key(api_key)
}
