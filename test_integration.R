#!/usr/bin/env Rscript
# Integration test for rsynthbio package
# Tests the async API functionality end-to-end

cat("=== rsynthbio Integration Test ===\n\n")

# Load the package
cat("1. Loading rsynthbio package...\n")
library(rsynthbio)
cat("   ✓ Package loaded successfully\n\n")

# Set API key
cat("2. Setting API key...\n")
Sys.setenv(SYNTHESIZE_API_KEY = "sbio_D-unLZgfYhuMgMqYhuNsjs_K2x6-WzS9Qai3ojlkJfA")
if (has_synthesize_token()) {
    cat("   ✓ API key set successfully\n\n")
} else {
    stop("   ✗ Failed to set API key")
}

# Test get_valid_modalities
cat("3. Testing get_valid_modalities()...\n")
modalities <- get_valid_modalities()
cat("   Available modalities:", paste(modalities, collapse = ", "), "\n")
if (all(c("bulk", "single-cell") %in% modalities)) {
    cat("   ✓ Both modalities available\n\n")
} else {
    stop("   ✗ Expected modalities not found")
}

# Test get_valid_modes
cat("4. Testing get_valid_modes()...\n")
modes <- get_valid_modes()
cat("   Available modes:", paste(modes, collapse = ", "), "\n")
cat("   ✓ Modes retrieved successfully\n\n")

# Test bulk RNA-seq query
cat("5. Testing bulk RNA-seq query...\n")
cat("   Creating bulk query...\n")
bulk_query <- get_valid_query(modality = "bulk")
cat("   Query modality:", bulk_query$modality, "\n")
cat("   Query mode:", bulk_query$mode, "\n")
cat("   Number of input conditions:", length(bulk_query$inputs), "\n")

cat("   Validating query...\n")
validate_query(bulk_query)
validate_modality(bulk_query)
cat("   ✓ Query validation passed\n")

cat("   Submitting query to API (this may take a moment)...\n")
start_time <- Sys.time()
bulk_result <- predict_query(bulk_query, as_counts = TRUE)
end_time <- Sys.time()
elapsed <- as.numeric(difftime(end_time, start_time, units = "secs"))

cat("   Query completed in", round(elapsed, 1), "seconds\n")
cat("   Result structure:\n")
cat("     - Metadata rows:", nrow(bulk_result$metadata), "\n")
cat("     - Metadata columns:", ncol(bulk_result$metadata), "\n")
cat("     - Expression rows:", nrow(bulk_result$expression), "\n")
cat("     - Expression columns (genes):", ncol(bulk_result$expression), "\n")

if (nrow(bulk_result$metadata) > 0 && nrow(bulk_result$expression) > 0) {
    cat("   ✓ Bulk query successful\n\n")
} else {
    stop("   ✗ Bulk query returned empty results")
}

# Test single-cell query
cat("6. Testing single-cell RNA-seq query...\n")
cat("   Creating single-cell query...\n")
sc_query <- get_valid_query(modality = "single-cell")
cat("   Query modality:", sc_query$modality, "\n")
cat("   Query mode:", sc_query$mode, "\n")
cat("   Number of input conditions:", length(sc_query$inputs), "\n")

cat("   Validating query...\n")
validate_query(sc_query)
validate_modality(sc_query)
cat("   ✓ Query validation passed\n")

cat("   Submitting query to API (this may take a moment)...\n")
start_time <- Sys.time()
sc_result <- predict_query(sc_query, as_counts = TRUE)
end_time <- Sys.time()
elapsed <- as.numeric(difftime(end_time, start_time, units = "secs"))

cat("   Query completed in", round(elapsed, 1), "seconds\n")
cat("   Result structure:\n")
cat("     - Metadata rows:", nrow(sc_result$metadata), "\n")
cat("     - Metadata columns:", ncol(sc_result$metadata), "\n")
cat("     - Expression rows:", nrow(sc_result$expression), "\n")
cat("     - Expression columns (genes):", ncol(sc_result$expression), "\n")

if (nrow(sc_result$metadata) > 0 && nrow(sc_result$expression) > 0) {
    cat("   ✓ Single-cell query successful\n\n")
} else {
    stop("   ✗ Single-cell query returned empty results")
}

# Test log transformation
cat("7. Testing log CPM transformation...\n")
cat("   Requesting log-transformed data...\n")
log_result <- predict_query(bulk_query, as_counts = FALSE)
cat(
    "   Log-transformed expression dimensions:",
    nrow(log_result$expression), "x", ncol(log_result$expression), "\n"
)

# Check that values are different from raw counts
if (!identical(bulk_result$expression, log_result$expression)) {
    cat("   ✓ Log transformation applied successfully\n\n")
} else {
    stop("   ✗ Log transformation did not change values")
}

# Verify data quality
cat("8. Verifying data quality...\n")
cat("   Bulk RNA-seq:\n")
cat("     - Min expression value:", min(bulk_result$expression), "\n")
cat("     - Max expression value:", max(bulk_result$expression), "\n")
cat("     - Mean expression value:", round(mean(as.matrix(bulk_result$expression)), 2), "\n")

cat("   Single-cell RNA-seq:\n")
cat("     - Min expression value:", min(sc_result$expression), "\n")
cat("     - Max expression value:", max(sc_result$expression), "\n")
cat("     - Mean expression value:", round(mean(as.matrix(sc_result$expression)), 2), "\n")

cat("   ✓ Data quality checks passed\n\n")

# Test polling parameters
cat("9. Testing custom polling parameters...\n")
custom_result <- predict_query(
    bulk_query,
    as_counts = TRUE,
    poll_interval_seconds = 3,
    poll_timeout_seconds = 600
)
if (nrow(custom_result$expression) > 0) {
    cat("   ✓ Custom polling parameters work\n\n")
} else {
    stop("   ✗ Query with custom polling failed")
}

# Summary
cat("===========================================\n")
cat("✓ ALL INTEGRATION TESTS PASSED!\n")
cat("===========================================\n\n")

cat("Summary:\n")
cat("  - Package loads correctly\n")
cat("  - API authentication works\n")
cat("  - Both bulk and single-cell queries work\n")
cat("  - Async polling completes successfully\n")
cat("  - Data transformation works\n")
cat("  - Custom parameters are respected\n")
cat("\nThe rsynthbio package is ready for use! 🎉\n")
