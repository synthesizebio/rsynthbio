#' Log CPM Transformation (Internal)
#'
#' @description Transforms raw counts expression data into log1p(CPM) (Counts Per Million).
#' This is a common normalization method for gene expression data that accounts for
#' library size differences and applies a log transformation to reduce the effect of outliers.
#'
#' @param expression A data.frame containing raw counts expression data.
#' @return A data.frame containing log1p(CPM) transformed data.
#' @keywords internal
log_cpm <- function(expression) {
  # Check if the input is a data frame or matrix
  if (!is.data.frame(expression) && !is.matrix(expression)) {
    stop("Input must be a data frame or matrix.", call. = FALSE)
  }

  # Check if the input has more than zero rows and columns
  if (nrow(expression) == 0 || ncol(expression) == 0) {
    stop("Input must have at least one row and one column.", call. = FALSE)
  }

  # If there's a sample ID deal with it
  if ("sample_id" %in% colnames(expression)) {
    deal_with_sample_id <- TRUE
    sample_id <- expression$sample_id
    expression <- expression[, -1]
  } else {
    deal_with_sample_id <- FALSE
  }

  # Convert to matrix but drop sample id
  expr_matrix <- as.matrix(expression)

  # Replace NAs with 0
  expr_matrix[is.na(expr_matrix)] <- 0

  # Replace negative values with 0
  expr_matrix[expr_matrix < 0] <- 0

  # Calculate library size (row sums)
  library_size <- rowSums(expr_matrix)

  # Calculate CPM
  cpm_matrix <- t(t(expr_matrix) / library_size) * 1e6

  # Apply log1p transformation
  log_cpm_matrix <- log1p(cpm_matrix)

  # Return as data frame with appropriate column names
  if (deal_with_sample_id) {
    result <- data.frame(
      sample_id = sample_id,
      as.data.frame(log_cpm_matrix)
    )
    colnames(result) <- c(
      "sample_id",
      paste0(colnames(expression), "_cpm")
    )
  } else {
    result <- as.data.frame(log_cpm_matrix)
    colnames(result) <- colnames(expression)
  }

  return(result)
}

#' Transform Standard Model Output (Internal)
#'
#' @description Extracts and combines gene expression data from a standard API response
#' (e.g., gem-1-bulk, gem-1-sc models). This is the default transformer for most models.
#'
#' @param final_json The parsed API response list
#' @param as_counts Logical, if FALSE, transforms the predicted expression counts into logCPM
#'        (default is TRUE, returning raw counts).
#' @return A list with:
#'         - metadata: data.frame containing sample metadata
#'         - expression: data.frame containing combined gene expression data
#'         - latents: data.frame containing embeddings (if requested)
#' @keywords internal
transform_standard_output <- function(final_json, as_counts = TRUE) {
  # Extract the expression matrices and combine them
  counts_list <- final_json$outputs$counts

  # Convert each sample's expression vector into a row of the expression matrix
  expression <- do.call(rbind, lapply(counts_list, function(x) as.numeric(x)))
  expression <- as.data.frame(expression)

  # Set gene names as column names
  colnames(expression) <- final_json$gene_order

  # Apply log CPM transformation if requested
  if (!as_counts) {
    expression <- log_cpm(expression)
  }

  # Get metadata dataframe
  metadata <- final_json$outputs$metadata

  # Add sample identifiers to match the expression data
  metadata <- data.frame(
    sample_id = paste0("sample_", seq_len(nrow(metadata))),
    metadata
  )

  # if latents - add those too
  if ("latents" %in% names(final_json$outputs)) {
    final_list <- list(
      metadata = metadata,
      expression = expression,
      latents = final_json$outputs$latents
    )
  } else {
    final_list <- list(
      metadata = metadata,
      expression = expression
    )
  }
  # Return both components
  return(final_list)
}

#' Transform Metadata Model Output (Internal)
#'
#' @description Transforms output from metadata-enhanced models such as
#' gem-1-bulk_predict-metadata and gem-1-sc_predict-metadata. These models
#' provide additional metadata fields in their output.
#'
#' @param final_json The parsed API response list
#' @param as_counts Logical, if FALSE, transforms the predicted expression counts into logCPM
#'        (default is TRUE, returning raw counts).
#' @return A list with:
#'         - metadata: data.frame containing sample metadata (with enhanced fields)
#'         - expression: data.frame containing combined gene expression data
#'         - latents: data.frame containing embeddings (if requested)
#' @keywords internal
transform_metadata_output <- function(final_json, as_counts = TRUE) {
  # For now, use the same logic as standard output
  # In the future, this can be customized to handle additional metadata fields
  # that are specific to metadata-enhanced models
  transform_standard_output(final_json, as_counts = as_counts)
}

#' Output Transformer Registry
#'
#' @description A registry mapping model IDs to their corresponding output transformer functions.
#' Models not in the registry will use the default standard transformer.
#' @keywords internal
OUTPUT_TRANSFORMERS <- list(
  "gem-1-bulk_predict-metadata" = transform_metadata_output,
  "gem-1-sc_predict-metadata" = transform_metadata_output
  # Default fallback: NULL means use transform_standard_output
)

#' Get Output Transformer for Model (Internal)
#'
#' @description Looks up the appropriate output transformer for a given model ID.
#' Returns the standard transformer as the default if no specific transformer is registered.
#'
#' @param model_id Character string specifying the model ID
#' @return A transformer function that accepts (final_json, as_counts) and returns
#'         a list with metadata, expression, and optionally latents
#' @keywords internal
get_output_transformer <- function(model_id) {
  # Look up transformer in registry
  transformer <- OUTPUT_TRANSFORMERS[[model_id]]

  # If not found, use standard transformer as default
  if (is.null(transformer)) {
    transformer <- transform_standard_output
  }

  return(transformer)
}
