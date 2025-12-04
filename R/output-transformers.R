#' Transform Standard Model Output (Internal)
#'
#' @description Extracts and combines gene expression data from a standard API response
#' (e.g., gem-1-bulk, gem-1-sc models).
#'
#' @param final_json The parsed API response list
#' @return A list with:
#'         - metadata: data.frame containing sample metadata
#'         - expression: data.frame containing combined gene expression data
#'         - latents: data.frame containing embeddings (if requested)
#' @keywords internal
transform_baseline_output <- function(final_json) {
  # Extract the expression matrices and combine them
  counts_list <- final_json$outputs$counts

  # Handle case where jsonlite converts to data.frame with one column of lists
  # This happens when the server returns an array of arrays
  if (is.data.frame(counts_list) && ncol(counts_list) == 1) {
    counts_list <- counts_list[[1]]
  }

  # Convert each sample's expression vector into a row of the expression matrix
  expression <- do.call(rbind, lapply(counts_list, function(x) as.numeric(x)))
  expression <- as.data.frame(expression)

  # Set gene names as column names
  colnames(expression) <- final_json$gene_order

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
#' @description Default output transformer, does not modify the response from the server.
#'
#' @param final_json The parsed API response list
#' @return the same final_json
#' @keywords internal
default_output_transformer <- function(final_json) {
  # return the entire output
  return (final_json)
}

#' Output Transformer Registry
#'
#' @description A registry mapping model IDs to their corresponding output transformer functions.
#' Models not in the registry will use the default standard transformer.
#' @keywords internal
OUTPUT_TRANSFORMERS <- list(
  "gem-1-bulk" = transform_baseline_output,
  "gem-1-sc" = transform_baseline_output,
  "gem-1-bulk_reference-conditioning" = transform_baseline_output,
  "gem-1-sc_reference-conditioning" = transform_baseline_output
)

#' Get Output Transformer for Model (Internal)
#'
#' @description Looks up the appropriate output transformer for a given model ID.
#' Returns the standard transformer as the default if no specific transformer is registered.
#'
#' @param model_id Character string specifying the model ID
#' @return A transformer function that accepts (final_json) and returns
#'         a list with metadata, expression, and optionally latents
#' @keywords internal
get_output_transformer <- function(model_id) {
  # Look up transformer in registry
  transformer <- OUTPUT_TRANSFORMERS[[model_id]]

  # If not found, use standard transformer as default
  if (is.null(transformer)) {
    transformer <- default_output_transformer
  }

  return(transformer)
}
