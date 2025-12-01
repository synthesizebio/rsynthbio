#' Transform Standard Model Output (Internal)
#'
#' @description Extracts and combines gene expression data from a standard API response
#' (e.g., gem-1-bulk, gem-1-sc models). This is the default transformer for most models.
#'
#' @param final_json The parsed API response list
#' @return A list with:
#'         - metadata: data.frame containing sample metadata
#'         - expression: data.frame containing combined gene expression data
#'         - latents: data.frame containing embeddings (if requested)
#' @keywords internal
transform_standard_output <- function(final_json) {
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
#' @description Transforms output from metadata-enhanced models such as
#' gem-1-bulk_predict-metadata and gem-1-sc_predict-metadata. These models
#' provide additional metadata fields in their output.
#'
#' @param final_json The parsed API response list
#' @return A list with:
#'         - metadata: data.frame containing sample metadata (with enhanced fields)
#'         - expression: data.frame containing combined gene expression data
#'         - latents: data.frame containing embeddings (if requested)
#' @keywords internal
transform_metadata_output <- function(final_json) {
  # For now, use the same logic as standard output
  # In the future, this can be customized to handle additional metadata fields
  # that are specific to metadata-enhanced models
  transform_standard_output(final_json)
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
#' @return A transformer function that accepts (final_json) and returns
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
