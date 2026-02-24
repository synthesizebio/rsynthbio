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
#' @description Extracts metadata prediction outputs and converts them to
#' data.frames for metadata, latents, classifier_probs, and expression
#' (from decoder_sample counts).
#'
#' Uses the same direct column-access pattern as transform_baseline_output,
#' since jsonlite with simplifyDataFrame=TRUE converts the outputs array
#' into a columnar structure.
#'
#' @param final_json The parsed API response list
#' @return A list with:
#'         - metadata: data.frame of predicted metadata
#'         - latents: data.frame with biological/technical/perturbation columns
#'         - classifier_probs: data.frame of per-category probability dicts
#'         - expression: data.frame of decoder sample counts
#' @keywords internal
transform_metadata_output <- function(final_json) {
  outputs <- final_json$outputs

  metadata <- outputs$metadata
  latents <- outputs$latents
  classifier_probs <- outputs$classifier_probs

  # Extract expression counts from decoder_sample.
  # Each output's decoder_sample is {"counts": [...]}.
  # jsonlite may simplify this to a data.frame with a "counts" column,
  # or keep it as a nested list.
  decoder_sample <- outputs$decoder_sample
  if (is.data.frame(decoder_sample)) {
    counts_list <- decoder_sample$counts
  } else if (is.list(decoder_sample)) {
    counts_list <- lapply(decoder_sample, function(x) x$counts)
  } else {
    counts_list <- list()
  }

  # Handle jsonlite wrapping counts in a single-column data.frame
  if (is.data.frame(counts_list) && ncol(counts_list) == 1) {
    counts_list <- counts_list[[1]]
  }

  expression <- do.call(rbind, lapply(counts_list, function(x) as.numeric(x)))
  expression <- as.data.frame(expression)

  gene_order <- final_json$gene_order
  if (!is.null(gene_order)) {
    colnames(expression) <- gene_order
  }

  return(list(
    metadata = metadata,
    latents = latents,
    classifier_probs = classifier_probs,
    expression = expression
  ))
}

#' Output Transformer Registry
#'
#' @description A registry mapping model IDs to their corresponding output transformer functions.
#' @keywords internal
OUTPUT_TRANSFORMERS <- list(
  "gem-1-bulk" = transform_baseline_output,
  "gem-1-sc" = transform_baseline_output,
  "gem-1-bulk_reference-conditioning" = transform_baseline_output,
  "gem-1-sc_reference-conditioning" = transform_baseline_output,
  "gem-1-bulk_condition-on-sample-ids" = transform_baseline_output,
  "gem-1-bulk_predict-metadata" = transform_metadata_output,
  "gem-1-sc_predict-metadata" = transform_metadata_output
)

#' Get Output Transformer for Model (Internal)
#'
#' @description Looks up the appropriate output transformer for a given model ID.
#' Returns NULL if no transformer is registered.
#'
#' @param model_id Character string specifying the model ID
#' @return A transformer function, or NULL if not registered
#' @keywords internal
get_output_transformer <- function(model_id) {
  return(OUTPUT_TRANSFORMERS[[model_id]])
}
