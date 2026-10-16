# Hosted GEM-2 responses differ from GEM-1, especially metadata and reconstruction.
gem2_output_transformer <- function(model_id, query = list()) {
  if (identical(model_id, "gem-2")) {
    operation <- query$operation
  } else if (grepl("^gem-2-(bulk|sc)_reference-conditioning-(4w6l3p61|p3rz5xv7|x4s7hdmu)$", model_id)) {
    operation <- "predict-expression-from-reference"
  } else {
    pattern <- "^gem-2(?:-(bulk|sc))?(?:_(baseline|reference-conditioning|predict-metadata|metadata|autoencode))?(?:-ensemble)?$"
    matched <- regmatches(model_id, regexec(pattern, model_id, perl = TRUE))[[1]]
    if (length(matched) == 0L || (matched[2] == "" && matched[3] == "")) return(NULL)
    operation <- switch(matched[3],
      "reference-conditioning" = "predict-expression-from-reference",
      "predict-metadata" = "predict-metadata", "metadata" = "predict-metadata",
      "autoencode" = "autoencode", "predict-expression")
  }
  if (is.null(operation) || !(operation %in% c("predict-expression", "predict-expression-from-reference", "predict-metadata", "autoencode"))) return(NULL)
  function(content) transform_gem2_output(content, operation)
}

gem2_output_frame <- function(rows, field, list_columns = FALSE) {
  values <- lapply(rows, function(row) row[[field]])
  fields <- unique(unlist(lapply(values, names), use.names = FALSE))
  result <- data.frame(row.names = seq_along(rows))
  for (key in fields) {
    column <- lapply(values, function(value) value[[key]])
    scalar <- vapply(column, function(value) {
      is.null(value) || (is.atomic(value) && length(value) == 1L)
    }, logical(1))
    if (list_columns || !all(scalar)) {
      result[[key]] <- I(column)
    } else {
      result[[key]] <- unlist(lapply(column, function(value) if (is.null(value)) NA else value), use.names = FALSE)
    }
  }
  result
}

transform_gem2_output <- function(content, operation) {
  # Normalize jsonlite's nested data.frame and list representations, retaining
  # double precision and nullable metadata. No float32 conversion is involved.
  rows <- jsonlite::fromJSON(jsonlite::toJSON(content$outputs,
    dataframe = "rows", auto_unbox = TRUE, digits = NA, null = "null"), simplifyVector = FALSE)
  metadata <- gem2_output_frame(rows,
    if (operation == "predict-metadata") "predicted_metadata" else "metadata")
  metadata <- data.frame(sample_id = paste0("sample_", seq_along(rows)), metadata,
    check.names = FALSE)
  result <- list(metadata = metadata, latents = gem2_output_frame(rows, "latents", TRUE))
  if (operation == "predict-metadata") {
    result$classifier_probs <- gem2_output_frame(rows, "classifier_probabilities", TRUE)
    result$expression <- data.frame(row.names = seq_along(rows))
  } else {
    counts <- lapply(rows, function(row) as.numeric(unlist(row$counts, use.names = FALSE)))
    if (any(lengths(counts) != length(content$gene_order))) stop("GEM-2 counts do not match the gene order")
    result$expression <- as.data.frame(do.call(rbind, counts))
    colnames(result$expression) <- content$gene_order
    if (operation == "autoencode") result$provenance <- data.frame(
      model_release_id = vapply(rows, function(row) row$model_release_id, character(1)),
      vae_run_id = vapply(rows, function(row) row$vae_run_id, character(1)))
  }
  result
}
