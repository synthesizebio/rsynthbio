#' @title Arrow IPC Stream Content Type
#' @description MIME type used by the self-hosted container for synchronous
#' Apache Arrow IPC stream responses.
#' @noRd
ARROW_STREAM_CONTENT_TYPE <- "application/vnd.apache.arrow.stream"

#' @title Ensure the arrow Package is Available (Internal)
#' @description Stops with an actionable message when `self_hosted = TRUE` is
#' requested but the optional `arrow` package is not installed. The production
#' (async JSON) path never calls this, so `arrow` remains a soft dependency.
#' @return Invisibly TRUE when `arrow` is available; otherwise throws an error.
#' @noRd
require_arrow <- function() {
  if (!requireNamespace("arrow", quietly = TRUE)) {
    stop(
      paste0(
        "The 'arrow' package is required when self_hosted = TRUE. ",
        "Install it with install.packages(\"arrow\")."
      ),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

#' @title Build Headers for a Self-Hosted Arrow Request (Internal)
#' @description Requests an Apache Arrow IPC stream and only attaches an
#' `Authorization` header when an API key is configured, so the client works
#' against containers that run with authentication disabled.
#' @return An httr `request` object produced by [httr::add_headers()].
#' @importFrom httr add_headers
#' @noRd
build_arrow_post_headers <- function() {
  api_key <- Sys.getenv("SYNTHESIZE_API_KEY")
  if (nzchar(api_key)) {
    return(add_headers(
      Accept = ARROW_STREAM_CONTENT_TYPE,
      `Content-Type` = "application/json",
      Authorization = paste("Bearer", api_key)
    ))
  }
  add_headers(
    Accept = ARROW_STREAM_CONTENT_TYPE,
    `Content-Type` = "application/json"
  )
}

#' @title Read an Arrow IPC Stream into a Table (Internal)
#' @description Parses raw response bytes as an Apache Arrow IPC *stream* (not a
#' file) and returns an Arrow `Table` so schema-level metadata can be inspected.
#' @param raw_bytes A raw vector containing the Arrow IPC stream.
#' @return An Arrow `Table`.
#' @noRd
read_arrow_stream <- function(raw_bytes) {
  tryCatch(
    arrow::read_ipc_stream(raw_bytes, as_data_frame = FALSE),
    error = function(e) {
      stop(
        paste0("Failed to parse Arrow IPC stream from self-hosted response: ", e$message),
        call. = FALSE
      )
    }
  )
}

#' @title Extract Schema-Level Metadata from an Arrow Table (Internal)
#' @param table An Arrow `Table`.
#' @return A named list of metadata strings (possibly empty).
#' @noRd
arrow_schema_metadata <- function(table) {
  metadata <- table$schema$metadata
  if (is.null(metadata)) list() else metadata
}

#' @title Parse the gene_order Schema Metadata Value (Internal)
#' @description The container encodes `gene_order` as a JSON array of gene-id
#' strings. Returns NULL when absent (e.g. predict_metadata responses).
#' @param raw_value The raw `gene_order` metadata string, or NULL.
#' @return A character vector of gene ids, or NULL.
#' @importFrom jsonlite fromJSON
#' @noRd
parse_gene_order <- function(raw_value) {
  if (is.null(raw_value) || !nzchar(raw_value)) {
    return(NULL)
  }
  as.character(fromJSON(raw_value))
}

#' @title Extract a List Column from an Arrow Table as a Base R List (Internal)
#' @description Columns are pulled individually (rather than converting the whole
#' table) to avoid arrow's R-attribute metadata roundtrip, and coerced to a plain
#' list so downstream transformers see the same shape as the JSON path.
#' @param table An Arrow `Table`.
#' @param name The column name to extract.
#' @return A base R list, or NULL when the column is absent.
#' @noRd
arrow_list_column <- function(table, name) {
  if (!(name %in% names(table))) {
    return(NULL)
  }
  as.list(as.vector(table[[name]]))
}

#' @title Extract a Struct Column from an Arrow Table as a data.frame (Internal)
#' @param table An Arrow `Table`.
#' @param name The column name to extract.
#' @return A base data.frame, or NULL when the column is absent.
#' @noRd
arrow_struct_column <- function(table, name) {
  if (!(name %in% names(table))) {
    return(NULL)
  }
  as.data.frame(as.vector(table[[name]]))
}

#' @title Build a Baseline JSON-Shaped List from an Arrow Table (Internal)
#' @description Reshapes the per-row Arrow columns (`counts`, `metadata`, and the
#' three latent list columns) into the same nested list structure that
#' [transform_baseline_output()] consumes, so the JSON and Arrow paths share one
#' transformer and return an identical shape.
#' @param table An Arrow `Table`.
#' @param gene_order Character vector of gene ids (column order for `counts`).
#' @return A list with `gene_order` and `outputs` (counts, metadata, latents).
#' @noRd
build_baseline_json <- function(table, gene_order) {
  outputs <- list(
    counts = arrow_list_column(table, "counts"),
    metadata = arrow_struct_column(table, "metadata")
  )

  biological <- arrow_list_column(table, "biological_latent")
  technical <- arrow_list_column(table, "technical_latent")
  perturbation <- arrow_list_column(table, "perturbation_latent")

  if (!is.null(biological) && !is.null(technical) && !is.null(perturbation)) {
    outputs$latents <- data.frame(
      biological = I(biological),
      technical = I(technical),
      perturbation = I(perturbation)
    )
  }

  list(gene_order = gene_order, outputs = outputs)
}

#' @title Build a Metadata-Prediction JSON-Shaped List (Internal)
#' @description Reshapes predict_metadata Arrow columns into the structure that
#' [transform_metadata_output()] consumes.
#' @param table An Arrow `Table`.
#' @param gene_order Character vector of gene ids, or NULL.
#' @return A list with `gene_order` and `outputs`.
#' @noRd
build_metadata_json <- function(table, gene_order) {
  list(
    gene_order = gene_order,
    outputs = list(
      metadata = arrow_struct_column(table, "metadata"),
      latents = arrow_struct_column(table, "latents"),
      classifier_probs = arrow_struct_column(table, "classifier_probs"),
      decoder_sample = arrow_struct_column(table, "decoder_sample")
    )
  )
}

#' @title Transform a Parsed Arrow Table into Output Data Frames (Internal)
#' @description Reads `request_type`, `model_version`, and `gene_order` from the
#' Arrow schema metadata and dispatches to the matching existing transformer so
#' the return value matches the production JSON path exactly. `model_version` and
#' `request_type` are attached as attributes on the result.
#' @param table An Arrow `Table`.
#' @return A named list of data.frames (see [transform_baseline_output()] /
#'         [transform_metadata_output()]).
#' @noRd
transform_arrow_table <- function(table) {
  metadata <- arrow_schema_metadata(table)
  request_type <- metadata$request_type
  if (is.null(request_type) || !nzchar(request_type)) {
    stop("Arrow response schema is missing required 'request_type' metadata.", call. = FALSE)
  }

  gene_order <- parse_gene_order(metadata$gene_order)

  result <- switch(request_type,
    baseline = transform_baseline_output(build_baseline_json(table, gene_order)),
    reference_conditioned = transform_baseline_output(build_baseline_json(table, gene_order)),
    predict_metadata = transform_metadata_output(build_metadata_json(table, gene_order)),
    stop(paste0("Unsupported request_type in Arrow response: ", request_type), call. = FALSE)
  )

  attr(result, "model_version") <- metadata$model_version
  attr(result, "request_type") <- request_type
  result
}

#' @title Build the Raw (Unformatted) Self-Hosted Result (Internal)
#' @description Returns the parsed Arrow `Table` alongside the schema metadata
#' for callers that pass `raw_response = TRUE`.
#' @param table An Arrow `Table`.
#' @return A list with `table`, `model_version`, `request_type`, `gene_order`.
#' @noRd
arrow_raw_result <- function(table) {
  metadata <- arrow_schema_metadata(table)
  list(
    table = table,
    model_version = metadata$model_version,
    request_type = metadata$request_type,
    gene_order = parse_gene_order(metadata$gene_order)
  )
}

#' @title Run a Synchronous Self-Hosted Prediction (Internal)
#' @description Performs a single synchronous POST to a self-hosted model
#' container's predict endpoint (`/api/models/<model_id>/predict`), requesting an
#' Apache Arrow IPC stream, and converts the response into output data frames. Unlike
#' the production path this does NOT require an API key; a key is only attached
#' when one is configured.
#' @param query The query list to send as the JSON request body.
#' @param model_id Character string specifying the model ID.
#' @param api_base_url The base URL for the self-hosted container.
#' @param raw_response Logical; if TRUE, return the parsed Arrow `Table` and
#'        schema metadata without applying any transformer.
#' @return A named list of data.frames, or the raw Arrow result when
#'         `raw_response = TRUE`.
#' @importFrom httr POST content status_code timeout
#' @importFrom jsonlite toJSON
#' @noRd
predict_query_self_hosted <- function(query, model_id, api_base_url, raw_response = FALSE) {
  require_arrow()

  url <- paste0(api_base_url, "/api/models/", model_id, "/predict")
  query_json <- toJSON(query, auto_unbox = TRUE)

  response <- tryCatch(
    POST(
      url = url,
      build_arrow_post_headers(),
      body = query_json,
      encode = "json",
      timeout(SELF_HOSTED_TIMEOUT)
    ),
    error = function(e) {
      stop(paste0("Self-hosted predict request failed due to a network issue: ", e$message))
    }
  )

  if (status_code(response) >= 400) {
    stop(paste0(
      "Self-hosted predict request failed with status ",
      status_code(response), ": ", content(response, "text")
    ))
  }

  raw_bytes <- content(response, as = "raw")
  table <- read_arrow_stream(raw_bytes)

  if (isTRUE(raw_response)) {
    return(arrow_raw_result(table))
  }

  transform_arrow_table(table)
}
