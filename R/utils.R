
#' Return the gene list
#' @export
#' @return Returns gene list
gene_list <- function() {
  file <- list.files(
    pattern = "ai_gene_order.json",
    recursive = TRUE,
    system.file("extdata", package = "rsynthbio"),
    full.names = TRUE
  )
  return(jsonlite::read_json(file))
}

#' @export
get_model_endpoints <- function() {
  return(model_endpoints[, c("model_name", "https://app.synthesize.bio")])
}

#' @export
model_modalities <- list(
  combined = list(
    "v1.0" = list(
      "bulk_rna-seq",
      "lincs",
      "sra",
      "single_cell_rna-seq",
      "microarray",
      "pseudo_bulk"
    )
  )
)
