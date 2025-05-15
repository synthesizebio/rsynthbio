
#' @title API Base URL
#' @description Base URL for the Synthesize Bio API
#' @export
API_BASE_URL <- "https://app.synthesize.bio"

#' @title Model Modalities
#' @description A nested list containing supported modalities for different model versions
#' @format A nested list with structure: model type > version > modalities
#' @export
MODEL_MODALITIES <- list(
  "v1.0" = list(
    "bulk_rna-seq",
    "lincs",
    "sra",
    "single_cell_rna-seq",
    "microarray",
    "pseudo_bulk"
  )
)

utils::globalVariables(c("sample_group", "sample_index"))

