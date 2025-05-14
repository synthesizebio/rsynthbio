#' @title Synthesize Bio API R Client
#' @description A package for interacting with the Synthesize Bio API to generate
#' gene expression data based on specified biological conditions.
#'
#' @importFrom httr POST add_headers content http_status status_code
#' @importFrom jsonlite toJSON fromJSON
#' @importFrom dplyr %>%
#' @importFrom tidyr pivot_longer
#'
#' @name synthesize
NULL

library(httr)
library(jsonlite)
library(dplyr)
library(tidyr)

#' @title API Base URL
#' @description Base URL for the Synthesize Bio API
#' @export
API_BASE_URL <- "https://app.synthesize.bio"

#' @title Model Modalities
#' @description A nested list containing supported modalities for different model versions
#' @format A nested list with structure: model type > version > modalities
#' @export
MODEL_MODALITIES <- list(
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



#' @title Synthesize Bio API R Client
#' @description A package for interacting with the Synthesize Bio API to generate
#' gene expression data based on specified biological conditions.
#' @import mockery
#' @export
mock_api_response <- function(success = TRUE, error = NULL, data = NULL) {
  if (success) {
    response <- list(
      status_code = 200,
      headers = list(`content-type` = "application/json"),
      content = charToRaw('{"status":"success"}')
    )
    class(response) <- "response"
    return(response)
  } else {
    response <- list(
      status_code = 400,
      headers = list(`content-type` = "application/json"),
      content = charToRaw('{"error":"Mock API error"}')
    )
    class(response) <- "response"
    return(response)
  }
}
