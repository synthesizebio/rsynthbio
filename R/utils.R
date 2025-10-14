RELEASE_NUMBER <- "v3.0"

#' @title API Base URL
#' @description Base URL for the Synthesize Bio API
#' @export
API_BASE_URL <- "https://app.synthesize.bio"

#' @title Model Modalities
#' @description A nested list containing supported modalities for different
#' model versions
#' + bulk = bulk RNA-seq
#' + single-cell = single-cell RNA-seq
#' @format A nested list with structure: model type > version > modalities
#' @export
MODEL_MODALITIES <- list()
MODEL_MODALITIES[[RELEASE_NUMBER]] <- c("bulk", "single-cell")

#' @title Default Timeout
#' @description Default timeout (seconds) for outbound HTTP requests
#' @export
DEFAULT_TIMEOUT <- 30

#' @title Default Poll Interval
#' @description Default polling interval (seconds) for async model queries
#' @export
DEFAULT_POLL_INTERVAL_SECONDS <- 2

#' @title Default Poll Timeout
#' @description Default maximum timeout (seconds) for async model queries
#' @export
DEFAULT_POLL_TIMEOUT_SECONDS <- 15 * 60
