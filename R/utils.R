#' @title API Base URL
#' @description Base URL for the Synthesize Bio API
#' @export
API_BASE_URL <- Sys.getenv("SYNTHESIZE_API_BASE_URL", unset = "https://app.synthesize.bio")

#' @title Default Timeout
#' @description Default timeout (seconds) for outbound HTTP requests
#' @export
DEFAULT_TIMEOUT <- 300

#' @title Default Poll Interval
#' @description Default polling interval (seconds) for async model queries
#' @export
DEFAULT_POLL_INTERVAL_SECONDS <- 2

#' @title Default Poll Timeout
#' @description Default maximum timeout (seconds) for async model queries
#' @export
DEFAULT_POLL_TIMEOUT_SECONDS <- 15 * 60
