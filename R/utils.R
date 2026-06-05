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

#' @title Interpret an Environment Variable as a Boolean Flag (Internal)
#' @description Returns TRUE for the values 1/true/yes/on (case-insensitive,
#' surrounding whitespace ignored) and `default` when the variable is unset or
#' empty. Mirrors the truthy parsing used by the Python client (pysynthbio).
#' @param name The environment variable name.
#' @param default Logical value to return when the variable is unset/empty.
#' @return A length-one logical.
#' @keywords internal
env_flag <- function(name, default = FALSE) {
  value <- Sys.getenv(name, unset = NA_character_)
  if (is.na(value) || !nzchar(value)) {
    return(default)
  }
  tolower(trimws(value)) %in% c("1", "true", "yes", "on")
}

#' @title Resolve the API Base URL (Internal)
#' @description An explicit `api_base_url` always wins. Otherwise the value is
#' read from the `SYNTHESIZE_API_BASE_URL` environment variable, falling back to
#' the production default when that variable is unset/empty.
#' @param api_base_url An explicit base URL, or NULL to resolve from the
#'        environment.
#' @return A character scalar base URL.
#' @keywords internal
resolve_api_base_url <- function(api_base_url = NULL) {
  if (!is.null(api_base_url)) {
    return(api_base_url)
  }
  Sys.getenv("SYNTHESIZE_API_BASE_URL", unset = API_BASE_URL)
}

#' @title Resolve the Self-Hosted Flag (Internal)
#' @description An explicit `self_hosted` value always wins. Otherwise the flag
#' is read from the `SYNTHESIZE_SELF_HOSTED` environment variable (truthy for
#' 1/true/yes/on), defaulting to FALSE so the production async path is unchanged
#' when neither the argument nor the variable is set.
#' @param self_hosted An explicit logical, or NULL to resolve from the
#'        environment.
#' @return A length-one logical.
#' @keywords internal
resolve_self_hosted <- function(self_hosted = NULL) {
  if (!is.null(self_hosted)) {
    return(isTRUE(self_hosted))
  }
  env_flag("SYNTHESIZE_SELF_HOSTED")
}
