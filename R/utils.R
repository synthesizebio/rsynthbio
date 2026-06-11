#' @title API Base URL
#' @description Base URL for the Synthesize Bio API
#' @export
API_BASE_URL <- Sys.getenv("SYNTHESIZE_API_BASE_URL", unset = "https://app.synthesize.bio")

#' @title Default Timeout
#' @description Default timeout (seconds) for outbound HTTP requests
#' @export
DEFAULT_TIMEOUT <- 300

#' @title Self-Hosted Timeout
#' @description Timeout (seconds) for synchronous self-hosted container
#' predictions. These run on the partner's GPU box and can take minutes for
#' large sample counts, so they use a longer timeout than hosted control-plane
#' calls.
#' @export
SELF_HOSTED_TIMEOUT <- 600

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
#' @noRd
env_flag <- function(name, default = FALSE) {
  value <- Sys.getenv(name, unset = NA_character_)
  if (is.na(value) || !nzchar(value)) {
    return(default)
  }
  tolower(trimws(value)) %in% c("1", "true", "yes", "on")
}

# A model's variant slugs (reference-conditioning, predict-metadata) are served
# by the same container as their base model, so they resolve to the same host.
MODEL_ID_SUFFIXES <- c("_reference-conditioning", "_predict-metadata")

#' @title Reduce a Model Slug to its Base Model (Internal)
#' @description Strips the variant suffixes so all slugs backed by one container
#' map to the same per-model environment variable.
#' @param model_id A model slug.
#' @return The base model id.
#' @noRd
base_model_id <- function(model_id) {
  for (suffix in MODEL_ID_SUFFIXES) {
    if (endsWith(model_id, suffix)) {
      return(substr(model_id, 1, nchar(model_id) - nchar(suffix)))
    }
  }
  model_id
}

#' @title Per-Model Base URL Environment Variable Name (Internal)
#' @description Builds the env var that holds the self-hosted base URL for a
#' specific model, e.g. `gem-1-bulk` (and its variants) ->
#' `SYNTHESIZE_API_BASE_URL__GEM_1_BULK`. Mirrors the pysynthbio naming.
#' @param model_id A model slug.
#' @return The environment variable name.
#' @noRd
per_model_env_var <- function(model_id) {
  key <- gsub("[^A-Z0-9]+", "_", toupper(base_model_id(model_id)))
  paste0("SYNTHESIZE_API_BASE_URL__", key)
}

#' @title Resolve the API Base URL (Internal)
#' @description Resolves the base URL in precedence order: an explicit
#' `api_base_url` argument, then the per-model environment variable
#' `SYNTHESIZE_API_BASE_URL__<MODEL>` (when `model_id` is supplied), then the
#' global `SYNTHESIZE_API_BASE_URL`, then the production default. The per-model
#' variable lets a scientist point each model at its own self-hosted container
#' once and never pass a URL on every call.
#' @param api_base_url An explicit base URL, or NULL to resolve from the
#'        environment.
#' @param model_id Optional model slug used to look up a per-model variable.
#' @return A character scalar base URL.
#' @noRd
resolve_api_base_url <- function(api_base_url = NULL, model_id = NULL) {
  if (!is.null(api_base_url)) {
    return(api_base_url)
  }
  if (!is.null(model_id)) {
    per_model <- Sys.getenv(per_model_env_var(model_id), unset = NA_character_)
    if (!is.na(per_model) && nzchar(per_model)) {
      return(per_model)
    }
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
#' @noRd
resolve_self_hosted <- function(self_hosted = NULL) {
  if (!is.null(self_hosted)) {
    return(isTRUE(self_hosted))
  }
  env_flag("SYNTHESIZE_SELF_HOSTED")
}
