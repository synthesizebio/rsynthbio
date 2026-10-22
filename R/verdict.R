#' Connect to the VERDICT model and trial API
#'
#' Returns methods for discovery, examples, asynchronous model and workflow
#' submission, polling and saved results. Scientific JSON is returned as lists;
#' unavailable values and model provenance are preserved. Existing Platform
#' functions and their defaults are unchanged. Create a key in VERDICT at
#' \url{https://verdict.synthesize.bio/app/api-keys}.
#'
#' @param api_key VERDICT key, or the VERDICT_API_KEY environment variable.
#' @param api_base_url HTTPS origin, or VERDICT_API_BASE_URL.
#' @param timeout HTTP timeout in seconds.
#' @return A list of methods: models(), example(model, operation),
#'   predict(model, operation, input, idempotency_key, release), run(id),
#'   result(id), workflows(), submit_workflow(input, idempotency_key),
#'   and wait(id, timeout, interval). Supply a stable idempotency key when retrying
#'   submissions. wait() returns terminal failures as well as successful runs.
#' @export
verdict_client <- function(api_key = Sys.getenv("VERDICT_API_KEY"),
                           api_base_url = Sys.getenv("VERDICT_API_BASE_URL", "https://verdict.synthesize.bio"),
                           timeout = 60) {
  if (!is.character(api_key) || length(api_key) != 1L || !nzchar(api_key)) {
    stop("Supply a VERDICT API key or set VERDICT_API_KEY", call. = FALSE)
  }
  parsed <- httr::parse_url(api_base_url)
  if (!identical(parsed$scheme, "https") || is.null(parsed$hostname) ||
      !is.null(parsed$username) || !is.null(parsed$password) ||
      length(parsed$query) || !is.null(parsed$fragment)) {
    stop("api_base_url must be an HTTPS origin without credentials", call. = FALSE)
  }
  base <- sub("/+$", "", api_base_url)
  encode <- function(value) utils::URLencode(value, reserved = TRUE)
  validate_key <- function(value) {
    if (!is.character(value) || length(value) != 1L || is.na(value) ||
        !nzchar(trimws(value)) || nchar(value) > 180L) {
      stop("idempotency_key must be one nonblank string of at most 180 characters", call. = FALSE)
    }
    value
  }
  send <- function(method, path, body = NULL, idempotency_key = NULL) {
    headers <- c(Authorization = paste("Bearer", api_key))
    if (!is.null(idempotency_key)) headers <- c(headers, `Idempotency-Key` = idempotency_key)
    response <- httr::VERB(method, paste0(base, "/api/v1/", path),
      httr::add_headers(.headers = headers), httr::timeout(timeout),
      httr::config(followlocation = 0L), body = body, encode = "json")
    raw_body <- httr::content(response, "text", encoding = "UTF-8")
    result <- tryCatch(jsonlite::fromJSON(raw_body, simplifyVector = FALSE), error = function(e) e)
    malformed <- inherits(result, "error")
    if (malformed) result <- list(error = "non_json_response", body = raw_body)
    status <- httr::status_code(response)
    if (malformed || status < 200L || status >= 300L) {
      failure <- structure(list(message = paste("VERDICT HTTP", status,
        jsonlite::toJSON(result, auto_unbox = TRUE)), call = NULL,
        status_code = status, details = result),
        class = c("verdict_api_error", "error", "condition"))
      stop(failure)
    }
    result
  }
  run <- function(id) send("GET", paste0("runs/", encode(id)))
  list(
    models = function() send("GET", "models"),
    example = function(model, operation = NULL) {
      path <- paste0("models/", encode(model), "/example-query")
      if (!is.null(operation)) path <- paste0(path, "?operation=", encode(operation))
      send("GET", path)
    },
    predict = function(model, operation, input, idempotency_key, release = NULL) {
      idempotency_key <- validate_key(idempotency_key)
      body <- list(operation = operation, input = input)
      if (!is.null(release)) body$release <- release
      send("POST", paste0("models/", encode(model), "/predict"), body, idempotency_key)
    },
    run = run,
    result = function(id) send("GET", paste0("runs/", encode(id), "/result")),
    workflows = function() send("GET", "workflows"),
    submit_workflow = function(input, idempotency_key) {
      idempotency_key <- validate_key(idempotency_key)
      send("POST", "workflows", list(input = input, idempotencyKey = idempotency_key))
    },
    wait = function(id, timeout = 1800, interval = 5) {
      if (timeout <= 0 || interval <= 0) stop("timeout and interval must be positive")
      deadline <- proc.time()[["elapsed"]] + timeout
      repeat {
        value <- run(id)
        if (!value$status %in% c("queued", "running")) return(value)
        remaining <- deadline - proc.time()[["elapsed"]]
        if (remaining <= 0) stop(paste("VERDICT run", id, "is still running; resume polling"))
        Sys.sleep(min(interval, remaining))
      }
    }
  )
}
