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
#' @details High-level methods: run_trial(input, idempotency_key, timeout = 3600,
#'   poll_interval = 5) submits a live workflow and waits for a displayable report.
#'   Reuse the same key for retries; use a new key for intentional new inference.
#'   No local cache or automatic file writes occur. wait_for_trial(id, timeout,
#'   poll_interval) waits for an existing run without submitting. get_trial_result(id)
#'   fetches a completed report once without polling. Reports expose summary,
#'   biology, efficacy, safety, raw, run_id, ui_url, and save_json(path).
#'   A verdict_trial_timeout error retains run_id; resume with wait_for_trial.
#'   A verdict_trial_run_error retains run_id and status if a terminal run has no report.
#'   import_trial(nct_id = NULL, document = NULL, timeout = 210) proposes a trial
#'   draft from exactly one NCT ID or local PDF/DOCX/TXT file (up to 5 MiB).
#'   It returns draft and metadata with field evidence and review flags; it never
#'   submits an analysis. Streamed extraction errors raise verdict_api_error even
#'   with HTTP 200. Retrying performs fresh extraction, not run recovery.
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
  http_timeout <- timeout
  send <- function(method, path, body = NULL, idempotency_key = NULL, form = NULL, request_timeout = http_timeout) {
    headers <- c(Authorization = paste("Bearer", api_key))
    if (!is.null(idempotency_key)) headers <- c(headers, `Idempotency-Key` = idempotency_key)
    if (!is.null(body)) {
      # httr's JSON encoder turns NULL into {}, which violates nullable fields.
      body <- jsonlite::toJSON(body, auto_unbox = TRUE, null = "null", digits = 17)
      headers <- c(headers, `Content-Type` = "application/json")
    }
    response <- httr::VERB(method, paste0(base, "/api/v1/", path),
      httr::add_headers(.headers = headers), httr::timeout(request_timeout),
      httr::config(followlocation = 0L), body = if (is.null(form)) body else form,
      encode = if (is.null(form)) "raw" else "multipart")
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
  import_trial <- function(nct_id = NULL, document = NULL, timeout = 210) {
    if (is.null(nct_id) == is.null(document)) stop("Supply exactly one of nct_id or document", call. = FALSE)
    if (!is.numeric(timeout) || length(timeout) != 1L || !is.finite(timeout) || timeout <= 0)
      stop("timeout must be a positive finite number", call. = FALSE)
    if (!is.null(nct_id)) {
      if (!is.character(nct_id) || length(nct_id) != 1L || is.na(nct_id) || !nzchar(trimws(nct_id)))
        stop("nct_id must be a nonblank string", call. = FALSE)
      form <- list(nctId = nct_id)
    } else {
      if (!is.character(document) || length(document) != 1L || is.na(document) ||
          !grepl("\\.(pdf|docx|txt)$", document, ignore.case = TRUE))
        stop("document must be a PDF, DOCX, or TXT path", call. = FALSE)
      size <- file.info(document)$size
      if (is.na(size) || file.info(document)$isdir || size < 1 || size > 5 * 1024^2)
        stop("document must be a readable file between 1 byte and 5 MiB", call. = FALSE)
      form <- list(document = httr::upload_file(document))
    }
    result <- send("POST", "synthia/import", form = form, request_timeout = timeout)
    if (!is.list(result) || !is.null(result$error) || !is.list(result$result)) {
      stop(structure(list(message = paste("Synthia trial import failed:",
        jsonlite::toJSON(result, auto_unbox = TRUE)), call = NULL,
        status_code = 200L, details = result), class = c("verdict_api_error", "error", "condition")))
    }
    result$result
  }
  client <- list(
    import_trial = import_trial,
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
      if (!is.numeric(timeout) || length(timeout) != 1L || !is.finite(timeout) || timeout <= 0 ||
          !is.numeric(interval) || length(interval) != 1L || !is.finite(interval) || interval <= 0)
        stop("timeout and interval must be positive finite numbers")
      deadline <- proc.time()[["elapsed"]] + timeout
      expired <- function() stop(structure(list(
        message = paste("VERDICT run", id, "is still running; resume polling"),
        call = NULL, run_id = id), class = c("verdict_trial_timeout", "error", "condition")))
      repeat {
        remaining <- deadline - proc.time()[["elapsed"]]
        if (remaining <= 0) expired()
        budget <- if (http_timeout > 0) min(http_timeout, remaining) else remaining
        value <- tryCatch(send("GET", paste0("runs/", encode(id)), request_timeout = budget),
          error = function(error) {
            if (proc.time()[["elapsed"]] >= deadline ||
                (budget == remaining && inherits(error, "curl_error_operation_timedout"))) expired()
            stop(error)
          })
        remaining <- deadline - proc.time()[["elapsed"]]
        if (remaining <= 0) expired()
        if (!value$status %in% c("queued", "running")) return(value)
        Sys.sleep(min(interval, remaining))
      }
    }
  )
  polling_options <- function(timeout, poll_interval) {
    stopifnot(length(timeout) == 1, is.finite(timeout), timeout > 0,
      length(poll_interval) == 1, is.finite(poll_interval), poll_interval > 0)
  }
  client$get_trial_result <- function(id) {
    report <- client$result(id)
    .verdict_validate_report(report, id)
    .verdict_trial_result(report, base, "Retrieved from the VERDICT API")
  }
  client$wait_for_trial <- function(id, timeout = 3600, poll_interval = 5) {
    polling_options(timeout, poll_interval)
    status <- client$wait(id, timeout = timeout, interval = poll_interval)
    if (is.null(status$result)) stop(structure(list(message = paste("Trial", id, "has no report:", status$status, status$failureCode),
      call = NULL, run_id = id, status = status), class = c("verdict_trial_run_error", "error", "condition")))
    client$get_trial_result(id)
  }
  client$run_trial <- function(input, idempotency_key, timeout = 3600, poll_interval = 5) {
    polling_options(timeout, poll_interval)
    key <- validate_key(idempotency_key)
    execution <- client$workflows()$execution
    if (!identical(execution$mode, "live")) stop("Deployment is not in live mode; no trial was submitted")
    submitted <- client$submit_workflow(input, key)
    result <- client$wait_for_trial(submitted$id, timeout, poll_interval)
    .verdict_validate_report(result$raw, submitted$id, input)
    .verdict_trial_result(result$raw, base, "Retrieved from the VERDICT API", execution)
  }
  client

}
