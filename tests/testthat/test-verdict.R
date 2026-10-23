test_that("VERDICT rejects plaintext credentials and does not change Platform defaults", {
  expect_error(verdict_client("key", "http://example.org"), "HTTPS")
  expect_error(verdict_client("key", "https://user:password@example.org"), "HTTPS")
  expect_error(verdict_client(""), "VERDICT API key")
  expect_true(is.function(verdict_client("key")$predict))
})

test_that("VERDICT rejects malformed success responses and unusable submission keys", {
  reply <- structure(list(url = "https://verdict.synthesize.bio/api/v1/models", status_code = 200L, headers = list(`content-type` = "text/plain"),
    content = charToRaw("upstream unavailable")), class = "response")
  mockery::stub(verdict_client, "httr::VERB", function(...) reply)
  client <- verdict_client("key")
  failure <- tryCatch(client$result("saved-id"), verdict_api_error = function(e) e)
  expect_s3_class(failure, "verdict_api_error")
  expect_identical(failure$details$body, "upstream unavailable")
  reply$status_code <- 502L
  failure <- tryCatch(client$models(), verdict_api_error = function(e) e)
  expect_identical(failure$status_code, 502L)
  expect_identical(failure$details$body, "upstream unavailable")
  for (key in list(NULL, NA_character_, "", "  ", c("a", "b"))) {
    expect_error(client$predict("safety", "predict", list(), key), "idempotency_key")
    expect_error(client$submit_workflow(list(), key), "idempotency_key")
  }
})

test_that("VERDICT preserves model JSON and structured validation errors", {
  reply <- structure(list(url = "https://verdict.synthesize.bio/api/v1/models", status_code = 200L, headers = list(`content-type` = "application/json"),
    content = charToRaw('{"result":{"unsupported":true,"release":"pilot"}}')),
    class = "response")
  mockery::stub(verdict_client, "httr::VERB", function(...) reply)
  client <- verdict_client("key")
  expect_true(client$models()$result$unsupported)
  expect_identical(client$result("saved-id")$result$release, "pilot")
  reply$status_code <- 422L
  failure <- tryCatch(client$models(), verdict_api_error = function(e) e)
  expect_s3_class(failure, "verdict_api_error")
  expect_identical(failure$status_code, 422L)
  expect_true(failure$details$result$unsupported)
})

test_that("VERDICT sends the API contract with credentials and redirect protection", {
  calls <- list()
  reply <- structure(list(url = "https://verdict.synthesize.bio", status_code = 200L,
    headers = list(`content-type` = "application/json"), content = charToRaw("{}")), class = "response")
  mockery::stub(verdict_client, "httr::VERB", function(verb, url, ...) {
    calls[[length(calls) + 1L]] <<- list(verb = verb, url = url, args = list(...))
    reply
  })
  client <- verdict_client("vdt_test")
  input <- list(inputs = list(list(name = "example", max_age_years = NULL,
    value = 0.123456789012345, source_ids = list("source"), empty = list())))
  client$predict("safety", "predict_adverse_event_incidence", input, "model-1")
  client$submit_workflow(input, "trial-1")
  client$models()
  client$example("gem-2", "autoencode")
  expect_identical(vapply(calls, `[[`, "", "verb"), c("POST", "POST", "GET", "GET"))
  expect_identical(vapply(calls, `[[`, "", "url"), paste0("https://verdict.synthesize.bio/api/v1/",
    c("models/safety/predict", "workflows", "models", "models/gem-2/example-query?operation=autoencode")))
  expect_identical(jsonlite::fromJSON(calls[[1]]$args$body, simplifyVector = FALSE),
    list(operation = "predict_adverse_event_incidence", input = input))
  expect_identical(calls[[1]]$args[[1]]$headers[["Idempotency-Key"]], "model-1")
  expect_identical(jsonlite::fromJSON(calls[[2]]$args$body, simplifyVector = FALSE),
    list(input = input, idempotencyKey = "trial-1"))
  for (call in calls[1:2]) {
    expect_identical(call$args$encode, "raw")
    expect_identical(call$args[[1]]$headers[["Content-Type"]], "application/json")
  }
  for (call in calls) {
    expect_identical(call$args[[1]]$headers[["Authorization"]], "Bearer vdt_test")
    expect_identical(call$args[[3]]$options$followlocation, 0L)
  }
})

test_that("VERDICT polling resumes saved IDs and caps waits at the deadline", {
  clock <- 0
  sleeps <- numeric()
  statuses <- c("queued", "running", "complete")
  index <- 0L
  urls <- character()
  mockery::stub(verdict_client, "proc.time", function() c(elapsed = clock))
  mockery::stub(verdict_client, "Sys.sleep", function(seconds) {
    sleeps <<- c(sleeps, seconds)
    clock <<- clock + seconds
  })
  mockery::stub(verdict_client, "httr::VERB", function(verb, url, ...) {
    expect_identical(verb, "GET")
    urls <<- c(urls, url)
    index <<- index + 1L
    status <- statuses[[min(index, length(statuses))]]
    structure(list(url = url, status_code = 200L, headers = list(`content-type` = "application/json"),
      content = charToRaw(paste0('{"status":"', status, '"}'))), class = "response")
  })
  client <- verdict_client("key")
  expect_identical(client$wait("saved-id")$status, "complete")
  expect_identical(sleeps, c(5, 5))
  statuses <- "failed"
  expect_identical(client$wait("saved-id")$status, "failed")
  statuses <- "running"
  sleeps <- numeric()
  polls_before <- length(urls)
  expect_error(client$wait("saved-id", timeout = 2, interval = 5), "resume polling")
  expect_identical(sleeps, 2)
  expect_length(urls, polls_before + 1L)
  statuses <- "complete"
  expect_identical(client$wait("saved-id")$status, "complete")
  expect_true(all(urls == "https://verdict.synthesize.bio/api/v1/runs/saved-id"))
  expect_error(client$wait("saved-id", timeout = 0), "positive")
  expect_error(client$wait("saved-id", interval = 0), "positive")
})
