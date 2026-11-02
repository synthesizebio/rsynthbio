# File/display contracts; no simulated HTTP responses or model predictions.
test_that("saved trial reports preserve data and print without Jupyter", {
  report <- list(run_id = "saved-report", status = "unavailable",
    request = list(trial = list(name = "<script>example</script>", endpoints = list())),
    components = list(), analyses = list(list(analysis = "biology", status = "unavailable", issues = list())),
    values = list(0.011582433985350042, 1.2345678901234567e-100, 1 + 2^-52))
  path <- tempfile(fileext = ".json")
  on.exit(unlink(path))
  writeLines(jsonlite::toJSON(report, auto_unbox = TRUE, null = "null", digits = 17), path)
  result <- load_trial_result(path)
  expect_identical(result$raw, report)
  expect_identical(result$biology$data$analysis$status, "unavailable")
  expect_null(result$efficacy$data$analysis)
  expect_match(result$summary$html, "&lt;script&gt;", fixed = TRUE)
  expect_output(print(result$summary), "offline")
  result$save_json(path)
  expect_identical(load_trial_result(path)$raw, report)
  expect_error(load_trial(path), "workflow input")
})

test_that("trial files retain nulls, arrays, and numeric precision", {
  trial <- list(trial = list(endpoints = list(), age = NULL, value = 0.123456789012345),
    requested_analyses = list("biology"))
  path <- tempfile(fileext = ".json")
  on.exit(unlink(path))
  writeLines(jsonlite::toJSON(trial, auto_unbox = TRUE, null = "null", digits = NA), path)
  expect_identical(load_trial(path), trial)
  expect_error(load_trial_result(path), "trial result")
})

test_that("report validation compares the JSON request rather than R container types", {
  input <- list(requested_analyses = c("biology", "toxicity"),
    trial = list(endpoints = list(list(outcome_analysis = list(value = 1L))), age = NULL))
  wire <- jsonlite::fromJSON(jsonlite::toJSON(input, auto_unbox = TRUE, null = "null", digits = 17), simplifyVector = FALSE)
  wire$trial$endpoints[[1]]$outcome_analysis$endpoint_role <- "primary"
  report <- list(run_id = "run", request = wire, components = list(), analyses = list(), status = "complete")
  expect_no_error(rsynthbio:::.verdict_validate_report(report, "run", input))
  report$request$requested_analyses <- rev(report$request$requested_analyses)
  expect_error(rsynthbio:::.verdict_validate_report(report, "run", input), "does not match")
})
