mock_validation_response <- function(body, status = 422L) {
  structure(list(status_code = status, content = charToRaw(body),
                 headers = list(`Content-Type` = "application/json; charset=utf-8")),
            class = "response")
}

test_that("production validation is readable and lossless through predict_query", {
  original <- setup_test_environment()
  on.exit(restore_api_key(original))
  body <- paste(readLines(test_path("fixtures", "gem2-validation.json")), collapse = "\n")
  payload <- jsonlite::fromJSON(body, simplifyVector = FALSE)
  post <- mockery::mock(mock_validation_response(body))
  # Stub the nested POST through the public function to cover error propagation.
  mockery::stub(predict_query, "POST", post, depth = 2)
  poll <- mockery::mock(stop("Unexpected polling"))
  mockery::stub(predict_query, "poll_model_query", poll)
  error <- tryCatch(predict_query(list(inputs = list()), model_id = "gem-2-sc"),
                    synthesize_validation_error = identity)
  expect_s3_class(error, "synthesize_validation_error")
  expect_s3_class(error, "error")
  expect_s3_class(error, "simpleError")
  expect_identical(error$status_code, 422L)
  expect_identical(error$response_body, body)
  expect_identical(error$response_json, payload)
  lines <- vapply(payload$errors, function(x) paste0("- ", x$path, ": ", x$message), character(1))
  expect_identical(conditionMessage(error),
                   paste(c("Invalid GEM-2 request (422):", lines), collapse = "\n"))
  mockery::expect_called(post, 1)
  mockery::expect_called(poll, 0)
})

test_that("single errors and root paths support both validation statuses", {
  for (status in c(400L, 422L)) {
    for (path in c("inputs.items[1].metadata.age_years", "")) {
      payload <- list(errors = list(list(path = path, message = "Correct the value.", code = "x")))
      body <- jsonlite::toJSON(payload, auto_unbox = TRUE)
      error <- tryCatch(stop_api_error(mock_validation_response(body, status), "Predict"),
                        synthesize_validation_error = identity)
      expect_identical(error$status_code, status)
      expected_path <- if (nzchar(path)) path else "(request)"
      expect_identical(conditionMessage(error),
                       paste0("Invalid request (", status, "):\n- ", expected_path,
                              ": Correct the value."))
      expect_equal(error$response_json, payload)
    }
  }
})

test_that("malformed and legacy responses retain their complete fallback text", {
  bodies <- c(
    "Legacy validation error", "{broken json", "null", "[]", '{"errors": []}',
    '{"errors": {"path": "x", "message": "bad"}}', '{"errors": [null]}',
    '{"errors": [{"path": 1, "message": "bad"}]}',
    '{"errors": [{"path": "x", "message": ["bad"]}]}',
    '{"errors": [{"path": "x", "message": " "}]}',
    '{"errors": [{"path": "x", "message": "bad"}, {}]}',
    '{"errorsTypo": [{"path": "x", "message": "bad"}]}',
    '{"errors": [{"pathTypo": "x", "message": "bad"}]}'
  )
  for (body in bodies) {
    error <- tryCatch(stop_api_error(mock_validation_response(body), "Predict"),
                      synthesize_validation_error = identity)
    expect_identical(conditionMessage(error),
                     paste0("Predict request failed with status 422: ", body))
    expect_identical(error$response_body, body)
    parsed <- tryCatch(jsonlite::fromJSON(body, simplifyVector = FALSE), error = function(e) NULL)
    expect_identical(error$response_json, parsed)
  }
})

test_that("nonvalidation HTTP errors keep their original behavior", {
  expect_error(stop_api_error(mock_validation_response("Unauthorized", 401L), "Predict"),
               "Predict request failed with status 401: Unauthorized", class = "simpleError")
  expect_error(stop_api_error(mock_validation_response("Server error", 500L), "Predict"),
               "Predict request failed with status 500: Server error", class = "simpleError")
})
