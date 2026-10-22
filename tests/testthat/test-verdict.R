test_that("VERDICT rejects plaintext credentials and does not change Platform defaults", {
  expect_error(verdict_client("key", "http://example.org"), "HTTPS")
  expect_error(verdict_client("key", "https://user:password@example.org"), "HTTPS")
  expect_error(verdict_client(""), "VERDICT API key")
  expect_true(is.function(verdict_client("key")$predict))
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
