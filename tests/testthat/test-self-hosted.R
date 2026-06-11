library(testthat)
library(mockery)
library(jsonlite)

# -----------------------------------------------------------------------------
# Helpers: build in-memory Arrow IPC stream fixtures that mimic the self-hosted
# container's synchronous responses (schema-level metadata + per-row columns).
# -----------------------------------------------------------------------------

make_baseline_stream <- function(gene_order,
                                 counts_list,
                                 metadata_df,
                                 request_type = "baseline",
                                 model_version = "gem-1-bulk-v2",
                                 with_latents = TRUE) {
  n <- length(counts_list)
  columns <- list(
    counts = counts_list,
    metadata = metadata_df
  )
  if (with_latents) {
    columns$biological_latent <- replicate(n, c(0.1, 0.2, 0.3), simplify = FALSE)
    columns$technical_latent <- replicate(n, c(1.1, 1.2), simplify = FALSE)
    columns$perturbation_latent <- replicate(n, c(2.1, 2.2, 2.3, 2.4), simplify = FALSE)
  }

  tab <- do.call(arrow::arrow_table, columns)
  tab <- tab$ReplaceSchemaMetadata(list(
    model_version = model_version,
    request_type = request_type,
    gene_order = as.character(jsonlite::toJSON(gene_order))
  ))
  arrow::write_to_raw(tab, format = "stream")
}

make_metadata_stream <- function(counts_list,
                                 metadata_df,
                                 model_version = "gem-1-bulk-predict-metadata-v2") {
  n <- length(counts_list)
  latents_df <- data.frame(
    biological = I(replicate(n, c(0.1, 0.2), simplify = FALSE)),
    technical = I(replicate(n, c(1.1, 1.2), simplify = FALSE)),
    perturbation = I(replicate(n, c(2.1, 2.2), simplify = FALSE))
  )
  classifier_df <- data.frame(
    sex = I(data.frame(
      male = runif(n),
      female = runif(n)
    ))
  )
  decoder_df <- data.frame(counts = I(counts_list))

  tab <- arrow::arrow_table(
    classifier_probs = classifier_df,
    latents = latents_df,
    metadata = metadata_df,
    decoder_sample = decoder_df
  )
  tab <- tab$ReplaceSchemaMetadata(list(
    model_version = model_version,
    request_type = "predict_metadata"
  ))
  arrow::write_to_raw(tab, format = "stream")
}

# -----------------------------------------------------------------------------
# Baseline / reference_conditioned Arrow parsing
# -----------------------------------------------------------------------------

test_that("transform_arrow_table parses a baseline response (mocked Arrow stream)", {
  skip_if_not_installed("arrow")

  gene_order <- c("ENSG00000000003", "ENSG00000000005", "ENSG00000000419")
  counts_list <- list(
    as.integer(c(100, 200, 300)),
    as.integer(c(150, 250, 350))
  )
  metadata_df <- data.frame(
    sample_type = c("cell line", "primary tissue"),
    sex = c("male", "female"),
    stringsAsFactors = FALSE
  )

  raw_bytes <- make_baseline_stream(gene_order, counts_list, metadata_df)
  table <- rsynthbio:::read_arrow_stream(raw_bytes)
  result <- rsynthbio:::transform_arrow_table(table)

  expect_named(result, c("metadata", "expression", "latents"))

  # Expression: gene_order column names and correct ordering of counts
  expect_s3_class(result$expression, "data.frame")
  expect_equal(colnames(result$expression), gene_order)
  expect_equal(nrow(result$expression), 2)
  expect_equal(as.numeric(result$expression[1, ]), c(100, 200, 300))
  expect_equal(as.numeric(result$expression[2, ]), c(150, 250, 350))

  # Metadata: sample_id prepended, original columns preserved
  expect_s3_class(result$metadata, "data.frame")
  expect_equal(nrow(result$metadata), 2)
  expect_true("sample_id" %in% colnames(result$metadata))
  expect_equal(result$metadata$sex, c("male", "female"))

  # Latents reshaped to biological/technical/perturbation list columns
  expect_s3_class(result$latents, "data.frame")
  expect_named(result$latents, c("biological", "technical", "perturbation"))
  expect_equal(length(result$latents$biological[[1]]), 3)

  # Schema metadata exposed as attributes (shape unchanged)
  expect_equal(attr(result, "model_version"), "gem-1-bulk-v2")
  expect_equal(attr(result, "request_type"), "baseline")
})

test_that("transform_arrow_table handles reference_conditioned the same as baseline", {
  skip_if_not_installed("arrow")

  gene_order <- c("gene1", "gene2", "gene3")
  raw_bytes <- make_baseline_stream(
    gene_order,
    counts_list = list(as.integer(c(1, 2, 3))),
    metadata_df = data.frame(sample_type = "cell line", stringsAsFactors = FALSE),
    request_type = "reference_conditioned"
  )
  result <- rsynthbio:::transform_arrow_table(rsynthbio:::read_arrow_stream(raw_bytes))

  expect_equal(colnames(result$expression), gene_order)
  expect_equal(attr(result, "request_type"), "reference_conditioned")
})

test_that("transform_arrow_table omits latents when absent", {
  skip_if_not_installed("arrow")

  raw_bytes <- make_baseline_stream(
    gene_order = c("g1", "g2"),
    counts_list = list(as.integer(c(5, 6))),
    metadata_df = data.frame(sex = "male", stringsAsFactors = FALSE),
    with_latents = FALSE
  )
  result <- rsynthbio:::transform_arrow_table(rsynthbio:::read_arrow_stream(raw_bytes))

  expect_named(result, c("metadata", "expression"))
})

# -----------------------------------------------------------------------------
# predict_metadata Arrow parsing
# -----------------------------------------------------------------------------

test_that("transform_arrow_table parses a predict_metadata response", {
  skip_if_not_installed("arrow")

  counts_list <- list(
    as.integer(c(10, 20, 30)),
    as.integer(c(40, 50, 60))
  )
  metadata_df <- data.frame(
    sex = c("male", "female"),
    sample_type = c("cell line", "primary tissue"),
    stringsAsFactors = FALSE
  )

  raw_bytes <- make_metadata_stream(counts_list, metadata_df)
  result <- rsynthbio:::transform_arrow_table(rsynthbio:::read_arrow_stream(raw_bytes))

  expect_named(result, c("metadata", "latents", "classifier_probs", "expression"))
  expect_s3_class(result$expression, "data.frame")
  expect_equal(nrow(result$expression), 2)
  expect_equal(ncol(result$expression), 3)
  expect_equal(as.numeric(result$expression[1, ]), c(10, 20, 30))
  expect_s3_class(result$metadata, "data.frame")
  expect_equal(nrow(result$metadata), 2)
  expect_equal(attr(result, "request_type"), "predict_metadata")
})

# -----------------------------------------------------------------------------
# Error handling
# -----------------------------------------------------------------------------

test_that("transform_arrow_table errors on missing request_type metadata", {
  skip_if_not_installed("arrow")

  tab <- arrow::arrow_table(
    counts = list(as.integer(c(1, 2))),
    metadata = data.frame(sex = "male", stringsAsFactors = FALSE)
  )
  raw_bytes <- arrow::write_to_raw(tab, format = "stream")
  table <- rsynthbio:::read_arrow_stream(raw_bytes)

  expect_error(
    rsynthbio:::transform_arrow_table(table),
    "missing required 'request_type'"
  )
})

test_that("transform_arrow_table errors on unsupported request_type", {
  skip_if_not_installed("arrow")

  tab <- arrow::arrow_table(
    counts = list(as.integer(c(1, 2))),
    metadata = data.frame(sex = "male", stringsAsFactors = FALSE)
  )
  tab <- tab$ReplaceSchemaMetadata(list(request_type = "something_new"))
  raw_bytes <- arrow::write_to_raw(tab, format = "stream")

  expect_error(
    rsynthbio:::transform_arrow_table(rsynthbio:::read_arrow_stream(raw_bytes)),
    "Unsupported request_type.*something_new"
  )
})

test_that("require_arrow errors with an install hint when arrow is missing", {
  require_arrow_fn <- rsynthbio:::require_arrow
  stub(require_arrow_fn, "requireNamespace", function(...) FALSE)

  expect_error(
    require_arrow_fn(),
    "install.packages\\(\"arrow\"\\)"
  )
})

# -----------------------------------------------------------------------------
# predict_query_self_hosted end-to-end (HTTP mocked, real Arrow parsing)
# -----------------------------------------------------------------------------

test_that("predict_query_self_hosted parses a baseline stream without an API key", {
  skip_if_not_installed("arrow")

  original_api_key <- Sys.getenv("SYNTHESIZE_API_KEY")
  Sys.unsetenv("SYNTHESIZE_API_KEY")
  on.exit({
    if (nzchar(original_api_key)) Sys.setenv(SYNTHESIZE_API_KEY = original_api_key)
  })

  gene_order <- c("g1", "g2", "g3")
  raw_bytes <- make_baseline_stream(
    gene_order,
    counts_list = list(as.integer(c(7, 8, 9))),
    metadata_df = data.frame(sex = "male", stringsAsFactors = FALSE)
  )

  fn <- rsynthbio:::predict_query_self_hosted
  stub(fn, "POST", structure(list(status_code = 200), class = "response"))
  stub(fn, "status_code", function(x) 200)
  stub(fn, "content", function(x, as) raw_bytes)

  result <- fn(
    query = list(inputs = list()),
    model_id = "gem-1-bulk",
    api_base_url = "https://gem-1-bulk.internal.partner.example",
    raw_response = FALSE
  )

  expect_equal(colnames(result$expression), gene_order)
  expect_equal(as.numeric(result$expression[1, ]), c(7, 8, 9))
})

test_that("predict_query_self_hosted returns the raw Arrow table when requested", {
  skip_if_not_installed("arrow")

  gene_order <- c("g1", "g2")
  raw_bytes <- make_baseline_stream(
    gene_order,
    counts_list = list(as.integer(c(3, 4))),
    metadata_df = data.frame(sex = "female", stringsAsFactors = FALSE)
  )

  fn <- rsynthbio:::predict_query_self_hosted
  stub(fn, "POST", structure(list(status_code = 200), class = "response"))
  stub(fn, "status_code", function(x) 200)
  stub(fn, "content", function(x, as) raw_bytes)

  result <- fn(
    query = list(inputs = list()),
    model_id = "gem-1-bulk",
    api_base_url = "https://host",
    raw_response = TRUE
  )

  expect_equal(result$request_type, "baseline")
  expect_equal(result$gene_order, gene_order)
  expect_true(inherits(result$table, "Table"))
})

test_that("predict_query_self_hosted raises on HTTP errors", {
  skip_if_not_installed("arrow")

  fn <- rsynthbio:::predict_query_self_hosted
  stub(fn, "POST", structure(list(status_code = 500), class = "response"))
  stub(fn, "status_code", function(x) 500)
  stub(fn, "content", function(x, ...) "Internal Server Error")

  expect_error(
    fn(
      query = list(inputs = list()),
      model_id = "gem-1-bulk",
      api_base_url = "https://host",
      raw_response = FALSE
    ),
    "Self-hosted predict request failed with status 500"
  )
})

test_that("predict_query dispatches to the self-hosted path when self_hosted = TRUE", {
  dispatch <- mockery::mock(list(dispatched = TRUE))
  stub(predict_query, "predict_query_self_hosted", dispatch)

  result <- predict_query(
    query = list(inputs = list()),
    model_id = "gem-1-bulk",
    api_base_url = "https://host",
    self_hosted = TRUE
  )

  expect_true(result$dispatched)
  expect_called(dispatch, 1)
})

# -----------------------------------------------------------------------------
# make_api_request self-hosted (no token) behaviour
# -----------------------------------------------------------------------------

test_that("make_api_request does not require a token in self_hosted mode", {
  original_api_key <- Sys.getenv("SYNTHESIZE_API_KEY")
  Sys.unsetenv("SYNTHESIZE_API_KEY")
  on.exit({
    if (nzchar(original_api_key)) Sys.setenv(SYNTHESIZE_API_KEY = original_api_key)
  })

  fn <- rsynthbio:::make_api_request
  stub(fn, "GET", structure(list(status_code = 200), class = "response"))
  stub(fn, "status_code", function(x) 200)
  stub(fn, "content", function(x, type) '{"ok":true}')

  result <- fn("https://host/api/models", "List models", self_hosted = TRUE)
  expect_true(result$ok)
})

test_that("make_api_request still requires a token in the production path", {
  original_api_key <- Sys.getenv("SYNTHESIZE_API_KEY")
  Sys.unsetenv("SYNTHESIZE_API_KEY")
  on.exit({
    if (nzchar(original_api_key)) Sys.setenv(SYNTHESIZE_API_KEY = original_api_key)
  })

  expect_error(
    rsynthbio:::make_api_request("https://host/api/models", "List models"),
    "set your API key"
  )
})

# -----------------------------------------------------------------------------
# Environment-variable defaults (parity with the Python client)
# -----------------------------------------------------------------------------

test_that("env_flag parses truthy values case-insensitively", {
  for (truthy in c("1", "true", "TRUE", "Yes", " on ", "ON")) {
    Sys.setenv(RSYNTHBIO_TEST_FLAG = truthy)
    expect_true(rsynthbio:::env_flag("RSYNTHBIO_TEST_FLAG"), info = truthy)
  }
  for (falsy in c("0", "false", "no", "off", "", "nope")) {
    Sys.setenv(RSYNTHBIO_TEST_FLAG = falsy)
    expect_false(rsynthbio:::env_flag("RSYNTHBIO_TEST_FLAG"), info = falsy)
  }
  Sys.unsetenv("RSYNTHBIO_TEST_FLAG")
  expect_false(rsynthbio:::env_flag("RSYNTHBIO_TEST_FLAG"))
  expect_true(rsynthbio:::env_flag("RSYNTHBIO_TEST_FLAG", default = TRUE))
})

test_that("resolve_api_base_url prefers explicit arg, then env var, then default", {
  original <- Sys.getenv("SYNTHESIZE_API_BASE_URL", unset = NA_character_)
  on.exit({
    if (is.na(original)) Sys.unsetenv("SYNTHESIZE_API_BASE_URL") else Sys.setenv(SYNTHESIZE_API_BASE_URL = original)
  })

  Sys.setenv(SYNTHESIZE_API_BASE_URL = "https://env.example")
  expect_equal(rsynthbio:::resolve_api_base_url("https://explicit.example"), "https://explicit.example")
  expect_equal(rsynthbio:::resolve_api_base_url(NULL), "https://env.example")

  Sys.unsetenv("SYNTHESIZE_API_BASE_URL")
  expect_equal(rsynthbio:::resolve_api_base_url(NULL), API_BASE_URL)
})

test_that("per_model_env_var builds the model-specific variable name", {
  expect_equal(rsynthbio:::per_model_env_var("gem-1-bulk"), "SYNTHESIZE_API_BASE_URL__GEM_1_BULK")
  expect_equal(rsynthbio:::per_model_env_var("gem-1-sc"), "SYNTHESIZE_API_BASE_URL__GEM_1_SC")
  # Variant slugs normalize to their base model's variable.
  expect_equal(
    rsynthbio:::per_model_env_var("gem-1-bulk_predict-metadata"),
    "SYNTHESIZE_API_BASE_URL__GEM_1_BULK"
  )
  expect_equal(
    rsynthbio:::per_model_env_var("gem-1-sc_reference-conditioning"),
    "SYNTHESIZE_API_BASE_URL__GEM_1_SC"
  )
})

test_that("resolve_api_base_url routes per-model without per-call URLs", {
  saved <- c("SYNTHESIZE_API_BASE_URL", "SYNTHESIZE_API_BASE_URL__GEM_1_BULK", "SYNTHESIZE_API_BASE_URL__GEM_1_SC")
  originals <- vapply(saved, function(v) Sys.getenv(v, unset = NA_character_), character(1))
  on.exit({
    for (v in saved) {
      if (is.na(originals[[v]])) Sys.unsetenv(v) else do.call(Sys.setenv, setNames(list(originals[[v]]), v))
    }
  })

  Sys.unsetenv("SYNTHESIZE_API_BASE_URL")
  Sys.setenv(SYNTHESIZE_API_BASE_URL__GEM_1_BULK = "http://bulk:8080")
  Sys.setenv(SYNTHESIZE_API_BASE_URL__GEM_1_SC = "http://sc:8080")

  # Each model resolves to its own host.
  expect_equal(rsynthbio:::resolve_api_base_url(model_id = "gem-1-bulk"), "http://bulk:8080")
  expect_equal(rsynthbio:::resolve_api_base_url(model_id = "gem-1-sc"), "http://sc:8080")
  # Variant slugs share the base model's host.
  expect_equal(rsynthbio:::resolve_api_base_url(model_id = "gem-1-bulk_predict-metadata"), "http://bulk:8080")
  expect_equal(rsynthbio:::resolve_api_base_url(model_id = "gem-1-sc_reference-conditioning"), "http://sc:8080")
  # Explicit arg still wins over the per-model env var.
  expect_equal(
    rsynthbio:::resolve_api_base_url("http://explicit:9000", model_id = "gem-1-bulk"),
    "http://explicit:9000"
  )

  # Per-model wins over the global, but a model without its own var uses the global.
  Sys.setenv(SYNTHESIZE_API_BASE_URL = "http://global:8080")
  expect_equal(rsynthbio:::resolve_api_base_url(model_id = "gem-1-sc"), "http://sc:8080")
  Sys.unsetenv("SYNTHESIZE_API_BASE_URL__GEM_1_BULK")
  expect_equal(rsynthbio:::resolve_api_base_url(model_id = "gem-1-bulk"), "http://global:8080")
})

test_that("resolve_self_hosted prefers explicit arg over env var", {
  original <- Sys.getenv("SYNTHESIZE_SELF_HOSTED", unset = NA_character_)
  on.exit({
    if (is.na(original)) Sys.unsetenv("SYNTHESIZE_SELF_HOSTED") else Sys.setenv(SYNTHESIZE_SELF_HOSTED = original)
  })

  Sys.setenv(SYNTHESIZE_SELF_HOSTED = "true")
  expect_false(rsynthbio:::resolve_self_hosted(FALSE))
  expect_true(rsynthbio:::resolve_self_hosted(NULL))

  Sys.setenv(SYNTHESIZE_SELF_HOSTED = "off")
  expect_false(rsynthbio:::resolve_self_hosted(NULL))
  expect_true(rsynthbio:::resolve_self_hosted(TRUE))

  Sys.unsetenv("SYNTHESIZE_SELF_HOSTED")
  expect_false(rsynthbio:::resolve_self_hosted(NULL))
})

test_that("predict_query enables self-hosted from SYNTHESIZE_SELF_HOSTED env var", {
  original_flag <- Sys.getenv("SYNTHESIZE_SELF_HOSTED", unset = NA_character_)
  original_url <- Sys.getenv("SYNTHESIZE_API_BASE_URL", unset = NA_character_)
  on.exit({
    if (is.na(original_flag)) Sys.unsetenv("SYNTHESIZE_SELF_HOSTED") else Sys.setenv(SYNTHESIZE_SELF_HOSTED = original_flag)
    if (is.na(original_url)) Sys.unsetenv("SYNTHESIZE_API_BASE_URL") else Sys.setenv(SYNTHESIZE_API_BASE_URL = original_url)
  })

  Sys.setenv(SYNTHESIZE_SELF_HOSTED = "1")
  Sys.setenv(SYNTHESIZE_API_BASE_URL = "https://gem-1-bulk.internal.partner.example")

  dispatch <- mockery::mock(list(dispatched = TRUE))
  stub(predict_query, "predict_query_self_hosted", dispatch)

  result <- predict_query(query = list(inputs = list()), model_id = "gem-1-bulk")

  expect_true(result$dispatched)
  expect_called(dispatch, 1)
  # The resolved base URL from the env var is forwarded to the self-hosted path.
  call_args <- mockery::mock_args(dispatch)[[1]]
  expect_equal(call_args$api_base_url, "https://gem-1-bulk.internal.partner.example")
})

test_that("list_models resolves base URL from the env var when not supplied", {
  original_url <- Sys.getenv("SYNTHESIZE_API_BASE_URL", unset = NA_character_)
  on.exit({
    if (is.na(original_url)) Sys.unsetenv("SYNTHESIZE_API_BASE_URL") else Sys.setenv(SYNTHESIZE_API_BASE_URL = original_url)
  })
  Sys.setenv(SYNTHESIZE_API_BASE_URL = "https://gem-1-bulk.internal.partner.example")

  seen_url <- NULL
  stub(list_models, "make_api_request", function(url, context_msg, ...) {
    seen_url <<- url
    list(ok = TRUE)
  })

  list_models()
  expect_equal(seen_url, "https://gem-1-bulk.internal.partner.example/api/models")
})
