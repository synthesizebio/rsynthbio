# Offline design editing, review/run orchestration with a fake client, and
# readable result tables from a saved report. No network requests.

hcc_trial <- function() {
  trial <- Trial$new("HCC phase 3")
  trial$population$set_disease("hepatocellular carcinoma")
  trial$edit(phase = "phase 3", allocation = "randomized", blinding = "open label", min_age = 18)
  trial$arms$add("Durvalumab", type = "experimental", treatments = "durvalumab")
  trial$arms$add("Sorafenib", type = "active comparator",
    treatments = list(list(name = "sorafenib", dose = "400 mg twice daily")))
  trial$endpoints$add("Overall survival", family = "overall survival",
    effect_measure = "hazard ratio", hypothesis = "superiority")
  trial
}

fixture <- function(name) {
  jsonlite::fromJSON(test_path("fixtures", name), simplifyVector = FALSE)
}

test_that("vocabulary matching is forgiving and errors list the allowed values", {
  choose <- rsynthbio:::.trial_choose
  expect_identical(choose("phase", "Phase 3"), "PHASE3")
  expect_identical(choose("phase", "3"), "PHASE3")
  expect_identical(choose("phase", "early phase 1"), "EARLY_PHASE1")
  expect_identical(choose("blinding", "open label"), "NONE")
  expect_identical(choose("arm_type", "Active comparator"), "active_comparator")
  expect_identical(choose("arm_type", "placebo"), "placebo_comparator")
  expect_identical(choose("effect_measure", "Hazard Ratio"), "hazard_ratio")
  expect_null(choose("sex", NULL))
  expect_error(choose("blinding", "sorta blind"), "DOUBLE", class = "trial_design_error")
  expect_true("QUADRUPLE" %in% trial_vocabulary("blinding"))
  expect_error(trial_vocabulary("nope"), "known")
  expect_output(print(trial_vocabulary()), "blinding: NONE")
  fields <- trial_fields()
  expect_true(all(c("details.phases", "arms[].treatments[].chembl", "comparisons[]") %in% fields$field))
  expect_match(fields$allowed_values[fields$field == "population.sex"], "female")
})

test_that("designs build the clinical-design contract and round-trip", {
  trial <- hcc_trial()
  trial$details$edit(phase = "Phase 1/2", start_date = "2024-03")
  design <- trial$to_list()
  expect_identical(design$schemaVersion, "verdict.clinical-design/v1")
  expect_identical(design$population$phases, list("PHASE1", "PHASE2"))
  expect_identical(design$metadata$blinding, "NONE")
  expect_identical(design$arms[[2]]$components,
    list(list(name = "sorafenib", doseNotes = "400 mg twice daily", doseStated = TRUE)))
  expect_identical(design$endpoints[[1]]$effectMeasure, "hazard_ratio")
  # Two arms and no explicit comparison: the server builds the default one.
  expect_null(design$comparisons)
  expect_identical(design$requestedAnalyses, list("efficacy", "toxicity", "biology"))
  wire <- jsonlite::fromJSON(trial$to_json(), simplifyVector = FALSE)
  expect_true("sex" %in% names(wire$population))
  expect_null(wire$population$sex)
  expect_identical(wire$population$phases, list("PHASE1", "PHASE2"))
  path <- tempfile(fileext = ".json")
  on.exit(unlink(path))
  trial$save(path)
  expect_identical(load_trial_design(path)$to_json(), trial$to_json())
  expect_identical(trial$copy()$to_list(), design)
})

test_that("arms, endpoints and comparisons stay consistent", {
  trial <- hcc_trial()
  trial$arms$add("Placebo", type = "placebo", treatments = list(list(name = "Placebo", role = "placebo")))
  trial$comparisons$all_pairs("Sorafenib")
  labels <- function() vapply(trial$comparisons$list(), function(c) c$label, character(1))
  expect_identical(labels(), c("Durvalumab vs Sorafenib", "Placebo vs Sorafenib"))
  trial$endpoints$add("Progression-free survival", effect_measure = "hazard ratio", hypothesis = "superiority")
  # New endpoints join existing comparisons.
  expect_true(all(vapply(trial$comparisons$list(), function(c) length(c$endpoints), 1L) == 2L))
  trial$comparisons$add("Sorafenib", "Durvalumab") # an existing pair reverses
  expect_identical(trial$comparisons$get("Sorafenib vs Durvalumab")$experimental$label, "Sorafenib")
  expect_identical(trial$comparisons$get(1)$label, "Sorafenib vs Durvalumab")
  trial$comparisons$edit("Sorafenib", "Durvalumab", remove_endpoints = "Overall survival")
  design <- trial$to_list()
  expect_identical(design$comparisons[[1]], list(experimentalArm = 1L, comparatorArm = 0L, endpoints = list(1L)))
  trial$arms$remove("Placebo")
  expect_identical(labels(), "Sorafenib vs Durvalumab")
  trial$endpoints$remove("progression-free survival")
  expect_identical(trial$comparisons$count(), 0L) # left without endpoints
  expect_identical(length(trial$arms), 2L)
  expect_identical(trial$arms$names(), c("Durvalumab", "Sorafenib"))
  expect_true(trial$arms$has("durvalumab"))
  expect_error(trial$arms$get("Nivolumab"), "have: Durvalumab, Sorafenib")
  expect_error(trial$arms$get(5), "there are 2")
  trial$arms$move("Sorafenib", 1)
  expect_identical(trial$arms$names(), c("Sorafenib", "Durvalumab"))
  arm <- trial$arms$get("Durvalumab")
  arm$treatments$add("tremelimumab", role = "study", chembl = "chembl2108658")
  expect_identical(arm$treatments$get("tremelimumab")$identity(), "ChEMBL CHEMBL2108658")
  arm$treatments$remove(1)
  expect_identical(arm$treatments$names(), "tremelimumab")
  frame <- trial$arms$to_data_frame()
  expect_identical(names(frame), c("Arm", "Type", "Treatments"))
  expect_identical(as.data.frame(trial$endpoints)$Endpoint, "Overall survival")
  expect_output(print(trial), "Sorafenib")
})

test_that("edits are validated atomically", {
  trial <- hcc_trial()
  expect_error(trial$edit(sex = "male", min_age = 80, max_age = 40), class = "trial_design_error")
  expect_null(trial$population$sex)
  expect_identical(trial$population$min_age, 18)
  expect_error(trial$arms$rename("Durvalumab", "sorafenib"), "already exists")
  trial$arms$rename("Durvalumab", "Durvalumab") # renaming to itself is fine
  expect_error(trial$arms$get("Durvalumab")$treatments$edit("durvalumab", chembl = "CHEMBL1", smiles = "C"),
    "at most one")
  expect_null(trial$arms$get("Durvalumab")$treatments$get(1)$chembl)
  expect_error(trial$edit(sample_size = 100), "Unknown")
  expect_error(trial$endpoints$add("overall survival"), "already exists")
  expect_error(trial$population$edit(min_age = -1), "non-negative")
  expect_error(trial$details$edit(start_date = "March 2024"), "YYYY")
  expect_error(trial$arms$get(1)$edit(type = "sideways"), "experimental, active_comparator")
  trial$analyses <- c("Efficacy", "biology")
  expect_identical(trial$analyses, c("efficacy", "biology"))
})

test_that("variants are independent and their keys are valid", {
  trial <- hcc_trial()
  variants <- trial$vary(sex = c("female", "male"), min_age = c(18, 65))
  expect_identical(length(variants), 4L)
  expect_identical(variants$labels()[[1]], "sex=female, min_age=18")
  expect_identical(variants$get("sex=male, min_age=65")$population$sex, "male")
  expect_identical(variants$get("sex=male, min_age=65")$name, "HCC phase 3 (sex=male, min_age=65)")
  expect_null(trial$population$sex)
  male <- trial$variant(sex = "male", max_age = 64)
  expect_identical(male$population$summary, "hepatocellular carcinoma \u00b7 male only \u00b7 ages 18\u201364")
  key <- rsynthbio:::.trial_variant_key("hcc-sweep", "sex=male, min_age=65")
  expect_identical(key, "hcc-sweep:sex-male-min_age-65")
  long <- rsynthbio:::.trial_variant_key("p", strrep("label ", 60))
  expect_true(nchar(long) <= 200L)
  for (k in c(key, long)) expect_match(k, rsynthbio:::.verdict_design_key_pattern)
  expect_error(trial$vary(), "at least one")
})

test_that("Trial Import drafts become editable designs", {
  trial <- trial_from_import(fixture("verdict-import-proposal.json"))
  expect_identical(trial$population$disease, "Breast cancer")
  expect_identical(trial$details$phases, "PHASE3")
  expect_false(trial$population$healthy_volunteers)
  expect_identical(trial$population$min_age, 18L)
  expect_identical(trial$details$arm_count, 2L)
  expect_identical(trial$arms$get("Ribociclib + letrozole")$treatments$get("ribociclib")$chembl, "CHEMBL3545110")
  expect_null(trial$arms$get("Ribociclib + letrozole")$treatments$get("letrozole")$chembl)
  expect_true(trial$arms$get("Placebo + letrozole")$treatments$get("Placebo")$name_only)
  expect_identical(trial$comparisons$get(1)$label, "Ribociclib + letrozole vs Placebo + letrozole")
  design <- trial$to_list()
  expect_identical(design$endpoints[[1]]$title, "Progression-free survival")
  expect_identical(design$endpoints[[1]]$role, "primary")
  expect_identical(design$metadata$primaryEndpointCount, 1L)
  expect_null(design$safetyQueries) # the blank draft query is dropped
})

fake_client <- function(availability = list(), issues = function(design) list()) {
  calls <- new.env()
  calls$prepare <- list()
  calls$run <- list()
  calls$order <- character()
  client <- list(
    prepare_trial_design = function(design, idempotency_key = NULL) {
      calls$prepare[[length(calls$prepare) + 1L]] <- list(design = design, key = idempotency_key)
      requested <- unlist(design$requestedAnalyses)
      found <- issues(design)
      list(issues = found, entities = list(), assumptions = list(),
        availability = lapply(requested, function(a) {
          outcome <- if (is.null(availability[[a]])) "accepted" else availability[[a]]
          list(analysis = a, outcome = outcome, reasons = if (outcome == "unavailable") list("why") else list())
        }),
        reviewFingerprint = if (length(found)) NULL else strrep("f", 64))
    },
    run_trial_design = function(design, review_fingerprint, idempotency_key) {
      calls$run[[length(calls$run) + 1L]] <- list(design = design, fingerprint = review_fingerprint, key = idempotency_key)
      calls$order <- c(calls$order, "submit")
      list(id = idempotency_key, status = "queued")
    },
    wait_for_trial = function(id, timeout = 3600, poll_interval = 5) {
      calls$order <- c(calls$order, "wait")
      list(id = id, headlines = function() list(status = "complete", run = id))
    }
  )
  list(client = client, calls = calls)
}

test_that("run skips unavailable analyses and reuses the key", {
  fake <- fake_client(list(biology = "unavailable"))
  result <- hcc_trial()$run(fake$client, idempotency_key = "hcc-1")
  expect_identical(result$id, "hcc-1")
  expect_identical(fake$calls$run[[1]]$design$requestedAnalyses, list("efficacy", "toxicity"))
  expect_identical(fake$calls$run[[1]]$key, "hcc-1")
  expect_identical(fake$calls$run[[1]]$fingerprint, strrep("f", 64))
  expect_true(all(vapply(fake$calls$prepare, function(c) identical(c$key, "hcc-1"), logical(1))))
  expect_error(hcc_trial()$run(fake$client), "idempotency_key")
  review <- hcc_trial()$review(fake$client)
  expect_true(review$ok) # Biology is skipped; the others can run
  expect_identical(review$unavailable, "biology")
  expect_output(print(review), "biology: unavailable")
  expect_identical(as.data.frame(review)$outcome, c("accepted", "accepted", "unavailable"))
})

test_that("run refuses when nothing can run", {
  fake <- fake_client(list(efficacy = "unavailable", toxicity = "unavailable", biology = "unavailable"))
  failure <- tryCatch(hcc_trial()$run(fake$client, idempotency_key = "hcc-2"), trial_review_error = function(e) e)
  expect_s3_class(failure, "trial_review_error")
  expect_s3_class(failure$review, "verdict_trial_review")
  expect_length(fake$calls$run, 0L)
})

test_that("run drops Toxicity when the release needs a safety query", {
  fake <- fake_client(issues = function(design) {
    if ("toxicity" %in% unlist(design$requestedAnalyses)) list(list(field = "query.0.term", message = "needs a query"))
    else list()
  })
  receipt <- hcc_trial()$run(fake$client, idempotency_key = "hcc-3", wait = FALSE)
  expect_identical(receipt$status, "queued")
  expect_identical(fake$calls$run[[1]]$design$requestedAnalyses, list("efficacy", "biology"))
})

test_that("trial sets submit every variant before waiting", {
  fake <- fake_client()
  variants <- hcc_trial()$vary(sex = c("female", "male"))
  readiness <- variants$review(fake$client)
  expect_identical(names(readiness), c("variant", "ready", "will_run", "skipped", "issues"))
  expect_true(all(readiness$ready))
  results <- variants$run(fake$client, idempotency_prefix = "by-sex")
  expect_identical(fake$calls$order, c("submit", "submit", "wait", "wait"))
  expect_identical(results$table$run, c("by-sex:sex-female", "by-sex:sex-male"))
  expect_identical(names(results$results), c("sex=female", "sex=male"))
  expect_error(variants$run(fake$client, idempotency_prefix = "has spaces"), "idempotency_prefix")
})

test_that("the client sends trial-design requests with valid keys", {
  calls <- list()
  reply <- structure(list(url = "https://verdict.synthesize.bio", status_code = 200L,
    headers = list(`content-type` = "application/json"), content = charToRaw('{"ok":true}')), class = "response")
  mockery::stub(verdict_client, "httr::VERB", function(verb, url, ...) {
    calls[[length(calls) + 1L]] <<- list(verb = verb, url = url, args = list(...))
    reply
  })
  client <- verdict_client("vdt_test")
  client$trial_catalog()
  client$trial_schema()
  client$prepare_trial_design(hcc_trial(), "k1")
  client$prepare_trial_design(hcc_trial())
  client$run_trial_design(hcc_trial()$to_list(), strrep("f", 64), idempotency_key = "k1")
  expect_identical(vapply(calls, `[[`, "", "verb"), c("GET", "GET", "POST", "POST", "POST"))
  expect_identical(vapply(calls, `[[`, "", "url"), paste0("https://verdict.synthesize.bio/api/v1/trial-designs/",
    c("catalog", "openapi.json", "prepare", "prepare", "run")))
  body <- jsonlite::fromJSON(calls[[3]]$args$body, simplifyVector = FALSE)
  expect_identical(body$idempotencyKey, "k1")
  expect_identical(body$design$name, "HCC phase 3")
  expect_identical(body$design$population$phases, list("PHASE3"))
  generated <- jsonlite::fromJSON(calls[[4]]$args$body, simplifyVector = FALSE)$idempotencyKey
  expect_match(generated, rsynthbio:::.verdict_design_key_pattern)
  run <- jsonlite::fromJSON(calls[[5]]$args$body, simplifyVector = FALSE)
  expect_identical(run$reviewFingerprint, strrep("f", 64))
  expect_error(client$run_trial_design(list(), strrep("f", 64), idempotency_key = "has spaces"), "letters")
  expect_error(client$run_trial_design(list(), strrep("f", 64)), "explicit idempotency_key")
  expect_error(client$prepare_trial_design(list(), strrep("a", 201)), "200 characters")
})

test_that("results read as tables, not JSON", {
  path <- tempfile(fileext = ".json")
  on.exit(unlink(path))
  writeLines(jsonlite::toJSON(fixture("verdict-trial-report.json"), auto_unbox = TRUE, null = "null", digits = NA), path)
  result <- load_trial_result(path)
  efficacy <- result$efficacy$table
  # The trial's first comparison leads, primary endpoint first, like the web report.
  expect_identical(efficacy$hazard_ratio[[1]], 0.75)
  expect_identical(efficacy$role[1:2], c("primary", "secondary"))
  expect_identical(names(efficacy), c("comparison", "endpoint", "role", "p_significant",
    "p_favors_experimental", "expected_abs_log_effect", "hazard_ratio", "status"))
  safety <- result$safety$table
  expect_identical(safety$event[[1]], "Any serious adverse event")
  peaks <- apply(as.matrix(safety[-1, -1]), 1L, max, na.rm = TRUE)
  expect_false(is.unsorted(rev(peaks)))
  expect_identical(result$biology$table$tissue[[1]], "Lung")
  expect_true(all(!grepl("^UBERON:", result$biology$table$tissue)))
  headlines <- result$headlines()
  expect_equal(headlines$p_success, 0.6043, tolerance = 1e-3)
  text <- paste(utils::capture.output(print(result)), collapse = "\n")
  expect_match(text, "P(Success) 60%", fixed = TRUE)
  expect_match(text, "Any serious AE", fixed = TRUE)
  expect_match(result$safety$html, "<table", fixed = TRUE)
  expect_identical(rsynthbio:::.verdict_percent(0.0123), "1.2%")
  expect_identical(rsynthbio:::.verdict_percent(NA), "\u2014")
  no_overall <- rsynthbio:::.verdict_safety_table(result$raw, include_overall = FALSE)
  expect_false("Any serious adverse event" %in% no_overall$event)
})

test_that("endpoint roles are optional, forgiving and only sent when set", {
  trial <- hcc_trial()
  endpoint <- trial$endpoints$get(1)
  expect_null(endpoint$role)
  expect_false("role" %in% names(trial$to_list()$endpoints[[1]]))
  expect_identical(as.data.frame(trial$endpoints)$Role, "primary")
  endpoint$edit(role = "Secondary")
  expect_identical(endpoint$role, "secondary")
  expect_identical(trial$to_list()$endpoints[[1]]$role, "secondary")
  expect_error(endpoint$edit(role = "tertiary"), "primary, secondary, unknown")
  expect_identical(endpoint$role, "secondary")
  copy <- trial_from_list(trial$to_list())
  expect_identical(copy$endpoints$get(1)$role, "secondary")
  expect_true("endpoints[].role" %in% trial_fields()$field)
})

test_that("review printouts count model notes instead of listing them", {
  review <- structure(list(ok = TRUE, issues = list(), availability = list(), entities = list(),
    assumptions = paste("note", 1:18)), class = "verdict_trial_review")
  out <- capture.output(print(review))
  expect_true(any(grepl("18 model notes: see review$assumptions", out, fixed = TRUE)))
  expect_false(any(grepl("note 1", out, fixed = TRUE)))
})
