# Readable tables from a saved VERDICT trial report (no network, no plotting).
#
# The numbers match the VERDICT results page and PDF: probabilities are model
# estimates, and unavailable values stay missing (NA) rather than being filled in.

.verdict_tissue_cache <- new.env(parent = emptyenv())

.verdict_tissue_names <- function() {
  if (is.null(.verdict_tissue_cache$names)) {
    path <- system.file("extdata", "verdict-tissue-names.csv", package = "rsynthbio")
    names <- character()
    if (nzchar(path)) {
      table <- utils::read.csv(path, stringsAsFactors = FALSE, encoding = "UTF-8")
      names <- stats::setNames(table$name, table$tissue_id)
    }
    .verdict_tissue_cache$names <- names
  }
  .verdict_tissue_cache$names
}

.verdict_tissue_name <- function(id) {
  names <- .verdict_tissue_names()
  if (id %in% names(names)) unname(names[[id]]) else id
}

.verdict_requested <- function(report, kind) kind %in% unlist(report$request$requested_analyses)

.verdict_outputs <- function(report, component) {
  for (item in report$components) {
    if (identical(item$component, component)) return(if (is.null(item$outputs)) list() else item$outputs)
  }
  list()
}

.verdict_arm_labels <- function(report) {
  arms <- report$request$trial$arms
  ids <- vapply(arms, function(a) a$arm_id, character(1))
  stats::setNames(vapply(arms, function(a) if (is.null(a$label)) a$arm_id else a$label, character(1)), ids)
}

.verdict_comparison_label <- function(report, comparison) {
  arms <- .verdict_arm_labels(report)
  label <- function(id) if (id %in% names(arms)) arms[[id]] else id
  paste(label(comparison$experimental_arm_id), "vs", label(comparison$comparator_arm_id))
}

.verdict_endpoint_role <- function(endpoint) {
  analysis <- endpoint$outcome_analysis
  if (is.null(analysis)) return("not stated")
  if (identical(analysis$endpoint_role, "secondary")) "secondary" else "primary"
}

.verdict_num <- function(x) if (is.null(x)) NA_real_ else as.numeric(x)

.verdict_efficacy_columns <- c("comparison", "endpoint", "role", "p_significant", "p_favors_experimental",
  "expected_abs_log_effect", "hazard_ratio", "status")

# One row per comparison and endpoint: probability of significance, direction and
# magnitude, or the predicted hazard ratio for survival endpoints.
.verdict_efficacy_table <- function(report) {
  empty <- data.frame(comparison = character(), endpoint = character(), role = character(),
    p_significant = numeric(), p_favors_experimental = numeric(), expected_abs_log_effect = numeric(),
    hazard_ratio = numeric(), status = character(), stringsAsFactors = FALSE)
  if (!.verdict_requested(report, "efficacy")) return(empty)
  trial <- report$request$trial
  endpoints <- trial$endpoints
  names(endpoints) <- vapply(endpoints, function(e) e$endpoint_id, character(1))
  outputs <- .verdict_outputs(report, "outcomes")
  rows <- list()
  for (comparison in trial$comparisons) {
    label <- .verdict_comparison_label(report, comparison)
    for (endpoint_id in unlist(comparison$endpoint_ids)) {
      endpoint <- endpoints[[endpoint_id]]
      if (is.null(endpoint)) next
      match <- Filter(function(o) identical(o$comparison_id, comparison$comparison_id) &&
        identical(o$endpoint_id, endpoint_id), outputs)
      outcome <- Filter(function(o) identical(o$kind, "analysis_outcome"), match)
      survival <- Filter(function(o) identical(o$kind, "survival_effect"), match)
      prediction <- if (length(outcome)) outcome[[1]]$prediction else list()
      status <- prediction$status
      if (is.null(status)) status <- if (length(survival)) "predicted" else "no prediction"
      rows[[length(rows) + 1L]] <- data.frame(
        comparison = label,
        endpoint = if (is.null(endpoint$name)) NA_character_ else endpoint$name,
        role = .verdict_endpoint_role(endpoint),
        p_significant = .verdict_num(prediction$probability_significant),
        p_favors_experimental = .verdict_num(prediction$probability_first_arm_favored),
        expected_abs_log_effect = .verdict_num(prediction$expected_absolute_log_effect),
        hazard_ratio = if (length(survival)) .verdict_num(survival[[1]]$hazard_ratio) else NA_real_,
        status = status, stringsAsFactors = FALSE)
    }
  }
  if (!length(rows)) return(empty)
  table <- do.call(rbind, rows)
  # Comparisons keep the trial's order; within each, primary endpoints come first.
  comparison_order <- match(table$comparison, unique(table$comparison))
  role_order <- match(table$role, c("primary", "secondary", "not stated"))
  table <- table[order(comparison_order, role_order, method = "radix"), , drop = FALSE]
  rownames(table) <- NULL
  table
}

.verdict_overall_query <- function(report) {
  for (query in report$request$safety_queries) {
    if (identical(query$seriousness, "serious") && tolower(trimws(query$term)) == "any serious adverse event") {
      return(query$query_id)
    }
  }
  NULL
}

# Predicted share of patients with each serious adverse event, by arm. Rows are
# events (overall first, then highest rate); columns are arms. NA means the model
# made no prediction for that event and arm.
.verdict_safety_table <- function(report, include_overall = TRUE) {
  empty <- data.frame(event = character(), stringsAsFactors = FALSE)
  if (!.verdict_requested(report, "toxicity")) return(empty)
  arms <- .verdict_arm_labels(report)
  incidence <- Filter(function(o) identical(o$kind, "safety_incidence") && identical(o$status, "predicted"),
    .verdict_outputs(report, "safety"))
  predicted_arms <- unique(vapply(incidence, function(o) o$arm_id, character(1)))
  arm_ids <- names(arms)[names(arms) %in% predicted_arms]
  if (!length(arm_ids)) return(empty)
  rate <- list()
  for (o in incidence) rate[[paste(o$arm_id, o$query_id, sep = "\r")]] <- .verdict_num(o$incidence)
  overall <- .verdict_overall_query(report)
  events <- character()
  values <- list()
  for (query in report$request$safety_queries) {
    is_overall <- identical(query$query_id, overall)
    if (is_overall && !include_overall) next
    row <- vapply(arm_ids, function(a) {
      value <- rate[[paste(a, query$query_id, sep = "\r")]]
      if (is.null(value)) NA_real_ else value
    }, numeric(1))
    if (all(is.na(row))) next
    events <- c(events, if (is_overall) "Any serious adverse event" else
      paste0(toupper(substr(query$term, 1L, 1L)), substring(query$term, 2L)))
    values[[length(values) + 1L]] <- row
  }
  if (!length(events)) return(empty)
  matrix <- do.call(rbind, values)
  first <- events == "Any serious adverse event"
  peak <- apply(matrix, 1L, function(r) if (all(is.na(r))) -Inf else max(r, na.rm = TRUE))
  ordering <- order(!first, -peak, method = "radix")
  table <- data.frame(event = events[ordering], stringsAsFactors = FALSE)
  for (i in seq_along(arm_ids)) table[[unname(arms[[arm_ids[[i]]]])]] <- matrix[ordering, i]
  rownames(table) <- NULL
  table
}

# Mean absolute log2 CPM change between arms, by tissue (rows) and comparison.
.verdict_biology_table <- function(report) {
  empty <- data.frame(tissue = character(), stringsAsFactors = FALSE)
  if (!.verdict_requested(report, "biology")) return(empty)
  comparisons <- list()
  for (c in report$request$trial$comparisons) comparisons[[c$comparison_id]] <- .verdict_comparison_label(report, c)
  tissues <- character()
  columns <- character()
  values <- list()
  for (o in .verdict_outputs(report, "gem2")) {
    contrast <- o$contrast
    if (identical(o$kind, "expression_effect") && identical(o$feature_kind, "aggregate") &&
        identical(o$metric, "mean_absolute_log2_cpm_change") && identical(contrast$kind, "trial_comparison")) {
      label <- comparisons[[contrast$comparison_id]]
      if (is.null(label)) label <- contrast$comparison_id
      tissue <- .verdict_tissue_name(o$tissue_id)
      if (!tissue %in% tissues) tissues <- c(tissues, tissue)
      if (!label %in% columns) columns <- c(columns, label)
      values[[paste(tissue, label, sep = "\r")]] <- .verdict_num(o$value)
    }
  }
  if (!length(tissues)) return(empty)
  table <- data.frame(tissue = tissues, stringsAsFactors = FALSE)
  for (label in columns) {
    table[[label]] <- vapply(tissues, function(t) {
      value <- values[[paste(t, label, sep = "\r")]]
      if (is.null(value)) NA_real_ else value
    }, numeric(1), USE.NAMES = FALSE)
  }
  table <- table[order(-table[[2L]], method = "radix"), , drop = FALSE]
  rownames(table) <- NULL
  table
}

# The few numbers to read first: overall status, P(Success) against historical odds,
# the main comparison's primary result and the overall serious-event rate by arm.
.verdict_headlines <- function(report) {
  out <- list(status = report$status)
  for (analysis in report$analyses) {
    if (identical(analysis$analysis, "trial_characteristics")) {
      estimates <- Filter(function(e) identical(e$kind, "phase_passage"), analysis$estimates)
      if (length(estimates)) {
        out$p_success <- .verdict_num(estimates[[1]]$probability)
        out$historical_odds <- .verdict_num(estimates[[1]]$reference_probability)
      }
    }
  }
  efficacy <- .verdict_efficacy_table(report)
  if (nrow(efficacy)) {
    out$main_comparison <- efficacy$comparison[[1]]
    out$main_endpoint <- efficacy$endpoint[[1]]
    out$p_significant <- efficacy$p_significant[[1]]
    out$p_favors_experimental <- efficacy$p_favors_experimental[[1]]
    out$hazard_ratio <- efficacy$hazard_ratio[[1]]
  }
  safety <- .verdict_safety_table(report)
  overall <- which(safety$event == "Any serious adverse event")
  if (length(overall)) {
    for (arm in setdiff(names(safety), "event")) out[[paste0("serious_ae: ", arm)]] <- safety[[arm]][[overall]]
  }
  out
}

.verdict_percent <- function(value) {
  if (is.null(value) || is.na(value)) return("\u2014")
  pct <- value * 100
  if (pct > 0 && pct < 10) sprintf("%.1f%%", pct) else sprintf("%.0f%%", pct)
}

.verdict_headline_lines <- function(h) {
  lines <- character()
  if (!is.null(h$p_success) && !is.na(h$p_success)) {
    lines <- c(lines, paste0("P(Success) ", .verdict_percent(h$p_success), " (historical odds ",
      .verdict_percent(h$historical_odds), ")"))
  }
  if (!is.null(h$main_endpoint)) {
    effect <- if (!is.na(h$hazard_ratio)) sprintf("HR %.2f", h$hazard_ratio) else
      paste("P(significant)", .verdict_percent(h$p_significant))
    lines <- c(lines, paste0("Efficacy \u00b7 ", h$main_comparison, " \u00b7 ", h$main_endpoint, ": ", effect))
  }
  for (key in grep("^serious_ae: ", names(h), value = TRUE)) {
    lines <- c(lines, paste0("Any serious AE \u00b7 ", sub("^serious_ae: ", "", key), ": ", .verdict_percent(h[[key]])))
  }
  lines
}

# Percent-formatted copy of a table for display.
.verdict_display_table <- function(table, kind) {
  columns <- if (kind == "toxicity") setdiff(names(table), "event") else grep("^p_", names(table), value = TRUE)
  for (column in columns) table[[column]] <- vapply(table[[column]], .verdict_percent, character(1))
  table
}

.verdict_table_html <- function(table, kind) {
  if (is.null(table) || !nrow(table)) return("<p>No predictions.</p>")
  table <- .verdict_display_table(table, kind)
  cell <- "padding:4px 10px;border-bottom:1px solid #e5e5e5;text-align:left"
  head <- paste0("<th style='", cell, "'>", .verdict_html_escape(names(table)), "</th>", collapse = "")
  body <- vapply(seq_len(nrow(table)), function(i) {
    values <- vapply(table[i, , drop = TRUE], function(v) {
      if (is.na(v)) "\u2014" else if (is.numeric(v)) format(signif(v, 4)) else as.character(v)
    }, character(1))
    paste0("<tr>", paste0("<td style='", cell, "'>", .verdict_html_escape(values), "</td>", collapse = ""), "</tr>")
  }, character(1))
  paste0("<table style='border-collapse:collapse'><thead><tr>", head, "</tr></thead><tbody>",
    paste(body, collapse = ""), "</tbody></table>")
}
