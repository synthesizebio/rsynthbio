#' Load a trial input file without contacting the API
#' @param path Path to a VERDICT workflow input JSON file.
#' @return The trial input as a list. No inference is submitted.
#' @export
load_trial <- function(path) {
  value <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  if (!is.list(value) || !all(c("trial", "requested_analyses") %in% names(value))) stop("Expected a VERDICT trial workflow input JSON file")
  value
}

#' Load a saved trial result offline
#' @param path Path to a saved trial report JSON file.
#' @param api_base_url VERDICT origin used only to construct the UI link.
#' @return A verdict_trial_result with summary, biology, efficacy, safety, raw,
#'   run_id, ui_url, and save_json(path). Loading and displaying never run models.
#' @export
load_trial_result <- function(path, api_base_url = "https://verdict.synthesize.bio") {
  report <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  .verdict_trial_result(report, sub("/+$", "", api_base_url), "Saved API report \u00b7 offline")
}

#' Print a trial report or analysis view
#' @param x A trial result or one of its summary/analysis views.
#' @param ... Plot selection arguments for analysis views.
#' @return The input, invisibly. Printing an analysis view draws its saved charts.
#' @export
print.verdict_trial_result <- function(x, ...) {
  cat("VERDICT trial", x$run_id, "\u00b7", x$raw$status, "\n", x$ui_url, "\n")
  invisible(x)
}

#' @rdname print.verdict_trial_result
#' @export
print.verdict_trial_view <- function(x, ...) {
  if ("IRkernel" %in% loadedNamespaces() && requireNamespace("IRdisplay", quietly = TRUE)) {
    IRdisplay::display_html(x$html)
  } else cat(x$text, "\n")
  if (!is.null(x$draw)) x$draw(...)
  invisible(x)
}

#' Plot saved trial analysis data without running inference
#' @param x A biology, efficacy, or safety view from a trial result.
#' @param ... Biology accepts contrast and tissue_id; safety accepts comparison_id.
#' @return The view, invisibly. All plotted values come from the saved API report.
#' @export
plot.verdict_trial_view <- function(x, ...) {
  if (is.null(x$draw)) stop("This view has no chart")
  x$draw(...)
  invisible(x)
}

.verdict_html_escape <- function(x) {
  x <- gsub("&", "&amp;", as.character(x), fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub('"', "&quot;", x, fixed = TRUE)
  gsub("'", "&#39;", x, fixed = TRUE)
}
.verdict_comparable_input <- function(value) {
  sort_keys <- function(x) {
    if (!is.list(x)) return(x)
    if (!is.null(names(x))) x <- x[order(names(x))]
    lapply(x, sort_keys)
  }
  for (i in seq_along(value$trial$endpoints)) {
    analysis <- value$trial$endpoints[[i]]$outcome_analysis
    if (!is.null(analysis) && !"endpoint_role" %in% names(analysis)) value$trial$endpoints[[i]]$outcome_analysis$endpoint_role <- "primary"
  }
  sort_keys(value)
}
.verdict_validate_report <- function(report, run_id = NULL, input = NULL) {
  if (!is.list(report) || !all(c("run_id", "request", "components", "analyses", "status") %in% names(report))) stop("Expected a VERDICT trial result, not an individual model result.")
  if (!is.null(run_id) && !identical(report$run_id, run_id)) stop("Saved report does not match the requested run.")
  if (!is.null(input) && !identical(.verdict_comparable_input(report$request), .verdict_comparable_input(input))) stop("Saved report does not match the requested trial input.")
  for (component in report$components) {
    if (component$status != "not_run" && component$mode != "live") stop("Report contains a non-live component; review provenance before recording.")
  }
}
.verdict_trial_result <- function(report, base, source, execution = NULL) {
  .verdict_validate_report(report)
  ui_url <- paste0(base, "/app/virtual-trials/runs/", report$run_id)
  rows <- vapply(report$analyses, function(a) {
    notes <- paste(vapply(a$issues, function(i) i$message, character(1)), collapse = "; ")
    paste0("<tr>", paste0("<td style='padding:8px;border-bottom:1px solid #ddd'>",
      .verdict_html_escape(c(gsub("_", " ", a$analysis), a$status, notes)), "</td>", collapse = ""), "</tr>")
  }, character(1))
  client_version <- if (requireNamespace("rsynthbio", quietly = TRUE)) {
    package <- utils::packageDescription("rsynthbio")
    list(package = "rsynthbio", version = package$Version, git_commit = package$RemoteSha)
  } else list(package = "rsynthbio", version = "not installed")
  provenance <- list(local_client = client_version, execution = execution,
    components = lapply(report$components, function(component) component[c("component", "mode", "status", "release", "issues")]))
  summary_html <- paste0("<h2>", .verdict_html_escape(report$request$trial$name), "</h2><p>", .verdict_html_escape(source),
    " \u00b7 Overall status: <strong>", .verdict_html_escape(report$status), "</strong></p><p>Run: ", .verdict_html_escape(report$run_id),
    "</p><p><a href='", .verdict_html_escape(ui_url), "'>Open this same report in VERDICT</a></p>",
    "<table><thead><tr><th>Analysis</th><th>Status</th><th>Notes</th></tr></thead><tbody>", paste(rows, collapse = ""),
    "</tbody></table><details><summary>Model and client provenance</summary><pre>",
    .verdict_html_escape(jsonlite::toJSON(provenance, auto_unbox = TRUE, pretty = TRUE, null = "null")), "</pre></details>")
  section <- function(kind, title, plot_name) {
    matches <- Filter(function(a) identical(a$analysis, kind), report$analyses)
    analysis <- if (length(matches)) matches[[1]] else NULL
    components <- Filter(function(c) c$component %in% unlist(analysis$components), report$components)
    notes <- paste(vapply(analysis$issues, function(i) i$message, character(1)), collapse = "; ")
    status <- if (is.null(analysis)) "not requested" else analysis$status
    text <- paste(title, status, notes, sep = " \u00b7 ")
    html <- paste0("<h2>", title, "</h2><p>", .verdict_html_escape(text), "</p>")
    structure(list(data = list(analysis = analysis, components = components),
      text = text, html = html,
      draw = function(...) {
        if (is.null(analysis)) { cat(text, "\n"); return(invisible(NULL)) }
        get(plot_name, envir = environment(.verdict_trial_result))(report, ...)
      }), class = "verdict_trial_view")
  }
  structure(list(raw = report, run_id = report$run_id, ui_url = ui_url,
    provenance = provenance,
    save_json = function(path) writeLines(jsonlite::toJSON(report, auto_unbox = TRUE, pretty = TRUE, null = "null", digits = NA), path),
    summary = structure(list(data = report$analyses, html = summary_html,
      text = paste(c(report$request$trial$name, source, paste("Overall status:", report$status), ui_url,
        vapply(report$analyses, function(a) paste(a$analysis, a$status, sep = ": "), character(1))), collapse = "\n")), class = "verdict_trial_view"),
    biology = section("biology", "Biology", ".verdict_plot_biology"),
    efficacy = section("efficacy", "Efficacy", ".verdict_plot_efficacy"),
    safety = section("toxicity", "Safety", ".verdict_plot_safety")), class = "verdict_trial_result")
}
