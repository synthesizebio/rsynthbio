#' @importFrom graphics abline axis legend par plot points rect segments text
#' @importFrom grDevices adjustcolor
#' @importFrom utils head
NULL

# Static report charts; all plotted values come from the saved API response.
.verdict_purple <- "#6A55A0"
.verdict_teal <- "#25857D"
.verdict_training <- function(status) {
  labels <- c(in_training = "Used to train this model", selection_only = "Used for model selection",
    not_in_training = "Not used for training or model selection", unknown = "Training membership unavailable")
  if (is.null(status) || !status %in% names(labels)) status <- "unknown"
  unname(labels[[status]])
}
.verdict_number <- function(x) if (is.null(x)) NA_real_ else x
.verdict_component <- function(report, name) {
  matches <- Filter(function(x) identical(x$component, name), report$components)
  if (!length(matches)) return(list(outputs = list(), mode = "unavailable", status = "unavailable"))
  component <- matches[[1]]
  cat(name, component$mode, component$status, "\n")
  component
}
.verdict_pair <- function(trial, id = NULL) {
  if (is.null(id)) return(trial$comparisons[[1]])
  Filter(function(x) identical(x$comparison_id, id), trial$comparisons)[[1]]
}
.verdict_arm <- function(trial, id) Filter(function(x) identical(x$arm_id, id), trial$arms)[[1]]$label
.verdict_comparison <- function(trial, id) {
  pair <- .verdict_pair(trial, id)
  paste(.verdict_arm(trial, pair$experimental_arm_id), "vs", .verdict_arm(trial, pair$comparator_arm_id))
}
.verdict_wrap <- function(x, width = 40) {
  # Allow long ontology/feature identifiers to wrap at underscores.
  gsub("_ ", "_", paste(strwrap(gsub("_", "_ ", x, fixed = TRUE), width), collapse = "\n"), fixed = TRUE)
}
.verdict_frame <- function(labels, xlim, title, xlabel, percent = FALSE) {
  plot(NA, xlim = xlim, ylim = c(length(labels) + 0.6, 0.4), xlab = xlabel, ylab = "", axes = FALSE, main = title)
  ticks <- pretty(xlim)
  ticks <- ticks[ticks >= xlim[1] - 1e-12 & ticks <= xlim[2] + 1e-12]
  abline(v = ticks, col = "#EEEEEE")
  axis(1, at = ticks, labels = if (percent) sprintf("%.1f%%", ticks * 100) else ticks)
  axis(2, at = seq_along(labels), labels = labels, las = 1, tick = FALSE, cex.axis = 0.8)
}

.verdict_plot_efficacy <- function(report) {
  component <- .verdict_component(report, "outcomes")
  rows <- Filter(function(o) identical(o$kind, "analysis_outcome"), component$outputs)
  if (!length(rows)) { cat("No endpoint predictions supplied.\n"); return(invisible(NULL)) }
  old <- par(no.readonly = TRUE); on.exit(par(old))
  par(mar = c(5, 13, 5, 2), mfrow = c(length(rows), 1))
  for (row in rows) {
    p <- row$prediction
    endpoint <- Filter(function(e) identical(e$endpoint_id, row$endpoint_id), report$request$trial$endpoints)[[1]]
    title <- paste(.verdict_wrap(endpoint$name, 65), .verdict_comparison(report$request$trial, row$comparison_id),
      paste("Efficacy", component$mode, component$status, sep = " \u00b7 "), sep = "\n")
    magnitude <- if (identical(p$magnitude_status, "predicted") && !is.null(p$expected_absolute_log_effect)) {
      sprintf("%.3f", p$expected_absolute_log_effect)
    } else if (identical(p$magnitude_status, "not_applicable")) "Not applicable" else "Unsupported input"
    .verdict_frame(c("Probability significant", "Probability experimental arm favored,\ngiven significant superiority"), c(0, 1), title,
      paste("Expected absolute log-ratio magnitude:", magnitude, "\n", .verdict_training(p$training_status)), TRUE)
    fields <- c("probability_significant", "probability_first_arm_favored")
    statuses <- c("status", "direction_status")
    for (i in 1:2) {
      value <- .verdict_number(p[[fields[i]]])
      if (!is.na(value) && identical(p[[statuses[i]]], "predicted")) {
        rect(0, i - 0.18, value, i + 0.18, col = c(.verdict_purple, .verdict_teal)[i], border = NA)
        text(min(value + 0.02, 0.99), i, sprintf("%.1f%%", value * 100), adj = if (value > 0.9) 1 else 0, cex = 0.85)
      } else text(0.02, i, if (identical(p[[statuses[i]]], "not_applicable")) "Not applicable" else "Unsupported input", adj = 0)
    }
  }
  cat("Magnitude is unsigned |ln(ratio)|, not a hazard ratio or uncertainty interval. Significance is not trial-success probability.\n")
}

.verdict_plot_safety <- function(report, comparison_id = NULL) {
  component <- .verdict_component(report, "safety")
  if (!length(component$outputs) || !length(report$request$trial$arms)) {
    cat("No safety arm results supplied.\n"); return(invisible(NULL))
  }
  old <- par(no.readonly = TRUE); on.exit(par(old))
  par(mar = c(5, 14, 4, 2), mfrow = c(1, 1))
  if (length(report$request$trial$comparisons) || !is.null(comparison_id)) {
    pair <- .verdict_pair(report$request$trial, comparison_id)
    arms <- c(pair$experimental_arm_id, pair$comparator_arm_id)
  } else {
    arms <- vapply(head(report$request$trial$arms, 2), function(a) a$arm_id, character(1))
  }
  labels <- vapply(arms, function(a) .verdict_arm(report$request$trial, a), character(1))
  get_row <- function(q, arm) {
    matches <- Filter(function(o) identical(o$kind, "safety_incidence") && identical(o$query_id, q$query_id) && identical(o$arm_id, arm), component$outputs)
    if (length(matches)) matches[[1]] else list()
  }
  get_value <- function(q, arm) {
    row <- get_row(q, arm)
    if (identical(row$status, "predicted")) .verdict_number(row$incidence) else NA_real_
  }
  queries <- report$request$safety_queries
  for (seriousness in unique(vapply(queries, function(q) q$seriousness, character(1)))) {
    group <- Filter(function(q) identical(q$seriousness, seriousness), queries)
    scores <- vapply(group, get_value, numeric(1), arm = arms[1])
    group <- group[order(-scores, na.last = TRUE)]
    maximum <- 0
    for (q in group) for (arm in arms) {
      v <- get_value(q, arm); interval <- get_row(q, arm)$interval
      if (!is.na(v)) maximum <- max(maximum, v, if (is.null(interval)) v else interval$upper)
    }
    limits <- c(.02, .05, .1, .15, .2, .3, .4, .5, .6, .8, 1)
    maximum <- if (any(limits >= maximum * 1.04)) min(limits[limits >= maximum * 1.04]) else 1
    .verdict_frame(vapply(group, function(q) .verdict_wrap(q$term, 32), character(1)), c(0, maximum),
      paste(if (seriousness == "serious") "Serious adverse events" else "Other adverse events", component$mode, component$status, sep = " \u00b7 "),
      "Predicted share of patients with the event", TRUE)
    for (i in seq_along(group)) {
      q <- group[[i]]; values <- vapply(arms, function(a) get_value(q, a), numeric(1))
      if (length(values) == 2 && all(!is.na(values))) segments(values[1], i, values[2], i, col = "#C7C1CD", lwd = 3)
      for (j in seq_along(arms)) {
        if (is.na(values[j])) { text(0, i + (j - 1.5) * .25, paste(labels[j], "unavailable"), adj = 0, cex = .6); next }
        interval <- get_row(q, arms[j])$interval
        color <- c(.verdict_purple, .verdict_teal)[j]
        if (!is.null(interval)) segments(interval$lower, i, interval$upper, i, col = adjustcolor(color, .3), lwd = 6)
        points(values[j], i, pch = 19, col = color, cex = 1.3)
      }
    }
    legend("bottomright", labels, pch = 19, col = c(.verdict_purple, .verdict_teal), bty = "n", cex = .8)
  }
  cat("Lines connect arms; bands appear only when intervals are supplied.\n")
  cat(unique(vapply(component$outputs, function(o) .verdict_training(o$training_status), character(1))), sep = "\n")
}

.verdict_plot_biology <- function(report, contrast = NULL, tissue_id = NULL) {
  component <- .verdict_component(report, "gem2")
  aggregates <- Filter(function(o) identical(o$kind, "expression_effect") && identical(o$feature_kind, "aggregate") && identical(o$metric, "mean_absolute_log2_cpm_change"), component$outputs)
  if (!length(aggregates)) { cat("No tissue summaries supplied.\n"); return(invisible(NULL)) }
  if (is.null(contrast)) contrast <- aggregates[[1]]$contrast
  same_contrast <- function(x) identical(x$kind, contrast$kind) && identical(x$comparison_id, contrast$comparison_id) && identical(x$arm_id, contrast$arm_id)
  rows <- Filter(function(o) same_contrast(o$contrast), aggregates)
  if (!length(rows)) stop("No aggregates for this contrast")
  has_median <- any(vapply(rows, function(o) !is.null(o$median_donor_shift), logical(1)))
  statistic <- if (has_median) "median_donor_shift" else "value"
  rows <- rows[order(-vapply(rows, function(o) .verdict_number(o[[statistic]]), numeric(1)), vapply(rows, function(o) o$tissue_id, character(1)), na.last = TRUE)]
  tissues <- c("0000014" = "Skin", "0002046" = "Thyroid", "0002048" = "Lung", "0002084" = "Heart \u00b7 left ventricle",
    "0000310" = "Mammary gland", "0002107" = "Liver", "0000945" = "Stomach", "0001264" = "Pancreas", "0002113" = "Kidney", "0000178" = "Blood", "0001911" = "Mammary gland", "0002097" = "Skin")
  tissue_name <- function(id) { key <- sub("UBERON:", "", id); if (key %in% names(tissues)) unname(tissues[[key]]) else id }
  label <- if (contrast$kind == "trial_comparison") .verdict_comparison(report$request$trial, contrast$comparison_id) else paste(.verdict_arm(report$request$trial, contrast$arm_id), "vs untreated")
  old <- par(no.readonly = TRUE); on.exit(par(old))
  par(mar = c(5, 12, 4, 2), mfrow = c(1, 1))
  maximum <- max(.001, unlist(lapply(rows, function(o) c(o$value, o[[statistic]], o$interval$upper))), na.rm = TRUE) * 1.15
  .verdict_frame(vapply(rows, function(o) paste0(tissue_name(o$tissue_id), " (n=", o$reference_count, ")"), character(1)), c(0, maximum),
    paste(paste("Biology", component$mode, component$status, sep = " \u00b7 "), label, sep = "\n"), "Typical change per gene, up or down \u00b7 log2(CPM+1)")
  for (i in seq_along(rows)) {
    row <- rows[[i]]
    if (!is.null(row$interval)) segments(row$interval$lower, i, row$interval$upper, i, col = adjustcolor(.verdict_teal, .4), lwd = 3)
    if (!is.null(row$value)) points(row$value, i, pch = 21, bg = "white", col = .verdict_teal, cex = 1.3)
    if (!is.null(row[[statistic]])) points(row[[statistic]], i, pch = 19, col = .verdict_purple, cex = 1.1) else text(0, i, "Median not supplied", adj = 0, cex = .7)
  }
  legend("bottomright", c(if (has_median) "Median donor" else "Mean reference", "Mean reference (bands: supplied mean intervals)"), pch = c(19, 21), col = c(.verdict_purple, .verdict_teal), pt.bg = "white", bty = "n", cex = .7)
  if (is.null(tissue_id)) tissue_id <- rows[[1]]$tissue_id
  if (!tissue_id %in% vapply(rows, function(o) o$tissue_id, character(1))) stop("No aggregate for this tissue")
  for (kind in c("gene", "pathway")) {
    features <- Filter(function(o) same_contrast(o$contrast) && identical(o$tissue_id, tissue_id) && identical(o$feature_kind, kind) && identical(o$metric, "mean_signed_log2_cpm_change") && !is.null(o$value), component$outputs)
    features <- head(features[order(-vapply(features, function(o) abs(o$value), numeric(1)))], if (kind == "gene") 3 else 4)
    if (!length(features)) { cat("No", kind, "summaries supplied.\n"); next }
    values <- vapply(features, function(o) o$value, numeric(1)); maximum <- max(.001, abs(values)) * 1.2
    par(mar = c(5, 16, 4, 2))
    .verdict_frame(vapply(features, function(o) .verdict_wrap(o$feature_id, 35), character(1)), c(-maximum, maximum),
      paste(tissue_name(tissue_id), paste("largest", kind, "changes"), component$mode, component$status, sep = " \u00b7 "), "Down \u2190 signed mean change \u00b7 log2(CPM+1) \u2192 Up")
    abline(v = 0, col = "#777777")
    for (i in seq_along(values)) rect(min(0, values[i]), i - .2, max(0, values[i]), i + .2, border = NA, col = if (values[i] >= 0) .verdict_teal else .verdict_purple)
  }
  cat("Tissue changes measure expression, not efficacy or toxicity. Intervals describe means, not medians.\n",
    "Pathways: equal-symbol signed mean; not enrichment or causal pathway activity.\n")
  if (identical(report$request$trial$registry_id, "NCT02993523")) cat("VIALE-A uses normal reference tissues without bone marrow; this does not model AML marrow response.\n")
}
