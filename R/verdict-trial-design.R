# Build, inspect and edit VERDICT trial designs (verdict.clinical-design/v1).
#
# Nothing here contacts the API until Trial$review() or Trial$run(); the server
# remains the authority on name resolution, analysis availability and validation.

.trial_schema_version <- "verdict.clinical-design/v1"

# The pinned contract enums (VERDICT webapp app/lib/trial-catalog.ts). The server
# validates authoritatively; these allow friendly offline checks and discovery.
.trial_vocabulary <- list(
  phase = c("EARLY_PHASE1", "PHASE1", "PHASE2", "PHASE3", "PHASE4", "NA"),
  sex = c("all", "female", "male"),
  arm_type = c("experimental", "active_comparator", "placebo_comparator", "no_intervention"),
  treatment_kind = c("drug", "biologic", "procedure", "radiation", "surgery", "device",
    "transplant", "behavioral", "dietary_supplement", "other"),
  treatment_role = c("study", "background", "placebo", "supportive", "premedication",
    "control", "other"),
  timing = c("concurrent", "lead_in", "sequential", "before", "after", "maintenance",
    "unspecified", "other"),
  endpoint_category = c("overall_survival", "progression_free_survival", "other"),
  endpoint_role = c("primary", "secondary", "unknown"),
  effect_measure = c("mean_difference", "hazard_ratio", "geometric_mean_ratio",
    "percentage_difference", "risk_difference", "odds_ratio", "risk_ratio",
    "median_difference", "ratio", "difference", "other"),
  hypothesis = c("superiority", "noninferiority", "equivalence", "unspecified"),
  allocation = c("RANDOMIZED", "NON_RANDOMIZED", "NA"),
  blinding = c("NONE", "SINGLE", "DOUBLE", "TRIPLE", "QUADRUPLE"),
  purpose = c("TREATMENT", "PREVENTION", "SUPPORTIVE_CARE", "DIAGNOSTIC", "SCREENING",
    "HEALTH_SERVICES_RESEARCH", "BASIC_SCIENCE", "DEVICE_FEASIBILITY", "OTHER"),
  sponsor_type = c("INDUSTRY", "NIH", "FED", "OTHER_GOV", "NETWORK", "INDIV", "UNKNOWN", "OTHER"),
  seriousness = c("serious", "other"),
  analysis = c("trial_characteristics", "efficacy", "toxicity", "biology")
)

# Requested by default: every analysis a custom design can feed. Historical Odds
# (trial_characteristics) needs an original registry submission, so it is opt-in.
.trial_default_analyses <- c("efficacy", "toxicity", "biology")

.trial_aliases <- list(
  phase = c(`1` = "PHASE1", `2` = "PHASE2", `3` = "PHASE3", `4` = "PHASE4",
    EARLY_1 = "EARLY_PHASE1", `0` = "EARLY_PHASE1"),
  blinding = c(OPEN = "NONE", OPEN_LABEL = "NONE"),
  arm_type = c(placebo = "placebo_comparator", comparator = "active_comparator",
    active = "active_comparator", none = "no_intervention", no_treatment = "no_intervention")
)

# Field path, type, allowed values (vocabulary key or NA) and meaning.
.trial_fields <- matrix(c(
  "details.name", "text", NA, "Trial name (required)",
  "details.phases", "list", "phase", "Trial phase(s); 'phase 1/2' is accepted",
  "details.start_date", "YYYY[-MM[-DD]]", NA, "Planned or actual start; limits evidence",
  "details.allocation", "choice", "allocation", "Randomization",
  "details.blinding", "choice", "blinding", "Masking",
  "details.purpose", "choice", "purpose", "Primary purpose",
  "details.sponsor_type", "choice", "sponsor_type", "Lead sponsor class",
  "details.arm_count", "int", NA, "Registered arms, if more than modeled",
  "details.primary_endpoint_count", "int", NA, "Primary endpoints in the trial",
  "population.disease", "text", NA, "Condition name; MONDO resolves on review",
  "population.mondo_id", "MONDO:#######", NA, "Optional explicit disease identity",
  "population.sex", "choice", "sex", "Sex eligibility",
  "population.min_age", "years", NA, "Minimum age (NULL = not stated)",
  "population.max_age", "years", NA, "Maximum age (NULL = not stated)",
  "population.healthy_volunteers", "logical", NA, "Accepts healthy volunteers",
  "population.notes", "text", NA, "Eligibility as written",
  "arms[].label", "text", NA, "Arm name (unique)",
  "arms[].type", "choice", "arm_type", "Arm type",
  "arms[].treatments[].name", "text", NA, "Treatment name; resolves to ChEMBL on review",
  "arms[].treatments[].role", "choice", "treatment_role", "Role in the regimen",
  "arms[].treatments[].timing", "choice", "timing", "When it is given",
  "arms[].treatments[].kind", "choice", "treatment_kind", "Treatment type",
  "arms[].treatments[].chembl", "CHEMBL#", NA, "Explicit ChEMBL identity",
  "arms[].treatments[].smiles", "SMILES", NA, "Explicit structure",
  "arms[].treatments[].name_only", "logical", NA, "No molecular identity (e.g. radiation)",
  "arms[].treatments[].dose", "text", NA, "Dose as stated (reference only)",
  "endpoints[].title", "text", NA, "Registered endpoint title",
  "endpoints[].role", "choice", "endpoint_role", "Primary (default) or secondary",
  "endpoints[].family", "catalog key", NA, "Release family; see client$trial_catalog()",
  "endpoints[].category", "choice", "endpoint_category", "Scientific category",
  "endpoints[].effect_measure", "choice", "effect_measure", "Planned effect measure",
  "endpoints[].hypothesis", "choice", "hypothesis", "Planned hypothesis",
  "endpoints[].time_frame", "text", NA, "Time frame notes",
  "comparisons[]", "arm vs arm", NA, "Experimental arm vs comparator, on endpoints",
  "safety_queries[]", "term + seriousness", "seriousness", "Only for non-catalog releases",
  "analyses", "list", "analysis", "Requested analyses (default efficacy, toxicity, biology)"
), ncol = 4, byrow = TRUE)

#' @importFrom R6 R6Class
NULL

# ---- Helpers ------------------------------------------------------------------------

.trial_error <- function(...) {
  stop(structure(list(message = paste0(...), call = NULL),
    class = c("trial_design_error", "error", "condition")))
}

.trial_key <- function(x) toupper(gsub("[[:space:]/-]+", "_", trimws(x)))

.trial_is_missing <- function(value) {
  is.null(value) || (is.atomic(value) && length(value) == 1L && is.na(value))
}

.trial_choose <- function(field, value, nullable = TRUE) {
  allowed <- .trial_vocabulary[[field]]
  shown <- paste(allowed, collapse = ", ")
  if (.trial_is_missing(value)) {
    if (nullable) return(NULL)
    .trial_error(field, " is required; choose one of: ", shown)
  }
  if (!is.character(value) || length(value) != 1L) {
    .trial_error(field, " must be a single string; choose one of: ", shown)
  }
  aliases <- .trial_aliases[[field]]
  if (!is.null(aliases)) aliases <- stats::setNames(.trial_key(aliases), .trial_key(names(aliases)))
  wanted <- .trial_key(value)
  candidates <- unique(c(wanted, gsub("_", "", wanted, fixed = TRUE)))
  for (candidate in candidates) {
    bare <- if (field == "phase") sub("^PHASE", "", sub("^PHASE_", "", candidate)) else candidate
    for (key in unique(c(candidate, bare))) {
      if (key %in% names(aliases)) candidates <- c(candidates, aliases[[key]])
    }
  }
  for (option in allowed) {
    key <- .trial_key(option)
    if (key %in% candidates || gsub("_", "", key, fixed = TRUE) %in% candidates) return(option)
  }
  .trial_error("'", value, "' is not a valid ", field, "; choose one of: ", shown)
}

.trial_phases <- function(value) {
  if (.trial_is_missing(value) || !length(value)) return(character())
  if (is.character(value) && length(value) == 1L) {
    parts <- strsplit(value, "[/,+&]| and ")[[1]]
    parts <- parts[nzchar(trimws(parts))]
    # "Phase 1/2" names its first phase once; later parts are bare numbers.
    if (length(parts) > 1L) value <- parts
  }
  vapply(as.list(value), function(p) .trial_choose("phase", p, nullable = FALSE), character(1))
}

.trial_text <- function(value, field, required = FALSE) {
  if (.trial_is_missing(value) || (is.character(value) && length(value) == 1L && !nzchar(trimws(value)))) {
    if (required) .trial_error(field, " must be a nonblank string")
    return(NULL)
  }
  if (!is.character(value) || length(value) != 1L) .trial_error(field, " must be a single string")
  trimws(value)
}

.trial_age <- function(value, field) {
  if (.trial_is_missing(value) || identical(value, "")) return(NULL)
  if (!is.numeric(value) || length(value) != 1L || value < 0) {
    .trial_error(field, " must be a non-negative number of years or NULL")
  }
  value
}

.trial_count <- function(value, field) {
  if (.trial_is_missing(value)) return(NULL)
  if (!is.numeric(value) || length(value) != 1L || value < 1 || value != round(value)) {
    .trial_error(field, " must be a positive whole number or NULL")
  }
  as.integer(value)
}

.trial_flag <- function(value, field) {
  if (.trial_is_missing(value)) return(NULL)
  if (!is.logical(value) || length(value) != 1L) .trial_error(field, " must be TRUE, FALSE or NULL")
  value
}

.trial_start_date <- function(value) {
  value <- .trial_text(value, "start_date")
  if (!is.null(value) && !grepl("^[0-9]{4}(-[0-9]{2}(-[0-9]{2})?)?$", value)) {
    .trial_error("start_date must be YYYY, YYYY-MM or YYYY-MM-DD")
  }
  value
}

# A number from form text ("18", "2.5"); blank means not stated.
.trial_form_number <- function(value) {
  if (.trial_is_missing(value) || identical(value, "")) return(NULL)
  number <- suppressWarnings(as.numeric(value))
  if (is.na(number)) return(NULL)
  if (grepl(".", as.character(value), fixed = TRUE)) number else as.integer(number)
}

.trial_or_null <- function(value) {
  if (.trial_is_missing(value) || identical(value, "")) NULL else value
}

.trial_label <- function(value) {
  if (is.null(value) || !nzchar(value)) return("\u2014")
  display <- c(NONE = "Open label", `NA` = "Not applicable", NON_RANDOMIZED = "Non-randomized")
  if (value %in% names(display)) return(unname(display[[value]]))
  text <- gsub("_", " ", value, fixed = TRUE)
  if (grepl("[a-z]", value)) return(text)
  gsub("\\b([a-z])", "\\U\\1", tolower(text), perl = TRUE)
}

.trial_format_value <- function(value) {
  if (is.null(value)) return("NULL")
  if (inherits(value, "R6")) return(class(value)[[1]])
  paste(format(unlist(value), trim = TRUE), collapse = "/")
}

.trial_contains <- function(items, item) any(vapply(items, identical, logical(1), item))

.trial_drop <- function(items, item) Filter(function(other) !identical(other, item), items)

.trial_frame <- function(headers, rows) {
  columns <- lapply(seq_along(headers), function(i) {
    vapply(rows, function(row) { v <- row[[i]]; if (is.null(v)) NA_character_ else as.character(v) }, character(1))
  })
  stats::setNames(as.data.frame(columns, stringsAsFactors = FALSE, col.names = paste0("V", seq_along(headers))), headers)
}

# ---- Vocabulary and field discovery ------------------------------------------------

#' Allowed values for trial design fields
#'
#' Every enumerated value a VERDICT trial design (\code{verdict.clinical-design/v1})
#' accepts. Offline; no API call. Endpoint families come from the release catalog:
#' \code{client$trial_catalog()}.
#'
#' @param field Optional field name (e.g. \code{"blinding"}) to return only its values.
#' @return A named list of character vectors, or one character vector when
#'   \code{field} is given. Unknown fields raise an error listing the known ones.
#' @examples
#' trial_vocabulary()
#' trial_vocabulary("blinding")
#' @export
trial_vocabulary <- function(field = NULL) {
  if (is.null(field)) return(structure(.trial_vocabulary, class = "verdict_trial_vocabulary"))
  if (!is.character(field) || length(field) != 1L || !field %in% names(.trial_vocabulary)) {
    .trial_error("Unknown field '", format(field), "'; known: ",
      paste(sort(names(.trial_vocabulary)), collapse = ", "))
  }
  .trial_vocabulary[[field]]
}

#' @export
print.verdict_trial_vocabulary <- function(x, ...) {
  for (name in names(x)) cat(name, ": ", paste(x[[name]], collapse = ", "), "\n", sep = "")
  invisible(x)
}

#' Editable trial design fields
#'
#' Every editable field of a \code{\link{Trial}}: its path, type, allowed values
#' and meaning. Offline; no API call.
#'
#' @return A data frame with columns \code{field}, \code{type},
#'   \code{allowed_values} and \code{meaning}.
#' @examples
#' trial_fields()
#' @export
trial_fields <- function() {
  allowed <- vapply(.trial_fields[, 3], function(v) {
    if (is.na(v)) "" else paste(.trial_vocabulary[[v]], collapse = ", ")
  }, character(1))
  data.frame(field = .trial_fields[, 1], type = .trial_fields[, 2],
    allowed_values = unname(allowed), meaning = .trial_fields[, 4], stringsAsFactors = FALSE)
}

# ---- Building blocks ----------------------------------------------------------------

# A record whose fields are validated on every edit; edits apply all-or-nothing.
TrialRecord <- R6::R6Class("TrialRecord",
  cloneable = FALSE,
  public = list(
    edit = function(...) {
      private$apply_changes(list(...))
      invisible(self)
    },
    to_list = function() {
      stats::setNames(lapply(private$fields, function(f) self[[f]]), private$fields)
    },
    print = function(...) {
      values <- vapply(private$fields, function(f) .trial_format_value(self[[f]]), character(1))
      cat(paste0(private$fields, ": ", values), sep = "\n")
      invisible(self)
    }
  ),
  private = list(
    fields = character(),
    check = function(name, value, state) value,
    consistent = function(state) invisible(NULL),
    apply_changes = function(changes) {
      if (length(changes) && (is.null(names(changes)) || any(!nzchar(names(changes))))) {
        .trial_error("edit() takes named fields; editable: ", paste(private$fields, collapse = ", "))
      }
      unknown <- setdiff(names(changes), private$fields)
      if (length(unknown)) {
        .trial_error("Unknown ", tolower(class(self)[[1]]), " field(s) ", paste(unknown, collapse = ", "),
          "; editable: ", paste(private$fields, collapse = ", "))
      }
      state <- self$to_list()
      for (name in names(changes)) state[name] <- list(private$check(name, changes[[name]], state))
      private$consistent(state)
      for (name in names(changes)) self[[name]] <- state[[name]]
      invisible(self)
    }
  )
)

#' Trial design building blocks
#'
#' \code{Treatment} is one component of an arm's regimen. Create one with
#' \code{Treatment$new("durvalumab", role = "study", dose = "1500 mg")} or, more
#' usually, through \code{arm$treatments$add(...)}. Fields: \code{name},
#' \code{role}, \code{timing}, \code{kind}, \code{chembl}, \code{smiles},
#' \code{name_only}, \code{dose}, \code{dose_stated}. At most one of
#' \code{chembl}, \code{smiles} or \code{name_only} may be set; otherwise the
#' name resolves on review. Change fields with \code{$edit(...)}; values are
#' matched forgivingly and invalid ones raise a \code{trial_design_error} that
#' lists the allowed values.
#'
#' @format An \code{R6::R6Class} generator.
#' @seealso \code{\link{Trial}}
#' @export
Treatment <- R6::R6Class("Treatment",
  inherit = TrialRecord, cloneable = FALSE,
  public = list(
    name = NULL, role = NULL, timing = NULL, kind = NULL, chembl = NULL, smiles = NULL,
    name_only = FALSE, dose = NULL, dose_stated = NULL,
    initialize = function(name, role = NULL, timing = NULL, kind = NULL, chembl = NULL,
                          smiles = NULL, name_only = FALSE, dose = NULL, dose_stated = NULL) {
      private$apply_changes(list(name = name, role = role, timing = timing, kind = kind,
        chembl = chembl, smiles = smiles, name_only = name_only, dose = dose, dose_stated = dose_stated))
    },
    identity = function() {
      if (!is.null(self$chembl)) return(paste("ChEMBL", self$chembl))
      if (!is.null(self$smiles)) return("SMILES")
      if (isTRUE(self$name_only)) return("name only")
      "resolve by name"
    },
    contract = function() {
      out <- list(name = self$name)
      for (field in c("kind", "role", "timing")) if (!is.null(self[[field]])) out[[field]] <- self[[field]]
      if (!is.null(self$chembl)) out$identifier <- list(kind = "chembl", value = self$chembl)
      else if (!is.null(self$smiles)) out$identifier <- list(kind = "smiles", value = self$smiles)
      else if (isTRUE(self$name_only)) out$identifier <- list(kind = "name_only")
      if (!is.null(self$dose)) out$doseNotes <- self$dose
      if (!is.null(self$dose_stated) || !is.null(self$dose)) {
        out$doseStated <- if (is.null(self$dose_stated)) !is.null(self$dose) else self$dose_stated
      }
      out
    },
    print = function(...) {
      cat("Treatment: ", self$name, " \u00b7 ", .trial_label(self$role), " \u00b7 ", self$identity(),
        if (!is.null(self$dose)) paste0(" \u00b7 ", self$dose), "\n", sep = "")
      invisible(self)
    }
  ),
  private = list(
    fields = c("name", "role", "timing", "kind", "chembl", "smiles", "name_only", "dose", "dose_stated"),
    check = function(name, value, state) {
      switch(name,
        name = .trial_text(value, "treatment name", required = TRUE),
        role = .trial_choose("treatment_role", value),
        timing = .trial_choose("timing", value),
        kind = .trial_choose("treatment_kind", value),
        chembl = {
          value <- .trial_text(value, "chembl")
          if (!is.null(value)) {
            value <- toupper(value)
            if (!grepl("^CHEMBL[0-9]+$", value)) .trial_error("chembl must look like CHEMBL1336")
          }
          value
        },
        smiles = {
          value <- .trial_text(value, "smiles")
          if (!is.null(value) && grepl("[[:space:]]", value)) .trial_error("smiles must not contain spaces")
          value
        },
        name_only = isTRUE(.trial_flag(value, "name_only")),
        dose = .trial_text(value, "dose"),
        dose_stated = .trial_flag(value, "dose_stated"),
        value)
    },
    consistent = function(state) {
      if (sum(!is.null(state$chembl), !is.null(state$smiles), isTRUE(state$name_only)) > 1L) {
        .trial_error("Set at most one of chembl, smiles or name_only")
      }
    }
  )
)

.treatment_from_contract <- function(data) {
  identifier <- data$identifier
  kind <- if (is.null(identifier$kind)) "" else identifier$kind
  Treatment$new(data$name, role = data$role, timing = data$timing, kind = data$kind,
    chembl = if (kind == "chembl") identifier$value, smiles = if (kind == "smiles") identifier$value,
    name_only = kind == "name_only", dose = data$doseNotes, dose_stated = data$doseStated)
}

# Shared list behaviour: lookup by position, name or item; listing; data frames.
TrialCollection <- R6::R6Class("TrialCollection",
  cloneable = FALSE,
  public = list(
    initialize = function(trial = NULL) {
      private$trial <- trial
    },
    count = function() length(private$items),
    list = function() private$items,
    names = function() vapply(private$items, function(item) item[[private$name_attr]], character(1)),
    get = function(key) {
      for (item in private$items) if (identical(item, key)) return(item)
      if (is.numeric(key) && length(key) == 1L && !is.na(key) && key == round(key)) {
        if (key < 1 || key > length(private$items)) {
          .trial_error("No ", private$noun, " at position ", key, "; there are ", length(private$items))
        }
        return(private$items[[key]])
      }
      if (is.character(key) && length(key) == 1L && !is.na(key)) {
        wanted <- tolower(trimws(key))
        for (item in private$items) if (tolower(item[[private$name_attr]]) == wanted) return(item)
      }
      .trial_error("No ", private$noun, " ", .trial_format_value(key), "; have: ",
        if (length(private$items)) paste(self$names(), collapse = ", ") else "none")
    },
    has = function(key) !inherits(tryCatch(self$get(key), error = function(e) e), "error"),
    index = function(key) {
      item <- self$get(key)
      which(vapply(private$items, identical, logical(1), item))[[1]]
    },
    move = function(key, position) {
      item <- self$get(key)
      rest <- .trial_drop(private$items, item)
      position <- max(1L, min(as.integer(position), length(rest) + 1L))
      private$items <- append(rest, list(item), after = position - 1L)
      invisible(private$trial)
    },
    to_data_frame = function() .trial_frame(private$headers(), private$rows()),
    print = function(...) {
      if (!length(private$items)) {
        cat("No ", private$noun, "s\n", sep = "")
      } else {
        lines <- vapply(seq_along(private$items), function(i) {
          row <- unlist(private$rows()[[i]])
          paste0(i, ". ", paste(row[!is.na(row) & nzchar(row) & row != "\u2014"], collapse = " \u00b7 "))
        }, character(1))
        cat(lines, sep = "\n")
      }
      invisible(self)
    },
    # Internal: append without the side effects of add().
    .push = function(item) {
      private$items[[length(private$items) + 1L]] <- item
      invisible(item)
    }
  ),
  private = list(
    trial = NULL, items = list(), noun = "item", name_attr = "label",
    headers = function() character(), rows = function() list()
  )
)

#' @export
length.TrialCollection <- function(x) x$count()

#' @export
as.data.frame.TrialCollection <- function(x, ...) x$to_data_frame()

TreatmentList <- R6::R6Class("TreatmentList",
  inherit = TrialCollection, cloneable = FALSE,
  public = list(
    add = function(name, ...) {
      treatment <- if (inherits(name, "Treatment")) name else Treatment$new(name, ...)
      if (length(private$items) >= 20L) .trial_error("An arm can have at most 20 treatments")
      self$.push(treatment)
    },
    remove = function(key) {
      item <- self$get(key)
      private$items <- .trial_drop(private$items, item)
      invisible(item)
    },
    edit = function(key, ...) self$get(key)$edit(...),
    clear = function() {
      private$items <- list()
      invisible(self)
    }
  ),
  private = list(
    noun = "treatment", name_attr = "name",
    headers = function() c("Treatment", "Role", "Timing", "Dose", "Identity"),
    rows = function() lapply(private$items, function(t) {
      list(t$name, .trial_label(t$role), .trial_label(t$timing), t$dose, t$identity())
    })
  )
)

TrialArm <- R6::R6Class("TrialArm",
  inherit = TrialRecord, cloneable = FALSE,
  public = list(
    label = NULL, type = NULL, treatments = NULL,
    initialize = function(trial, label, type = NULL) {
      private$trial <- trial
      self$treatments <- TreatmentList$new(trial)
      private$apply_changes(list(label = label, type = type))
    },
    to_list = function() {
      list(label = self$label, type = self$type,
        treatments = lapply(self$treatments$list(), function(t) t$to_list()))
    },
    print = function(...) {
      regimen <- paste(self$treatments$names(), collapse = " + ")
      cat("Arm: ", self$label, " \u00b7 ", .trial_label(self$type), " \u00b7 ",
        if (nzchar(regimen)) regimen else "no treatments", "\n", sep = "")
      invisible(self)
    }
  ),
  private = list(
    trial = NULL, fields = c("label", "type"),
    check = function(name, value, state) {
      if (name == "type") return(.trial_choose("arm_type", value))
      value <- .trial_text(value, "arm label", required = TRUE)
      arms <- private$trial$arms
      if (!is.null(arms)) for (other in arms$list()) {
        if (!identical(other, self) && tolower(other$label) == tolower(value)) {
          .trial_error("An arm named '", value, "' already exists")
        }
      }
      value
    }
  )
)

ArmList <- R6::R6Class("ArmList",
  inherit = TrialCollection, cloneable = FALSE,
  public = list(
    add = function(label, type = NULL, treatments = list()) {
      if (length(private$items) >= 12L) .trial_error("A trial can have at most 12 arms")
      arm <- TrialArm$new(private$trial, label, type)
      if (is.character(treatments)) treatments <- as.list(treatments)
      if (inherits(treatments, "Treatment")) treatments <- list(treatments)
      for (item in treatments) {
        if (is.list(item) && !inherits(item, "R6")) do.call(arm$treatments$add, item)
        else arm$treatments$add(item)
      }
      self$.push(arm)
    },
    remove = function(key) {
      arm <- self$get(key)
      private$trial$comparisons$.drop_arm(arm)
      private$items <- .trial_drop(private$items, arm)
      invisible(arm)
    },
    edit = function(key, ...) self$get(key)$edit(...),
    rename = function(key, label) self$edit(key, label = label),
    clear = function() {
      for (arm in private$items) self$remove(arm)
      invisible(self)
    }
  ),
  private = list(
    noun = "arm",
    headers = function() c("Arm", "Type", "Treatments"),
    rows = function() lapply(private$items, function(a) {
      regimen <- vapply(a$treatments$list(), function(t) {
        if (is.null(t$dose)) t$name else paste0(t$name, " (", t$dose, ")")
      }, character(1))
      list(a$label, .trial_label(a$type), if (length(regimen)) paste(regimen, collapse = " + ") else "\u2014")
    })
  )
)

TrialEndpoint <- R6::R6Class("TrialEndpoint",
  inherit = TrialRecord, cloneable = FALSE,
  public = list(
    title = NULL, role = NULL, family = NULL, category = NULL, effect_measure = NULL, hypothesis = NULL,
    time_frame = NULL, notes = NULL,
    initialize = function(trial, title, role = NULL, family = NULL, category = NULL, effect_measure = NULL,
                          hypothesis = NULL, time_frame = NULL, notes = NULL) {
      private$trial <- trial
      private$apply_changes(list(title = title, role = role, family = family, category = category,
        effect_measure = effect_measure, hypothesis = hypothesis, time_frame = time_frame, notes = notes))
    },
    print = function(...) {
      plan <- vapply(Filter(Negate(is.null), list(self$effect_measure, self$hypothesis)), .trial_label, character(1))
      cat("Endpoint: ", self$title, if (length(plan)) paste0(" \u00b7 ", paste(plan, collapse = " \u00b7 ")), "\n", sep = "")
      invisible(self)
    }
  ),
  private = list(
    trial = NULL,
    fields = c("title", "role", "family", "category", "effect_measure", "hypothesis", "time_frame", "notes"),
    check = function(name, value, state) {
      switch(name,
        title = {
          value <- .trial_text(value, "endpoint title", required = TRUE)
          endpoints <- private$trial$endpoints
          if (!is.null(endpoints)) for (other in endpoints$list()) {
            if (!identical(other, self) && tolower(other$title) == tolower(value)) {
              .trial_error("An endpoint titled '", value, "' already exists")
            }
          }
          value
        },
        family = {
          value <- .trial_text(value, "family")
          if (is.null(value)) NULL else gsub(" ", "_", tolower(value), fixed = TRUE)
        },
        role = .trial_choose("endpoint_role", value),
        category = .trial_choose("endpoint_category", value),
        effect_measure = .trial_choose("effect_measure", value),
        hypothesis = .trial_choose("hypothesis", value),
        .trial_text(value, name))
    }
  )
)

EndpointList <- R6::R6Class("EndpointList",
  inherit = TrialCollection, cloneable = FALSE,
  public = list(
    add = function(title, ...) {
      if (length(private$items) >= 20L) .trial_error("A trial can have at most 20 endpoints")
      endpoint <- TrialEndpoint$new(private$trial, title, ...)
      self$.push(endpoint)
      # New endpoints join every existing comparison; edit comparisons to change that.
      for (comparison in private$trial$comparisons$list()) {
        comparison$endpoints[[length(comparison$endpoints) + 1L]] <- endpoint
      }
      invisible(endpoint)
    },
    remove = function(key) {
      endpoint <- self$get(key)
      private$items <- .trial_drop(private$items, endpoint)
      private$trial$comparisons$.drop_endpoint(endpoint)
      invisible(endpoint)
    },
    edit = function(key, ...) self$get(key)$edit(...),
    clear = function() {
      for (endpoint in private$items) self$remove(endpoint)
      invisible(self)
    }
  ),
  private = list(
    noun = "endpoint", name_attr = "title",
    headers = function() c("Endpoint", "Role", "Family", "Effect measure", "Hypothesis", "Time frame"),
    rows = function() lapply(private$items, function(e) {
      list(e$title, .trial_label(if (is.null(e$role)) "primary" else e$role), .trial_label(e$family), .trial_label(e$effect_measure), .trial_label(e$hypothesis), e$time_frame)
    })
  )
)

TrialComparison <- R6::R6Class("TrialComparison",
  cloneable = FALSE,
  public = list(
    experimental = NULL, comparator = NULL, endpoints = list(),
    initialize = function(experimental, comparator, endpoints) {
      self$experimental <- experimental
      self$comparator <- comparator
      self$endpoints <- endpoints
    },
    print = function(...) {
      cat("Comparison: ", self$label, " on ",
        paste(vapply(self$endpoints, function(e) e$title, character(1)), collapse = ", "), "\n", sep = "")
      invisible(self)
    }
  ),
  active = list(
    label = function() paste(self$experimental$label, "vs", self$comparator$label)
  )
)

ComparisonList <- R6::R6Class("ComparisonList",
  inherit = TrialCollection, cloneable = FALSE,
  public = list(
    get = function(key, comparator = NULL) {
      if (!is.null(comparator)) {
        experimental <- private$trial$arms$get(key)
        comparator <- private$trial$arms$get(comparator)
        for (item in private$items) {
          if (identical(item$experimental, experimental) && identical(item$comparator, comparator)) return(item)
        }
        .trial_error("No comparison ", experimental$label, " vs ", comparator$label)
      }
      if (is.character(key) && length(key) == 1L && grepl("\\s+vs\\.?\\s+", key, ignore.case = TRUE)) {
        parts <- regmatches(key, regexpr("\\s+vs\\.?\\s+", key, ignore.case = TRUE), invert = TRUE)[[1]]
        return(self$get(parts[[1]], parts[[2]]))
      }
      super$get(key)
    },
    add = function(experimental, comparator, endpoints = NULL) {
      exp <- private$trial$arms$get(experimental)
      comp <- private$trial$arms$get(comparator)
      if (identical(exp, comp)) .trial_error("A comparison needs two different arms")
      chosen <- private$endpoints_of(endpoints)
      for (item in private$items) {
        same <- identical(item$experimental, exp) && identical(item$comparator, comp)
        reversed <- identical(item$experimental, comp) && identical(item$comparator, exp)
        if (same || reversed) {
          item$experimental <- exp
          item$comparator <- comp
          item$endpoints <- chosen
          return(invisible(item))
        }
      }
      if (length(private$items) >= 24L) .trial_error("A trial can have at most 24 comparisons")
      self$.push(TrialComparison$new(exp, comp, chosen))
    },
    remove = function(key, comparator = NULL) {
      item <- self$get(key, comparator)
      private$items <- .trial_drop(private$items, item)
      invisible(item)
    },
    edit = function(key, comparator = NULL, endpoints = NULL, add_endpoints = NULL, remove_endpoints = NULL) {
      item <- self$get(key, comparator)
      chosen <- if (is.null(endpoints)) item$endpoints else private$endpoints_of(endpoints)
      for (endpoint in private$endpoints_of(add_endpoints, default_all = FALSE)) {
        if (!.trial_contains(chosen, endpoint)) chosen[[length(chosen) + 1L]] <- endpoint
      }
      removing <- private$endpoints_of(remove_endpoints, default_all = FALSE)
      chosen <- Filter(function(e) !.trial_contains(removing, e), chosen)
      if (!length(chosen)) .trial_error("A comparison needs at least one endpoint; remove it instead")
      item$endpoints <- chosen
      item
    },
    swap = function(key, comparator = NULL) {
      item <- self$get(key, comparator)
      experimental <- item$experimental
      item$experimental <- item$comparator
      item$comparator <- experimental
      item
    },
    clear = function() {
      private$items <- list()
      invisible(self)
    },
    all_pairs = function(comparator, endpoints = NULL) {
      control <- private$trial$arms$get(comparator)
      others <- Filter(function(arm) !identical(arm, control), private$trial$arms$list())
      invisible(lapply(others, function(arm) self$add(arm, control, endpoints)))
    },
    .drop_arm = function(arm) {
      private$items <- Filter(function(c) !identical(c$experimental, arm) && !identical(c$comparator, arm), private$items)
    },
    .drop_endpoint = function(endpoint) {
      for (item in private$items) item$endpoints <- .trial_drop(item$endpoints, endpoint)
      private$items <- Filter(function(c) length(c$endpoints) > 0L, private$items)
    }
  ),
  private = list(
    noun = "comparison",
    endpoints_of = function(keys, default_all = TRUE) {
      endpoints <- private$trial$endpoints
      if (is.null(keys)) {
        if (!default_all) return(list())
        if (!endpoints$count()) .trial_error("Add an endpoint before comparing arms")
        return(endpoints$list())
      }
      if (inherits(keys, "R6") || is.atomic(keys)) keys <- as.list(if (inherits(keys, "R6")) list(keys) else keys)
      lapply(keys, endpoints$get)
    },
    headers = function() c("Experimental", "Comparator", "Endpoints"),
    rows = function() lapply(private$items, function(c) {
      list(c$experimental$label, c$comparator$label,
        paste(vapply(c$endpoints, function(e) e$title, character(1)), collapse = ", "))
    })
  )
)

SafetyQuery <- R6::R6Class("SafetyQuery",
  cloneable = FALSE,
  public = list(
    term = NULL, seriousness = NULL,
    initialize = function(term, seriousness = "serious") {
      self$term <- .trial_text(term, "term", required = TRUE)
      self$seriousness <- .trial_choose("seriousness", seriousness, nullable = FALSE)
    },
    print = function(...) {
      cat("Safety query: ", self$term, " (", self$seriousness, ")\n", sep = "")
      invisible(self)
    }
  )
)

SafetyQueryList <- R6::R6Class("SafetyQueryList",
  inherit = TrialCollection, cloneable = FALSE,
  public = list(
    add = function(term, seriousness = "serious") self$.push(SafetyQuery$new(term, seriousness)),
    remove = function(key) {
      item <- self$get(key)
      private$items <- .trial_drop(private$items, item)
      invisible(item)
    },
    clear = function() {
      private$items <- list()
      invisible(self)
    }
  ),
  private = list(
    noun = "safety query", name_attr = "term",
    headers = function() c("Term", "Seriousness"),
    rows = function() lapply(private$items, function(q) list(q$term, q$seriousness))
  )
)

TrialDetails <- R6::R6Class("TrialDetails",
  inherit = TrialRecord, cloneable = FALSE,
  public = list(
    name = NULL, phases = character(), start_date = NULL, allocation = NULL, blinding = NULL,
    purpose = NULL, sponsor_type = NULL, arm_count = NULL, primary_endpoint_count = NULL,
    initialize = function(name) private$apply_changes(list(name = name)),
    edit = function(...) {
      changes <- list(...)
      # The singular is the common case.
      names(changes)[names(changes) == "phase"] <- "phases"
      private$apply_changes(changes)
      invisible(self)
    }
  ),
  active = list(
    phase = function() {
      if (!length(self$phases)) return("\u2014")
      labels <- sub("^PHASE", "Phase ", sub("^EARLY_PHASE", "Early phase ", self$phases))
      paste(labels, collapse = " / ")
    }
  ),
  private = list(
    fields = c("name", "phases", "start_date", "allocation", "blinding", "purpose", "sponsor_type",
      "arm_count", "primary_endpoint_count"),
    check = function(name, value, state) {
      switch(name,
        name = .trial_text(value, "name", required = TRUE),
        phases = .trial_phases(value),
        start_date = .trial_start_date(value),
        allocation = , blinding = , purpose = , sponsor_type = .trial_choose(name, value),
        .trial_count(value, name))
    }
  )
)

TrialPopulation <- R6::R6Class("TrialPopulation",
  inherit = TrialRecord, cloneable = FALSE,
  public = list(
    conditions = list(), sex = NULL, min_age = NULL, max_age = NULL, healthy_volunteers = NULL,
    notes = NULL,
    set_disease = function(name, mondo_id = NULL) {
      private$apply_changes(list(conditions = list(list(name = name, mondoId = mondo_id))))
      invisible(self)
    }
  ),
  active = list(
    disease = function() if (length(self$conditions)) self$conditions[[1]]$name else NULL,
    summary = function() {
      parts <- c(if (!is.null(self$disease)) self$disease)
      if (!is.null(self$sex) && self$sex != "all") parts <- c(parts, paste(self$sex, "only"))
      if (!is.null(self$min_age) || !is.null(self$max_age)) {
        low <- if (is.null(self$min_age)) "?" else format(self$min_age)
        high <- if (is.null(self$max_age)) "+" else paste0("\u2013", format(self$max_age))
        parts <- c(parts, paste0("ages ", low, high))
      }
      if (isTRUE(self$healthy_volunteers)) parts <- c(parts, "healthy volunteers")
      if (length(parts)) paste(parts, collapse = " \u00b7 ") else "\u2014"
    }
  ),
  private = list(
    fields = c("conditions", "sex", "min_age", "max_age", "healthy_volunteers", "notes"),
    check = function(name, value, state) {
      switch(name,
        conditions = {
          if (is.null(value)) value <- list()
          if (is.character(value)) value <- as.list(value)
          if (length(value) > 20L) .trial_error("At most 20 conditions")
          lapply(value, function(condition) {
            if (is.character(condition)) condition <- list(name = condition)
            out <- list(name = .trial_text(condition$name, "disease", required = TRUE))
            mondo <- .trial_or_null(if (!is.null(condition$mondoId)) condition$mondoId else condition$mondo_id)
            if (!is.null(mondo)) {
              mondo <- toupper(trimws(mondo))
              if (!grepl("^MONDO:[0-9]{7}$", mondo)) .trial_error("mondo_id must look like MONDO:0007256")
              out$mondoId <- mondo
            }
            out
          })
        },
        sex = .trial_choose("sex", value),
        min_age = , max_age = .trial_age(value, name),
        healthy_volunteers = .trial_flag(value, "healthy_volunteers"),
        notes = .trial_text(value, "notes"))
    },
    consistent = function(state) {
      if (!is.null(state$min_age) && !is.null(state$max_age) && state$min_age > state$max_age) {
        .trial_error("min_age must not exceed max_age")
      }
    }
  )
)

# ---- The trial ----------------------------------------------------------------------

.trial_detail_keys <- c("name", "phases", "phase", "start_date", "allocation", "blinding", "purpose",
  "sponsor_type", "arm_count", "primary_endpoint_count")
.trial_population_keys <- c("conditions", "sex", "min_age", "max_age", "healthy_volunteers", "notes",
  "disease", "mondo_id")

#' Build, edit, review and run a VERDICT trial design
#'
#' A \code{Trial} is an editable \code{verdict.clinical-design/v1} design: the
#' same clinical-name contract the VERDICT form submits. Nothing contacts the
#' API until \code{$review()} or \code{$run()}; every edit is checked offline
#' against the contract vocabulary, matching values forgivingly ("phase 3",
#' "double", "placebo") and raising a \code{trial_design_error} that lists the
#' allowed values when nothing matches. Edits apply all-or-nothing.
#'
#' @section Sections:
#' \describe{
#'   \item{\code{details}}{Name, phases, start date, allocation, blinding, purpose,
#'     sponsor type, arm count, primary endpoint count. Change with
#'     \code{trial$details$edit(phase = "phase 3", blinding = "double")}.}
#'   \item{\code{population}}{Conditions, sex, ages, healthy volunteers, notes.
#'     \code{trial$population$set_disease("hepatocellular carcinoma")}.}
#'   \item{\code{arms}, \code{endpoints}, \code{comparisons}, \code{safety_queries}}{
#'     Lists with \code{add()}, \code{remove()}, \code{edit()}, \code{list()},
#'     \code{names()}, \code{get()} (by 1-based position or case-insensitive
#'     name), \code{has()}, \code{move()}, \code{clear()} and
#'     \code{to_data_frame()}. Each arm has \code{treatments} with the same
#'     methods. Comparisons are looked up as \code{"A vs B"} or
#'     \code{get("A", "B")}, and also offer \code{swap()} and
#'     \code{all_pairs(control)}. Removing an arm or endpoint updates the
#'     comparisons that use it; new endpoints join every existing comparison.}
#'   \item{\code{analyses}}{Requested analyses (default efficacy, toxicity, biology).}
#' }
#'
#' @section Methods:
#' \describe{
#'   \item{\code{Trial$new(name = "Untitled trial")}}{A new, empty design.}
#'   \item{\code{$edit(...)}}{Change details and population fields in one call,
#'     e.g. \code{trial$edit(phase = "phase 2", sex = "male", min_age = 18)}.}
#'   \item{\code{$describe()}}{Every editable field (see \code{\link{trial_fields}}).}
#'   \item{\code{$copy(name = NULL)}, \code{$variant(name = NULL, ...)}}{Independent
#'     copies, optionally with changes.}
#'   \item{\code{$vary(...)}}{Every combination of the given values as a
#'     \code{\link{TrialSet}}: \code{trial$vary(sex = c("female", "male"), min_age = c(18, 65))}.}
#'   \item{\code{$to_list()}, \code{$to_json()}, \code{$save(path)}}{The API request
#'     body; see \code{\link{load_trial_design}} and \code{\link{trial_from_list}}.}
#'   \item{\code{$review(client, idempotency_key = NULL)}}{Ask the server to resolve
#'     names and check every analysis; runs nothing. Returns a
#'     \code{verdict_trial_review}.}
#'   \item{\code{$run(client, idempotency_key, skip_unavailable = TRUE, wait = TRUE,
#'     timeout = 3600, poll_interval = 5)}}{Review, submit and (by default) wait
#'     for a trial result. Analyses the deployed models cannot score are skipped
#'     and named in the review, like the VERDICT form; Toxicity is skipped when the
#'     release needs a safety query and none is given. Reuse the key to recover the
#'     same run. With \code{wait = FALSE} the submission receipt is returned.
#'     A design the server cannot run raises \code{trial_review_error}.}
#' }
#'
#' @format An \code{R6::R6Class} generator.
#' @seealso \code{\link{trial_vocabulary}}, \code{\link{trial_fields}},
#'   \code{\link{trial_from_import}}, \code{\link{TrialSet}}, \code{\link{verdict_client}}
#' @examples
#' trial <- Trial$new("HCC phase 3")
#' trial$population$set_disease("hepatocellular carcinoma")
#' trial$edit(phase = "phase 3", allocation = "randomized", blinding = "open label", min_age = 18)
#' trial$arms$add("Durvalumab", type = "experimental", treatments = "durvalumab")
#' trial$arms$add("Sorafenib", type = "active comparator", treatments = "sorafenib")
#' trial$endpoints$add("Overall survival", family = "overall_survival",
#'   effect_measure = "hazard ratio", hypothesis = "superiority")
#' trial
#' by_sex <- trial$vary(sex = c("female", "male"))
#' @export
Trial <- R6::R6Class("Trial",
  cloneable = FALSE,
  public = list(
    details = NULL, population = NULL, arms = NULL, endpoints = NULL, comparisons = NULL,
    safety_queries = NULL,
    initialize = function(name = "Untitled trial") {
      self$details <- TrialDetails$new(name)
      self$population <- TrialPopulation$new()
      self$arms <- ArmList$new(self)
      self$endpoints <- EndpointList$new(self)
      self$comparisons <- ComparisonList$new(self)
      self$safety_queries <- SafetyQueryList$new(self)
    },
    describe = function() trial_fields(),
    edit = function(...) {
      changes <- list(...)
      if (length(changes) && (is.null(names(changes)) || any(!nzchar(names(changes))))) {
        .trial_error("edit() takes named fields; see trial_fields()")
      }
      unknown <- setdiff(names(changes), c(.trial_detail_keys, .trial_population_keys, "analyses"))
      if (length(unknown)) {
        .trial_error("Unknown field(s) ", paste(unknown, collapse = ", "), "; see trial_fields() for editable fields")
      }
      staged <- self$copy()
      staged$.apply(changes)
      self$.apply(changes)
      invisible(self)
    },
    .apply = function(changes) {
      details <- changes[names(changes) %in% .trial_detail_keys]
      population <- changes[names(changes) %in% .trial_population_keys]
      if (length(details)) do.call(self$details$edit, details)
      if ("disease" %in% names(population) || "mondo_id" %in% names(population)) {
        disease <- population$disease
        if (is.null(disease)) disease <- self$population$disease
        self$population$set_disease(if (is.null(disease)) "" else disease, population$mondo_id)
        population <- population[!names(population) %in% c("disease", "mondo_id")]
      }
      if (length(population)) do.call(self$population$edit, population)
      if ("analyses" %in% names(changes)) self$analyses <- changes$analyses
      invisible(self)
    },
    copy = function(name = NULL) {
      twin <- trial_from_list(self$to_list())
      if (!is.null(name)) twin$details$edit(name = name)
      twin
    },
    variant = function(name = NULL, ...) {
      twin <- self$copy(name)
      twin$edit(...)
      twin
    },
    vary = function(...) {
      sweeps <- list(...)
      if (!length(sweeps) || is.null(names(sweeps)) || any(!nzchar(names(sweeps)))) {
        .trial_error("Pass at least one named field to vary, e.g. vary(sex = c(\"female\", \"male\"))")
      }
      combos <- list(list())
      for (field in names(sweeps)) {
        values <- as.list(sweeps[[field]])
        combos <- unlist(lapply(combos, function(combo) {
          lapply(values, function(value) c(combo, stats::setNames(list(value), field)))
        }), recursive = FALSE)
      }
      variants <- TrialSet$new()
      for (combo in combos) {
        label <- paste(paste0(names(combo), "=", vapply(combo, .trial_format_value, character(1))), collapse = ", ")
        variants$add(label, do.call(self$variant, c(list(name = paste0(self$name, " (", label, ")")), combo)))
      }
      variants
    },
    to_list = function() {
      d <- self$details
      p <- self$population
      arms <- self$arms$list()
      endpoints <- self$endpoints$list()
      position <- function(items, item) which(vapply(items, identical, logical(1), item))[[1]] - 1L
      population <- list(conditions = p$conditions, phases = as.list(d$phases), sex = p$sex,
        healthyVolunteers = p$healthy_volunteers, minAge = p$min_age, maxAge = p$max_age)
      if (!is.null(p$notes)) population$notes <- p$notes
      metadata <- list(allocation = d$allocation, blinding = d$blinding, purpose = d$purpose,
        sponsorType = d$sponsor_type,
        # Registered arms cannot be fewer than the arms being modeled.
        armCount = if (is.null(d$arm_count)) NULL else max(d$arm_count, length(arms)),
        primaryEndpointCount = d$primary_endpoint_count)
      if (!is.null(d$start_date)) metadata$startDate <- d$start_date
      design <- list(
        schemaVersion = .trial_schema_version,
        name = d$name,
        population = population,
        arms = lapply(arms, function(a) list(label = a$label, type = a$type,
          components = lapply(a$treatments$list(), function(t) t$contract()))),
        endpoints = lapply(endpoints, function(e) {
          # role is omitted when unset so servers without the role field still accept the design.
          out <- list(title = e$title, role = e$role, family = e$family, category = e$category,
            effectMeasure = e$effect_measure, hypothesis = e$hypothesis, timeFrame = e$time_frame,
            notes = e$notes)
          keep <- !vapply(out, is.null, logical(1)) | names(out) %in% c("effectMeasure", "hypothesis")
          out[keep]
        }),
        metadata = metadata,
        requestedAnalyses = as.list(private$analyses_)
      )
      # Two arms and no comparison: the server compares the first against the second.
      if (self$comparisons$count() || length(arms) != 2L) {
        design$comparisons <- lapply(self$comparisons$list(), function(c) list(
          experimentalArm = position(arms, c$experimental),
          comparatorArm = position(arms, c$comparator),
          endpoints = lapply(c$endpoints, function(e) position(endpoints, e))))
      }
      if (self$safety_queries$count()) {
        design$safetyQueries <- lapply(self$safety_queries$list(), function(q) {
          list(term = q$term, seriousness = q$seriousness)
        })
      }
      design
    },
    to_json = function(pretty = TRUE) {
      jsonlite::toJSON(self$to_list(), auto_unbox = TRUE, null = "null", digits = NA, pretty = pretty)
    },
    save = function(path) {
      writeLines(self$to_json(), path, useBytes = TRUE)
      invisible(path)
    },
    review = function(client, idempotency_key = NULL) {
      .verdict_trial_review(client$prepare_trial_design(self$to_list(), idempotency_key), self)
    },
    run = function(client, idempotency_key, skip_unavailable = TRUE, wait = TRUE, timeout = 3600,
                   poll_interval = 5) {
      if (missing(idempotency_key) || is.null(idempotency_key)) {
        stop("run() requires an idempotency_key; reuse it to recover the same run", call. = FALSE)
      }
      design <- self$to_list()
      review <- .verdict_trial_review(client$prepare_trial_design(design, idempotency_key), self)
      if (skip_unavailable) {
        skipped <- review$unavailable
        # Releases without the full Safety catalog need a query to run Toxicity.
        needs_query <- any(vapply(review$issues, function(i) {
          is.character(i$field) && startsWith(i$field, "query.")
        }, logical(1)))
        if (is.null(design$safetyQueries) && needs_query) skipped <- union(skipped, "toxicity")
        requested <- unlist(design$requestedAnalyses)
        remaining <- requested[!requested %in% skipped]
        if (length(skipped) && length(remaining)) {
          design$requestedAnalyses <- as.list(remaining)
          review <- .verdict_trial_review(client$prepare_trial_design(design, idempotency_key), self)
        }
      }
      if (!review$ok) .verdict_trial_review_error(review)
      receipt <- client$run_trial_design(design, review$fingerprint, idempotency_key = idempotency_key)
      if (!wait) return(receipt)
      client$wait_for_trial(receipt$id, timeout = timeout, poll_interval = poll_interval)
    },
    print = function(...) {
      d <- self$details
      design <- Filter(Negate(is.null), list(d$allocation, d$blinding, d$purpose))
      indent <- function(x) paste0("  ", utils::capture.output(print(x)))
      cat(c(
        paste("Trial:", d$name),
        paste0("  ", d$phase, " \u00b7 ", self$population$summary),
        paste0("  Design: ", if (length(design)) paste(vapply(design, .trial_label, ""), collapse = " \u00b7 ") else "not stated"),
        "Arms:", indent(self$arms),
        "Endpoints:", indent(self$endpoints),
        "Comparisons:", indent(self$comparisons),
        if (self$safety_queries$count()) c("Safety queries:", indent(self$safety_queries)),
        paste("Analyses:", paste(private$analyses_, collapse = ", "))
      ), sep = "\n")
      invisible(self)
    }
  ),
  active = list(
    name = function(value) {
      if (missing(value)) return(self$details$name)
      self$details$edit(name = value)
    },
    analyses = function(value) {
      if (missing(value)) return(private$analyses_)
      chosen <- vapply(as.list(value), function(v) .trial_choose("analysis", v, nullable = FALSE), character(1))
      if (!length(chosen)) .trial_error("Request at least one analysis")
      private$analyses_ <- unique(chosen)
    }
  ),
  private = list(analyses_ = .trial_default_analyses)
)

# ---- Loading and importing designs ---------------------------------------------------

#' Build a trial design from a list, a saved file or a Trial Import
#'
#' \code{trial_from_list()} builds a \code{\link{Trial}} from a
#' \code{verdict.clinical-design/v1} list (as from \code{trial$to_list()}).
#' \code{load_trial_design()} reads a design saved with \code{trial$save(path)}.
#' \code{trial_from_import()} builds one from \code{client$import_trial(...)}
#' output; imported values are suggestions to review, and the import's evidence
#' stays in \code{proposal$metadata}. None of these contact the API.
#'
#' @param design A clinical-design list.
#' @param path Path to a design JSON file.
#' @param proposal The result of \code{client$import_trial()} (or its \code{draft}).
#' @return A \code{\link{Trial}}.
#' @export
trial_from_list <- function(design) {
  if (!is.null(design$schemaVersion) && !identical(design$schemaVersion, .trial_schema_version)) {
    .trial_error("Expected schemaVersion '", .trial_schema_version, "'")
  }
  trial <- Trial$new(if (is.null(.trial_or_null(design$name))) "Untitled trial" else design$name)
  population <- design$population
  metadata <- design$metadata
  trial$details$edit(phases = unlist(population$phases), start_date = metadata$startDate,
    allocation = metadata$allocation, blinding = metadata$blinding, purpose = metadata$purpose,
    sponsor_type = metadata$sponsorType, arm_count = metadata$armCount,
    primary_endpoint_count = metadata$primaryEndpointCount)
  trial$population$edit(conditions = population$conditions, sex = population$sex,
    min_age = population$minAge, max_age = population$maxAge,
    healthy_volunteers = population$healthyVolunteers, notes = population$notes)
  for (arm in design$arms) {
    added <- trial$arms$add(arm$label, arm$type)
    for (component in arm$components) added$treatments$add(.treatment_from_contract(component))
  }
  for (endpoint in design$endpoints) {
    title <- .trial_or_null(endpoint$title)
    if (is.null(title)) title <- if (is.null(endpoint$category)) "Endpoint" else .trial_label(endpoint$category)
    trial$endpoints$.push(TrialEndpoint$new(trial, title, role = endpoint$role, family = endpoint$family,
      category = endpoint$category, effect_measure = endpoint$effectMeasure,
      hypothesis = endpoint$hypothesis, time_frame = endpoint$timeFrame, notes = endpoint$notes))
  }
  arms <- trial$arms$list()
  endpoints <- trial$endpoints$list()
  for (comparison in design$comparisons) {
    trial$comparisons$.push(TrialComparison$new(arms[[comparison$experimentalArm + 1L]],
      arms[[comparison$comparatorArm + 1L]],
      lapply(comparison$endpoints, function(i) endpoints[[i + 1L]])))
  }
  for (query in design$safetyQueries) trial$safety_queries$add(query$term, query$seriousness)
  if (length(design$requestedAnalyses)) trial$analyses <- unlist(design$requestedAnalyses)
  trial
}

#' @rdname trial_from_list
#' @export
load_trial_design <- function(path) {
  trial_from_list(jsonlite::fromJSON(path, simplifyVector = FALSE))
}

#' @rdname trial_from_list
#' @export
trial_from_import <- function(proposal) {
  draft <- if (!is.null(proposal$draft)) proposal$draft else proposal
  design <- if (!is.null(draft$custom$arms)) draft$custom else draft
  name <- .trial_or_null(design$name)
  trial <- Trial$new(if (is.null(name)) "Imported trial" else name)
  outcome <- design$outcomeDesign
  endpoints <- design$endpoints
  counts <- unique(unlist(lapply(endpoints, function(e) .trial_or_null(e$analysis$primaryEndpoints))))
  trial$details$edit(phases = unlist(design$phases), start_date = .trial_or_null(design$startDate),
    allocation = .trial_or_null(outcome$allocation), blinding = .trial_or_null(outcome$masking),
    purpose = .trial_or_null(outcome$primaryPurpose), sponsor_type = .trial_or_null(outcome$sponsorClass),
    arm_count = .trial_form_number(outcome$numberOfArms),
    primary_endpoint_count = if (length(counts) == 1L) .trial_form_number(counts) else NULL)
  healthy <- c(yes = TRUE, no = FALSE)
  trial$population$edit(
    conditions = Filter(function(c) !is.null(.trial_or_null(c$name)), design$conditions),
    sex = .trial_or_null(design$sex),
    min_age = .trial_form_number(design$minAge), max_age = .trial_form_number(design$maxAge),
    healthy_volunteers = if (isTRUE(design$healthyVolunteers %in% names(healthy))) healthy[[design$healthyVolunteers]] else NULL,
    notes = .trial_or_null(design$population))
  arm_ids <- list()
  for (arm in design$arms) {
    kind <- tolower(if (is.null(arm$registeredType)) "" else arm$registeredType)
    if (kind == "sham_comparator") kind <- "placebo_comparator"
    label <- .trial_or_null(arm$label)
    added <- trial$arms$add(if (is.null(label)) paste("Arm", trial$arms$count() + 1L) else label,
      if (kind %in% .trial_vocabulary$arm_type) kind else NULL)
    if (!is.null(arm$id)) arm_ids[[arm$id]] <- added
    for (t in arm$treatments) {
      if (is.null(.trial_or_null(t$name))) next
      identity <- if (is.null(t$identity)) "" else t$identity
      added$treatments$add(t$name,
        role = if (identical(t$role, "active")) "study" else .trial_or_null(t$role),
        timing = .trial_or_null(t$timing), kind = .trial_or_null(t$kind),
        chembl = if (identity == "chembl") .trial_or_null(t$chemblId),
        smiles = if (identity == "smiles") .trial_or_null(t$smiles),
        name_only = identity == "name_only", dose = .trial_or_null(t$dose), dose_stated = t$doseStated)
    }
  }
  endpoint_ids <- list()
  for (e in endpoints) {
    analysis <- e$analysis
    title <- .trial_or_null(analysis$title)
    if (is.null(title)) title <- .trial_or_null(e$name)
    if (is.null(title)) title <- paste("Endpoint", trial$endpoints$count() + 1L)
    if (tolower(title) %in% tolower(trial$endpoints$names())) title <- paste0(title, " (", trial$endpoints$count() + 1L, ")")
    endpoint <- TrialEndpoint$new(trial, title, role = .trial_or_null(analysis$endpointRole),
      family = .trial_or_null(e$family),
      category = if (isTRUE(e$kind %in% .trial_vocabulary$endpoint_category)) e$kind else NULL,
      effect_measure = .trial_or_null(analysis$effectType), hypothesis = .trial_or_null(analysis$hypothesis),
      time_frame = .trial_or_null(e$timeFrame), notes = .trial_or_null(e$definition))
    trial$endpoints$.push(endpoint)
    if (!is.null(e$id)) endpoint_ids[[e$id]] <- endpoint
  }
  for (c in design$comparisons) {
    exp <- if (is.character(c$experimental)) arm_ids[[c$experimental]]
    comp <- if (is.character(c$comparator)) arm_ids[[c$comparator]]
    chosen <- Filter(Negate(is.null), lapply(unlist(c$endpoints), function(i) endpoint_ids[[i]]))
    if (!is.null(exp) && !is.null(comp) && !identical(exp, comp) && length(chosen)) {
      trial$comparisons$.push(TrialComparison$new(exp, comp, chosen))
    }
  }
  for (q in draft$queries) {
    if (!is.null(.trial_or_null(q$term))) {
      trial$safety_queries$add(q$term, if (is.null(.trial_or_null(q$seriousness))) "serious" else q$seriousness)
    }
  }
  trial
}

# ---- Review ------------------------------------------------------------------------

.verdict_trial_review <- function(response, trial = NULL) {
  availability <- if (is.null(response$availability)) list() else response$availability
  outcome <- vapply(availability, function(a) if (is.null(a$outcome)) "" else a$outcome, character(1))
  analysis <- vapply(availability, function(a) a$analysis, character(1))
  review <- list(raw = response, trial = trial,
    issues = if (is.null(response$issues)) list() else response$issues,
    entities = if (is.null(response$entities)) list() else response$entities,
    assumptions = unlist(response$assumptions),
    availability = availability,
    fingerprint = response$reviewFingerprint,
    normalized = response$normalized,
    available = analysis[outcome != "unavailable"],
    unavailable = analysis[outcome == "unavailable"])
  review$ok <- !is.null(review$fingerprint) && nzchar(review$fingerprint) &&
    !length(review$issues) && length(review$available) > 0L
  structure(review, class = "verdict_trial_review")
}

.verdict_trial_review_error <- function(review) {
  problems <- paste(vapply(review$issues, function(i) if (is.null(i$message)) "" else i$message, character(1)),
    collapse = "; ")
  if (!nzchar(problems)) problems <- "no runnable analysis"
  stop(structure(list(message = paste("Trial design not runnable:", problems), call = NULL, review = review),
    class = c("trial_review_error", "error", "condition")))
}

#' Trial design reviews
#'
#' \code{trial$review(client)} returns a \code{verdict_trial_review}: a list with
#' \code{ok}, \code{issues}, \code{entities}, \code{assumptions},
#' \code{availability}, \code{available}, \code{unavailable}, \code{fingerprint},
#' \code{normalized} and \code{raw}. Printing summarizes it;
#' \code{as.data.frame()} gives analysis availability.
#'
#' @param x A \code{verdict_trial_review}.
#' @param ... Unused.
#' @return \code{print} returns \code{x} invisibly; \code{as.data.frame} returns a
#'   data frame with \code{analysis}, \code{outcome} and \code{reasons}.
#' @export
print.verdict_trial_review <- function(x, ...) {
  lines <- paste("Review:", if (x$ok) "ready to run" else "needs attention")
  for (i in x$issues) lines <- c(lines, trimws(paste("  issue:", if (is.null(i$field)) "" else i$field, i$message), "right"))
  for (a in x$availability) {
    reasons <- paste(unlist(a$reasons), collapse = " ")
    lines <- c(lines, paste0("  ", a$analysis, ": ", a$outcome, if (nzchar(reasons)) paste0(" \u2014 ", reasons)))
  }
  for (e in x$entities) {
    if (!identical(e$status, "resolved")) lines <- c(lines, paste0("  unresolved: ", e$name, " (", e$status, ")"))
  }
  if (length(x$assumptions)) {
    lines <- c(lines, paste0("  ", length(x$assumptions), " model notes: see review$assumptions"))
  }
  cat(lines, sep = "\n")
  invisible(x)
}

#' @rdname print.verdict_trial_review
#' @export
as.data.frame.verdict_trial_review <- function(x, ...) {
  data.frame(
    analysis = vapply(x$availability, function(a) a$analysis, character(1)),
    outcome = vapply(x$availability, function(a) if (is.null(a$outcome)) NA_character_ else a$outcome, character(1)),
    reasons = vapply(x$availability, function(a) paste(unlist(a$reasons), collapse = " "), character(1)),
    stringsAsFactors = FALSE)
}

# ---- Sets of variants ----------------------------------------------------------------

.verdict_design_key_pattern <- "^[A-Za-z0-9][A-Za-z0-9_.:-]{0,199}$"

# A valid, stable idempotency key for one variant (letters, digits, _.:-).
.trial_variant_key <- function(prefix, label) {
  slug <- gsub("^-+|-+$", "", gsub("[^A-Za-z0-9_.:-]+", "-", label))
  key <- paste0(prefix, ":", slug)
  if (nchar(key) <= 180L) return(key)
  path <- tempfile()
  on.exit(unlink(path))
  writeBin(charToRaw(enc2utf8(label)), path)
  paste0(substr(prefix, 1L, 120L), ":", unname(tools::md5sum(path)))
}

#' Run trial variations together
#'
#' A \code{TrialSet} holds named \code{\link{Trial}} variants, usually made with
#' \code{trial$vary(...)}. \code{$review(client)} reviews every variant without
#' running anything and returns a data frame of readiness. \code{$run(client,
#' idempotency_prefix)} submits every variant first and then waits for all of
#' them; each variant's key is \code{"<prefix>:<label>"}, so rerunning with the
#' same prefix recovers the same runs. It returns a
#' \code{verdict_trial_set_result} with \code{results} (one trial result per
#' variant) and \code{table} (one row per variant: status and headline
#' estimates).
#'
#' Other methods: \code{TrialSet$new()}, \code{$add(label, trial)},
#' \code{$get(label)}, \code{$labels()}, \code{$count()}, \code{$list()}.
#'
#' @format An \code{R6::R6Class} generator.
#' @seealso \code{\link{Trial}}
#' @export
TrialSet <- R6::R6Class("TrialSet",
  cloneable = FALSE,
  public = list(
    add = function(label, trial) {
      if (!is.character(label) || length(label) != 1L || !nzchar(label)) .trial_error("label must be a nonblank string")
      if (!inherits(trial, "Trial")) .trial_error("trial must be a Trial")
      if (label %in% names(private$trials)) .trial_error("A variant named '", label, "' already exists")
      private$trials[[label]] <- trial
      invisible(self)
    },
    get = function(label) {
      if (!label %in% names(private$trials)) {
        .trial_error("No variant '", label, "'; have: ", paste(names(private$trials), collapse = ", "))
      }
      private$trials[[label]]
    },
    labels = function() names(private$trials),
    count = function() length(private$trials),
    list = function() private$trials,
    review = function(client) {
      rows <- lapply(names(private$trials), function(label) {
        review <- private$trials[[label]]$review(client)
        list(variant = label, ready = review$ok,
          will_run = paste(review$available, collapse = ", "),
          skipped = paste(review$unavailable, collapse = ", "),
          issues = paste(vapply(review$issues, function(i) if (is.null(i$message)) "" else i$message, ""), collapse = "; "))
      })
      .verdict_rows_frame(rows, c("variant", "ready", "will_run", "skipped", "issues"))
    },
    run = function(client, idempotency_prefix, timeout = 3600, poll_interval = 5) {
      if (missing(idempotency_prefix) || !is.character(idempotency_prefix) || length(idempotency_prefix) != 1L ||
          !grepl(.verdict_design_key_pattern, idempotency_prefix)) {
        stop("idempotency_prefix must start with a letter or digit and use only letters, digits, '_', '.', ':' or '-'",
          call. = FALSE)
      }
      receipts <- lapply(names(private$trials), function(label) {
        private$trials[[label]]$run(client, idempotency_key = .trial_variant_key(idempotency_prefix, label), wait = FALSE)
      })
      results <- lapply(receipts, function(receipt) {
        client$wait_for_trial(receipt$id, timeout = timeout, poll_interval = poll_interval)
      })
      names(results) <- names(private$trials)
      rows <- lapply(names(results), function(label) c(list(variant = label), results[[label]]$headlines()))
      structure(list(results = results, table = .verdict_rows_frame(rows)), class = "verdict_trial_set_result")
    },
    print = function(...) {
      if (!length(private$trials)) cat("No variants\n")
      for (label in names(private$trials)) {
        trial <- private$trials[[label]]
        cat(label, ": ", trial$population$summary, " \u00b7 ", trial$details$phase, "\n", sep = "")
      }
      invisible(self)
    }
  ),
  private = list(trials = list())
)

#' @export
length.TrialSet <- function(x) x$count()

#' @export
print.verdict_trial_set_result <- function(x, ...) {
  print(x$table, row.names = FALSE)
  invisible(x)
}

#' @export
as.data.frame.verdict_trial_set_result <- function(x, ...) x$table

# Rows (named lists, possibly with different keys) to a data frame; NULL becomes NA.
.verdict_rows_frame <- function(rows, columns = NULL) {
  if (is.null(columns)) columns <- unique(unlist(lapply(rows, names)))
  if (!length(rows)) {
    return(stats::setNames(as.data.frame(rep(list(character()), length(columns)), col.names = paste0("V", seq_along(columns))), columns))
  }
  out <- lapply(columns, function(column) {
    unlist(lapply(rows, function(row) {
      value <- row[[column]]
      if (is.null(value) || !length(value)) NA else value
    }))
  })
  stats::setNames(as.data.frame(out, stringsAsFactors = FALSE, col.names = paste0("V", seq_along(columns))), columns)
}
