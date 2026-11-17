### R/faire_column_suggestions.R
#
# Step 3: FAIRe columns that are filled in for the published samples but
# that MAT doesn't put in the archive yet, each with a suggested Darwin Core
# / GBIF term name and the archive file it would go in. Nothing is added
# unless the user ticks it (add_faire_columns()) - a wrong guess would
# publish data under the wrong term.
#
# Suggestions, in order of confidence:
#   1. same name as a term                    ("Same name")
#   2. reference/faire_dwc_crosswalk.csv      ("Known FAIRe match")
#   3. same letters, other case / underscores ("Same name, different spelling")
#   4. a close name (<= 2 letters different)  ("Close name - check carefully")
# Term lists: reference/gbif_terms_*.csv (refreshed every 3 months by
# scripts/update_reference_terms.R).
#
# Only sampleMetadata (one value per sample) and experimentRunMetadata (one
# value per sample per assay) are scanned. taxaFinal is left out on purpose:
# names and ranks are decided in the WoRMS step, and copying e.g.
# specificEpithet back in would undo a name deliberately published at genus level.

FAIRE_SCAN_SHEETS <- c("sampleMetadata", "experimentRunMetadata")

# FAIRe / INSDC "missing value" wording - these cells count as empty.
FAIRE_MISSING_PATTERN <- "^(na|n/a|null|none|not applicable|missing|not collected|not provided|not recorded|restricted access)(\\b|:|$)"

# Which archive file a term goes in, tried in this order, per FAIRe sheet.
FAIRE_TARGET_ORDER <- list(
  sampleMetadata        = c("event", "dna_derived_data", "occurrence"),
  experimentRunMetadata = c("dna_derived_data", "occurrence")
)
FAIRE_TARGET_LABEL <- c(event = "Event core", occurrence = "Occurrence", dna_derived_data = "DNA Derived Data",
                        emof = "eMoF (measurement per sample)")

# sampleMetadata columns with no Darwin Core / GBIF term can go in the eMoF
# file instead, one row per sample: measurementType = a readable name
# (reference/faire_emof_terms.csv, else the FAIRe name), measurementTypeID =
# the term's link in the FAIRe checklist (often a MIxS term), unit from the
# same files. experimentRunMetadata columns are per sample AND assay, which
# an eMoF row per sample can't hold, so they are listed but not offered.
.faire_checklist <- function() {
  p <- file.path("reference", "faire_checklist.csv")
  if (!file.exists(p)) return(NULL)
  utils::read.csv(p, stringsAsFactors = FALSE, na.strings = c("", "NA"))
}
.faire_emof_terms <- function() {
  p <- file.path("reference", "faire_emof_terms.csv")
  if (!file.exists(p)) return(NULL)
  utils::read.csv(p, stringsAsFactors = FALSE, na.strings = c("", "NA"))
}
faire_emof_spec <- function(col, checklist = .faire_checklist(), emof_terms = .faire_emof_terms()) {
  e  <- if (!is.null(emof_terms)) emof_terms[emof_terms$faire_column == col, , drop = FALSE] else NULL
  ck <- if (!is.null(checklist)) checklist[checklist$term_name == col, , drop = FALSE] else NULL
  pick <- function(x) if (length(x) && !is.na(x[1]) && nzchar(x[1])) x[1] else NA_character_
  list(
    type      = if (!is.null(e) && nrow(e)) pick(e$measurementType) else col,
    type_id   = if (!is.null(ck) && nrow(ck)) pick(ck$URI) else NA_character_,
    unit      = if (!is.null(e) && nrow(e)) pick(e$measurementUnit) else if (!is.null(ck) && nrow(ck)) pick(ck$unit) else NA_character_,
    unit_id   = if (!is.null(e) && nrow(e)) pick(e$measurementUnitID) else NA_character_,
    value_map = if (!is.null(e) && nrow(e)) pick(e$value_map) else NA_character_
  )
}
# "1=active;0=passive" -> named vector, applied to values
apply_value_map <- function(x, map) {
  if (is.na(map)) return(x)
  pairs <- strsplit(strsplit(map, ";", fixed = TRUE)[[1]], "=", fixed = TRUE)
  m <- setNames(vapply(pairs, `[`, "", 2), vapply(pairs, `[`, "", 1))
  ifelse(!is.na(x) & x %in% names(m), unname(m[x]), x)
}

faire_cell_filled <- function(x) {
  x <- trimws(as.character(x))
  !is.na(x) & nzchar(x) & !grepl(FAIRE_MISSING_PATTERN, tolower(x))
}

# FAIRe columns the build_* code already reads (found in the code itself, so
# this stays right when the code changes). Comment lines are ignored.
faire_columns_used_by_mat <- local({
  cache <- NULL
  function() {
    if (is.null(cache)) {
      files <- list.files("R", pattern = "^build_.*[.]R$", full.names = TRUE)
      code <- unlist(lapply(files, readLines, warn = FALSE))
      cache <<- paste(code[!grepl("^\\s*#", code)], collapse = "\n")
    }
    cache
  }
})
faire_column_is_used <- function(col) {
  # \Q...\E = the column name taken literally, as a whole word.
  grepl(paste0("(?<![A-Za-z0-9_.])\\Q", col, "\\E(?![A-Za-z0-9_])"), faire_columns_used_by_mat(), perl = TRUE)
}

.faire_terms <- function() {
  tbls <- c("event", "occurrence", "dna_derived_data")
  do.call(rbind, lapply(tbls, function(t) {
    d <- utils::read.csv(file.path("reference", paste0("gbif_terms_", t, ".csv")), stringsAsFactors = FALSE)
    data.frame(table = t, term = d$term, stringsAsFactors = FALSE)
  }))
}
.faire_crosswalk <- function() {
  p <- file.path("reference", "faire_dwc_crosswalk.csv")
  if (!file.exists(p)) return(data.frame(faire_column = character(), term = character(), note = character()))
  utils::read.csv(p, stringsAsFactors = FALSE)
}

# Link to a term's official definition: the Darwin Core page for Event /
# Occurrence terms, the GBIF extension file (the version in
# reference/reference_sources.csv) for DNA Derived Data terms.
term_definition_url <- function(term, table) {
  if (identical(table, "dna_derived_data")) {
    src <- tryCatch(utils::read.csv(file.path("reference", "reference_sources.csv"), stringsAsFactors = FALSE), error = function(e) NULL)
    url <- if (!is.null(src)) src$url[src$file == "gbif_terms_dna_derived_data.csv"] else character()
    if (length(url) == 0) url <- "https://rs.gbif.org/extension/gbif/1.0/dna_derived_data_2024-07-11.xml"
    return(paste0(url[1], "#", term))
  }
  paste0("https://dwc.tdwg.org/terms/#dwc:", term)
}

#' Suggest a term for one FAIRe column.
#' @return list(term, table, how) or NULL when nothing fits.
suggest_faire_term <- function(col, sheet, terms = .faire_terms(), crosswalk = .faire_crosswalk()) {
  order <- FAIRE_TARGET_ORDER[[sheet]]
  pick <- function(cands, how) {
    cands <- cands[cands$table %in% order, , drop = FALSE]
    if (nrow(cands) == 0) return(NULL)
    cands <- cands[order(match(cands$table, order)), , drop = FALSE]
    list(term = cands$term[1], table = cands$table[1], how = how)
  }
  norm <- function(x) gsub("[^a-z0-9]", "", tolower(x))

  hit <- pick(terms[terms$term == col, ], "Same name")
  if (!is.null(hit)) return(hit)

  cw <- crosswalk$term[crosswalk$faire_column == col]
  if (length(cw)) {
    hit <- pick(terms[terms$term %in% cw, ], "Known FAIRe match")
    if (!is.null(hit)) return(hit)
  }

  hit <- pick(terms[norm(terms$term) == norm(col), ], "Same name, different spelling")
  if (!is.null(hit)) return(hit)

  if (nchar(norm(col)) >= 6) {
    d <- utils::adist(norm(col), norm(terms$term))[1, ]
    close <- terms[d <= 2 & nchar(norm(terms$term)) >= 6, , drop = FALSE]
    if (nrow(close)) {
      close <- close[order(d[d <= 2 & nchar(norm(terms$term)) >= 6]), , drop = FALSE]
      return(pick(close, "Close name - check carefully"))
    }
  }
  NULL
}

#' Scan the uploaded FAIRe files.
#'
#' @param input_files named list assay -> .xlsx path
#' @param build_result the Step 3 build (event_core, occurrence, dna_extension)
#' @return data.frame(sheet, column, filled, example, term, table, how) - one
#'   row per filled-in column MAT doesn't use; term/table/how are NA when no
#'   term fits. Columns whose suggested term is already in that file are left out.
scan_faire_columns <- function(input_files, build_result) {
  kept <- build_result$event_core$eventID
  terms <- .faire_terms()
  crosswalk <- .faire_crosswalk()
  checklist <- .faire_checklist()
  emof_terms <- .faire_emof_terms()
  existing <- list(
    event            = names(build_result$event_core),
    occurrence       = unique(unlist(lapply(build_result$occurrence, names))),
    dna_derived_data = unique(unlist(lapply(build_result$dna_extension, names))),
    emof             = unique(build_result$emof$measurementType)
  )

  rows <- list()
  for (sheet in FAIRE_SCAN_SHEETS) {
    # sampleMetadata is the same in every file; experimentRunMetadata is per assay.
    files <- if (sheet == "sampleMetadata") input_files[1] else input_files
    seen <- list()
    for (f in files) {
      d <- tryCatch(read_faire_sheet(f, sheet), error = function(e) NULL)
      if (is.null(d) || !"samp_name" %in% names(d)) next
      d <- d[d$samp_name %in% kept, , drop = FALSE]
      for (col in setdiff(names(d), c("samp_name", ""))) {
        if (is.na(col) || grepl("^\\.\\.\\.[0-9]+$", col)) next
        filled <- faire_cell_filled(d[[col]])
        seen[[col]] <- c(seen[[col]], d[[col]][filled])
      }
    }
    for (col in names(seen)) {
      vals <- seen[[col]]
      if (length(vals) == 0 || faire_column_is_used(col)) next
      s <- suggest_faire_term(col, sheet, terms, crosswalk)
      if (is.null(s) && sheet == "sampleMetadata") {
        spec <- faire_emof_spec(col, checklist, emof_terms)
        s <- list(term = spec$type, table = "emof", how = "eMoF measurement")
      }
      if (!is.null(s) && s$term %in% existing[[s$table]]) next
      rows[[length(rows) + 1]] <- data.frame(
        sheet = sheet, column = col, filled = length(vals),
        example = as.character(names(sort(table(as.character(vals)), decreasing = TRUE))[1]),
        term = if (is.null(s)) NA_character_ else s$term,
        table = if (is.null(s)) NA_character_ else s$table,
        how = if (is.null(s)) NA_character_ else s$how,
        link = if (is.null(s)) NA_character_
               else if (s$table == "emof") faire_emof_spec(col, checklist, emof_terms)$type_id
               else term_definition_url(s$term, s$table),
        stringsAsFactors = FALSE)
    }
  }
  if (length(rows) == 0) {
    return(data.frame(sheet = character(), column = character(), filled = integer(), example = character(),
                      term = character(), table = character(), how = character(), link = character(),
                      stringsAsFactors = FALSE))
  }
  out <- do.call(rbind, rows)
  out[order(is.na(out$term), out$sheet, -out$filled), , drop = FALSE]
}

#' Add the ticked FAIRe columns to the built tables, under their suggested term.
#'
#' @param picks rows of scan_faire_columns() to add
#' @return build_result with the new columns (missing-value wording -> empty)
add_faire_columns <- function(build_result, input_files, picks) {
  if (is.null(picks) || nrow(picks) == 0) return(build_result)
  clean <- function(x) { x <- trimws(as.character(x)); x[!faire_cell_filled(x)] <- NA_character_; x }
  # One value per eventID (collapsing repeats with " | ", as build_dna_extension does).
  lookup <- function(d, col) {
    v <- tapply(clean(d[[col]]), d$samp_name, function(x) {
      x <- unique(x[!is.na(x)]); if (length(x)) paste(x, collapse = " | ") else NA_character_
    })
    v
  }
  first <- read_faire_sheet(input_files[[1]], "sampleMetadata")
  checklist <- .faire_checklist()
  term_types <- tryCatch(do.call(rbind, lapply(c("event", "occurrence", "dna_derived_data"), function(t) {
    d <- utils::read.csv(file.path("reference", paste0("gbif_terms_", t, ".csv")), stringsAsFactors = FALSE); cbind(table = t, d)
  })), error = function(e) NULL)
  # A text term with no unit field of its own (e.g. size_frac "0.45") gets
  # the FAIRe unit written into the value ("0.45 µm"), so it isn't ambiguous.
  with_unit <- function(v, col, table, term) {
    unit <- if (!is.null(checklist)) checklist$unit[checklist$term_name == col] else character()
    type <- if (!is.null(term_types)) term_types$type[term_types$table == table & term_types$term == term] else character()
    if (length(unit) && !is.na(unit[1]) && nzchar(unit[1]) && identical(type[1], "string")) {
      ifelse(is.na(v) | grepl("[A-Za-zµ]", v), v, paste(v, unit[1]))
    } else v
  }

  for (i in seq_len(nrow(picks))) {
    p <- picks[i, ]
    if (p$table == "emof") {
      v <- lookup(first, p$column)
      spec <- faire_emof_spec(p$column, checklist)
      ids <- build_result$event_core$eventID
      vals <- apply_value_map(unname(v[ids]), spec$value_map)
      keep <- !is.na(vals)
      new_rows <- data.frame(
        eventID = ids[keep], occurrenceID = NA_character_,
        measurementType = spec$type, measurementTypeID = spec$type_id,
        measurementValue = vals[keep], measurementValueID = NA_character_,
        measurementUnit = spec$unit, measurementUnitID = spec$unit_id,
        stringsAsFactors = FALSE)
      old <- build_result$emof
      emof <- if (is.null(old)) new_rows else dplyr::bind_rows(old, new_rows)
      # Keep each sample's rows together, in Event core order (stable sort).
      emof <- emof[order(match(emof$eventID, ids)), , drop = FALSE]
      rownames(emof) <- NULL
      build_result$emof <- emof
      next
    }
    if (p$sheet == "sampleMetadata") {
      v <- with_unit(lookup(first, p$column), p$column, p$table, p$term)
      if (p$table == "event") {
        build_result$event_core[[p$term]] <- unname(v[build_result$event_core$eventID])
      } else {
        slot <- if (p$table == "occurrence") "occurrence" else "dna_extension"
        for (a in names(build_result[[slot]])) {
          build_result[[slot]][[a]][[p$term]] <- unname(v[build_result[[slot]][[a]]$eventID])
        }
      }
    } else {
      slot <- if (p$table == "occurrence") "occurrence" else "dna_extension"
      for (a in intersect(names(build_result[[slot]]), names(input_files))) {
        d <- read_faire_sheet(input_files[[a]], "experimentRunMetadata")
        if (!p$column %in% names(d)) next
        v <- with_unit(lookup(d, p$column), p$column, p$table, p$term)
        build_result[[slot]][[a]][[p$term]] <- unname(v[build_result[[slot]][[a]]$eventID])
      }
    }
  }
  build_result
}
