### R/qc_checks.R
#
# Refactored from scripts/05_qc_checks.R (now a thin CLI wrapper around
# run_qc_checks() below). Pure function - see that script for the full
# rationale behind each check, including the two confirmed obistools
# bugs/quirks worked around here (check_fields' `level` argument, and
# check_depth's crash-on-clean-pass).

#' Run the full QC battery against a finished archive.
#'
#' @param event Event core tibble
#' @param occurrence_tables Named list of per-assay Occurrence tibbles (post-WoRMS-matching)
#' @param dna_tables Named list of per-assay DNA Derived Data tibbles
#' @return list(results = named list of check outputs, any_failures = logical,
#'   any_skipped = character vector, messages = character vector)
run_qc_checks <- function(event, occurrence_tables, dna_tables) {
  messages <- character()
  log_msg <- function(...) messages <<- c(messages, paste0(...))

  any_failures <- FALSE
  any_warnings <- FALSE
  any_skipped  <- character()
  results <- list()

  # 1/2. Required fields, via flatten_occurrence() (see script 05 for
  # why check_fields() needs a combined Event+Occurrence view).
  # An empty scientificNameID is a WARNING, not a failure: names not yet in
  # WoRMS are published without one (OBIS guidance), and the reviewer can
  # leave an ID choice to OBIS in Step 4. Those records are still publishable.
  for (n in names(occurrence_tables)) {
    joined <- obistools::flatten_occurrence(event, occurrence_tables[[n]])
    check <- obistools::check_fields(joined, level = "warning")
    id_rows <- if (nrow(check) > 0 && "field" %in% names(check)) check$field == "scientificNameID" else logical(nrow(check))
    results[[paste0("check_fields_", n)]] <- check[!id_rows, , drop = FALSE]
    results[[paste0("scientificnameid_empty_", n)]] <- check[id_rows, , drop = FALSE]
    if (any(!id_rows)) any_failures <- TRUE
    if (any(id_rows)) any_warnings <- TRUE
  }

  # 2b. eventID uniqueness + parentEventID validity within the Event core.
  eventid_check <- obistools::check_eventids(event)
  results$check_eventids <- eventid_check
  if (nrow(eventid_check) > 0) any_failures <- TRUE

  # 3. eventDate formatting.
  date_check <- obistools::check_eventdate(event)
  results$check_eventdate <- date_check
  if (nrow(date_check) > 0) any_failures <- TRUE

  # 3b. Depth vs. bathymetry - wrapped for the known obistools::check_depth() bug.
  depth_check <- tryCatch(
    obistools::check_depth(event),
    error = function(e) {
      log_msg("SKIPPED check_depth - known obistools bug: ", conditionMessage(e))
      NULL
    }
  )
  if (!is.null(depth_check)) {
    results$check_depth <- depth_check
    if (nrow(depth_check) > 0) any_failures <- TRUE
  } else {
    any_skipped <- c(any_skipped, "check_depth (depth vs. bathymetry) - NOT verified")
  }

  # 4. Coordinates on land / out of range.
  onland_check <- obistools::check_onland(event)
  results$check_onland <- onland_check
  if (nrow(onland_check) > 0) any_failures <- TRUE

  coord_out_of_range <- event %>%
    dplyr::filter(
      is.na(decimalLatitude) | is.na(decimalLongitude) |
      decimalLatitude < -90 | decimalLatitude > 90 |
      decimalLongitude < -180 | decimalLongitude > 180 |
      (decimalLatitude == 0 & decimalLongitude == 0)
    )
  results$coord_out_of_range <- coord_out_of_range
  if (nrow(coord_out_of_range) > 0) any_failures <- TRUE

  # 5. eventID consistency between Event core and every extension
  # (Occurrence AND DNA Derived Data - see script 05 for why both matter).
  for (n in names(occurrence_tables)) {
    ext_check <- obistools::check_extension_eventids(event, occurrence_tables[[n]])
    results[[paste0("check_extension_eventids_occurrence_", n)]] <- ext_check
    if (length(ext_check) > 0 && !all(is.na(ext_check))) any_failures <- TRUE
  }

  for (n in names(dna_tables)) {
    if (!"eventID" %in% names(dna_tables[[n]])) {
      any_failures <- TRUE
      log_msg("ERROR: no eventID column in DNA extension '", n, "' - cannot link to Event core in IPT.")
      next
    }
    ext_check <- obistools::check_extension_eventids(event, dna_tables[[n]])
    results[[paste0("check_extension_eventids_dna_", n)]] <- ext_check
    if (length(ext_check) > 0 && !all(is.na(ext_check))) any_failures <- TRUE
  }

  # 6. occurrenceID <-> eventID consistency between Occurrence and DNA
  # Derived Data extension (not a built-in obistools check).
  occ_dna_crosscheck <- list()
  for (assay in intersect(names(occurrence_tables), names(dna_tables))) {
    occ_ids <- occurrence_tables[[assay]]$occurrenceID
    dna_ids <- dna_tables[[assay]]$occurrenceID
    missing_in_dna <- setdiff(occ_ids, dna_ids)
    missing_in_occ <- setdiff(dna_ids, occ_ids)
    occ_dna_crosscheck[[assay]] <- list(
      occurrence_rows = length(occ_ids),
      dna_rows        = length(dna_ids),
      missing_in_dna  = missing_in_dna,
      missing_in_occ  = missing_in_occ
    )
    if (length(missing_in_dna) > 0 || length(missing_in_occ) > 0) any_failures <- TRUE
  }
  results$occ_dna_crosscheck <- occ_dna_crosscheck

  # 7b. occurrenceID must be unique across the WHOLE archive, not just
  # within one assay's own file - added after a real production bug
  # where two different assays' Occurrence tables both happened to
  # contain occurrenceID "eventID_ASV_2774" (ASV numbers are local to
  # each assay's own pipeline run, not global), which every check
  # above missed since none of them looked ACROSS assays. See
  # CLAUDE.md's Identifiers section.
  all_occurrence_ids <- unlist(lapply(occurrence_tables, function(df) df$occurrenceID), use.names = FALSE)
  dup_occurrence_ids <- unique(all_occurrence_ids[duplicated(all_occurrence_ids)])
  results$duplicate_occurrence_ids <- dup_occurrence_ids
  if (length(dup_occurrence_ids) > 0) {
    any_failures <- TRUE
    log_msg("ERROR: ", length(dup_occurrence_ids), " occurrenceID(s) duplicated across assays - not unique archive-wide.")
  }

  # 7. scientificNameID completeness (post-WoRMS-matching check). A warning,
  # not a failure - see the note at check 1/2.
  scientificname_coverage <- list()
  for (n in names(occurrence_tables)) {
    occ <- occurrence_tables[[n]]
    n_missing <- sum(is.na(occ$scientificNameID) | occ$scientificNameID == "")
    scientificname_coverage[[n]] <- list(total = nrow(occ), missing = n_missing)
    if (n_missing > 0) any_warnings <- TRUE
  }
  results$scientificname_coverage <- scientificname_coverage

  list(results = results, any_failures = any_failures, any_warnings = any_warnings,
       any_skipped = any_skipped, messages = messages)
}
