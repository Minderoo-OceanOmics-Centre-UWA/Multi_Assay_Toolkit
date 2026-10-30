### R/validate_faire_files.R
#
# Structural + content validation for uploaded FAIRe files, built for
# the Shiny app's "Validate & Fix" step. Unlike build_event_core()
# (which stop()s hard on a sampleMetadata mismatch - correct for a CLI
# pipeline meant to fail loudly), this returns a structured list of
# issues so a UI can render each one with the right fix: a blocking
# error to take back to the source file, a question to ask the user
# for a project-level config value that has no source in the data at
# all (e.g. what GPS system was used), or a table of specific rows
# missing a required field for the user to fill in directly.
#
# Each issue is a list:
#   id       - stable string id (for tracking fixes across re-validation)
#   severity - "error" (blocks proceeding) | "warning" (should fix, not blocking)
#   title    - short label
#   message  - explanation of what's wrong and why it matters
#   fix_type - "config_question" | "data_table" | "info_only"
#   field    - (config_question only) which config value this answers
#   data     - (data_table only) tibble of the affected rows
#   columns  - (data_table only) character vector of the editable column(s)

REQUIRED_SHEETS <- c("projectMetadata", "sampleMetadata", "experimentRunMetadata", "taxaFinal", "otuFinal")
REQUIRED_SAMPLE_COLUMNS <- c("samp_name", "samp_category", "eventDate", "decimalLatitude", "decimalLongitude")

#' @param input_files Named list: assay name -> path to that assay's FAIRe .xlsx
#' @param sample_category_keep The samp_category value marking a real (non-control) sample
#' @param georeference_sources,associated_sequences_uri No longer checked here (optional
#'   Step 1 fields now); kept so existing callers don't break
#' @param reference_db "curated" or "nt" - the type of FAIRe file this publication uses
#' @param input_filenames Named list: assay name -> the file's ORIGINAL name (used to
#'   check every file matches reference_db; skipped if either is NULL)
#' @param allow_uneven_samples TRUE once the user has confirmed that samples
#'   missing from some files really weren't analysed with that assay
#' @return list(issues = list of issue objects, any_blocking = logical)
validate_faire_files <- function(input_files,
                                  sample_category_keep,
                                  georeference_sources = NA_character_,
                                  associated_sequences_uri = NA_character_,
                                  reference_db = NULL,
                                  input_filenames = NULL,
                                  allow_uneven_samples = FALSE) {
  issues <- list()
  add_issue <- function(issue) issues[[length(issues) + 1]] <<- issue

  # ---- 0. Every file must be the same type (curated vs nt) ---------------
  if (!is.null(reference_db) && !is.null(input_filenames)) {
    chosen_label <- names(REFERENCE_DB_CHOICES)[REFERENCE_DB_CHOICES == reference_db]
    detected <- vapply(names(input_files), function(a) detect_reference_db_from_filename(input_filenames[[a]]), character(1))

    wrong <- names(detected)[!is.na(detected) & detected != reference_db]
    if (length(wrong) > 0) {
      add_issue(list(
        id = "reference_db_mismatch",
        severity = "error",
        title = "File type doesn't match the selected reference database",
        message = paste0(
          "You selected \"", chosen_label, "\", but these files look like the other type: ",
          paste0(wrong, " (", vapply(wrong, function(a) input_filenames[[a]], character(1)), ")", collapse = ", "),
          ". A publication must use only curated-database files or only NCBI nt files, not a mix. ",
          "Re-upload matching files, or change the selection in Step 1."
        ),
        fix_type = "blocking_error"
      ))
    }

    unknown <- names(detected)[is.na(detected)]
    if (length(unknown) > 0) {
      add_issue(list(
        id = "reference_db_unverified",
        severity = "warning",
        title = "Couldn't confirm the file type from the filename",
        message = paste0(
          "The filename doesn't say curated or nt for: ",
          paste0(unknown, " (", vapply(unknown, function(a) input_filenames[[a]], character(1)), ")", collapse = ", "),
          ". Please check yourself that these are all \"", chosen_label, "\" files."
        ),
        fix_type = "info_only"
      ))
    }
  }

  # ---- 1. Required sheets present in every uploaded file ----------------
  sheet_lists <- list()
  for (assay in names(input_files)) {
    sheets <- tryCatch(readxl::excel_sheets(input_files[[assay]]), error = function(e) character())
    sheet_lists[[assay]] <- sheets
    missing_sheets <- setdiff(REQUIRED_SHEETS, sheets)
    if (length(missing_sheets) > 0) {
      add_issue(list(
        id = paste0("missing_sheets_", assay),
        severity = "error",
        title = paste0(assay, ": missing required sheet(s)"),
        message = paste0(
          "This file is missing: ", paste(missing_sheets, collapse = ", "),
          ". A FAIRe workbook needs all of: ", paste(REQUIRED_SHEETS, collapse = ", "),
          ". Re-upload a complete FAIRe file for this assay."
        ),
        fix_type = "blocking_error"
      ))
    }
  }
  if (any(vapply(issues, function(i) i$severity == "error", logical(1)))) {
    # Can't safely read further sheets if a required one is missing.
    return(list(issues = issues, any_blocking = TRUE))
  }

  # ---- 2. Required sampleMetadata columns present ------------------------
  sample_tables <- list()
  for (assay in names(input_files)) {
    sm <- tryCatch(read_faire_sheet(input_files[[assay]], "sampleMetadata"), error = function(e) NULL)
    sample_tables[[assay]] <- sm
    if (is.null(sm)) {
      add_issue(list(
        id = paste0("unreadable_sampleMetadata_", assay),
        severity = "error",
        title = paste0(assay, ": sampleMetadata could not be read"),
        message = "Check the file isn't corrupted and the 3-row header (requirement_level_code / section / column names) is intact.",
        fix_type = "blocking_error"
      ))
      next
    }
    missing_cols <- setdiff(REQUIRED_SAMPLE_COLUMNS, names(sm))
    if (length(missing_cols) > 0) {
      add_issue(list(
        id = paste0("missing_columns_", assay),
        severity = "error",
        title = paste0(assay, ": sampleMetadata missing required column(s)"),
        message = paste0("Missing: ", paste(missing_cols, collapse = ", "), ". These are required to build the Event core."),
        fix_type = "blocking_error"
      ))
    }
  }
  if (any(vapply(issues, function(i) i$severity == "error", logical(1)))) {
    return(list(issues = issues, any_blocking = TRUE))
  }

  # ---- 3. Cross-assay sampleMetadata consistency -------------------------
  # (soft version of build_event_core()'s stop() - surfaced as an issue,
  # not a hard halt, so the user sees it in context with everything else)
  assay_names <- names(input_files)

  # 3a. Same sample list in every file? A sample missing from one file is
  # usually a mistake (typo, wrong export), so it blocks - unless the user
  # confirms the samples really weren't analysed with that assay
  # (allow_uneven_samples). Duplicate samp_names always block.
  name_check <- compare_sample_names(lapply(sample_tables, function(t) t$samp_name))
  if (length(assay_names) > 1 && !name_check$same) {
    show_names <- function(x, max_n = 15) {
      if (length(x) <= max_n) paste(x, collapse = ", ")
      else paste0(paste(x[seq_len(max_n)], collapse = ", "), " ... and ", length(x) - max_n, " more")
    }
    pa <- name_check$per_assay
    if (any(pa$n_duplicated > 0)) {
      add_issue(list(
        id = "sample_names_duplicated",
        severity = "error",
        title = "A sample is listed more than once",
        message = paste0(
          paste(unlist(lapply(which(pa$n_duplicated > 0), function(k)
            paste0(pa$assay[k], " lists these samp_name(s) more than once: ", show_names(pa$duplicated[[k]]), "."))), collapse = " "),
          " Each sample must appear once in sampleMetadata. Fix the file, then upload it again."
        ),
        fix_type = "blocking_error"
      ))
    }
    if (any(pa$n_missing > 0)) {
      lines <- unlist(lapply(which(pa$n_missing > 0), function(k)
        paste0(pa$assay[k], " has no row for ", pa$n_missing[k], " sample(s) that other files have: ", show_names(pa$missing[[k]]), ".")))
      add_issue(list(
        id = "sample_names_differ",
        severity = if (isTRUE(allow_uneven_samples)) "warning" else "error",
        title = if (isTRUE(allow_uneven_samples)) "Not every sample was analysed with every assay (confirmed)"
                else "Your files don't list the same samples",
        message = paste0(
          paste(lines, collapse = " "),
          if (isTRUE(allow_uneven_samples))
            " You confirmed these samples weren't analysed with that assay. MAT will publish every sample once, and note on each affected sample which assays were not run, so a missing assay isn't read as \"nothing detected\"."
          else paste0(
            " This is usually a mistake (a typo in samp_name, or an older or wrong file) - fix the sampleMetadata sheets so they match, then upload again.",
            " If these samples really weren't analysed with that assay (for example discarded), tick the box below to publish all assays together.")
        ),
        fix_type = "confirm_uneven_samples"
      ))
    }
  }

  # 3b. Samples that two files share must have identical details.
  if (length(assay_names) > 1 && !any(name_check$per_assay$n_duplicated > 0)) {
    base_assay <- assay_names[1]
    for (other in assay_names[-1]) {
      bad_cols <- shared_metadata_mismatches(sample_tables[[base_assay]], sample_tables[[other]])
      if (length(bad_cols) > 0) {
        add_issue(list(
          id = paste0("column_mismatch_", base_assay, "_", other),
          severity = "error",
          title = paste0("sampleMetadata disagrees: ", base_assay, " vs ", other),
          message = paste0(
            "For the samples both files list, these column(s) differ: ",
            paste(bad_cols, collapse = ", "),
            ". Both assays describe the same physical samples, so this metadata should be identical - resolve the discrepancy in the source files."
          ),
          fix_type = "blocking_error"
        ))
      }
    }
  }

  # ---- 4. Row-level completeness for REAL samples -------------------------
  # Every sample from every file (identical to the first file's table when
  # the sample lists match).
  base_table <- union_sample_table(sample_tables)
  real_samples <- base_table %>% dplyr::filter(samp_category == sample_category_keep)

  incomplete <- real_samples %>%
    dplyr::filter(is.na(eventDate) | is.na(decimalLatitude) | is.na(decimalLongitude) | as.character(decimalLatitude) == "" | as.character(decimalLongitude) == "")

  if (nrow(incomplete) > 0) {
    add_issue(list(
      id = "incomplete_required_fields",
      severity = "error",
      title = paste0(nrow(incomplete), " sample(s) missing eventDate or coordinates"),
      message = "These required fields have no value. Fill them in below before continuing - OBIS QC will drop any record with a missing or zero coordinate.",
      fix_type = "data_table",
      data = incomplete %>% dplyr::select(samp_name, eventDate, decimalLatitude, decimalLongitude),
      columns = c("eventDate", "decimalLatitude", "decimalLongitude")
    ))
  }

  # (GPS system and raw-read archive link used to be asked here as warnings.
  # They are now optional fields in Step 1 - blank simply means "not supplied".)

  any_blocking <- any(vapply(issues, function(i) i$severity == "error", logical(1)))
  list(issues = issues, any_blocking = any_blocking)
}
