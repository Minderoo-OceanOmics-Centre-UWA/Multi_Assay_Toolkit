### R/check_dwc_mapping.R
#
# Shows the user, before they upload to the IPT, what the IPT's mapping
# screen would complain about: columns that aren't a term of the target
# Darwin Core / GBIF definition, and values that don't match a term's
# data type (e.g. a range like "178-228" in an integer field).
#
# The term lists in reference/ are copies of the official GBIF definitions
# (Event and Occurrence core 2025-07-10, DNA Derived Data extension
# 2024-07-11 from rs.gbif.org) so this works offline on the deployed app.
# Refresh them if GBIF publishes newer versions.

# Columns that aren't a term of the table's definition but are intentional.
DWC_EXPECTED_NON_TERMS <- list(
  occurrence       = c(seq_id = "ASV id - used to build occurrenceID and to look up the DNA sequence; the IPT will list it as unmapped, which is harmless."),
  dna_derived_data = c(eventID = "Link back to the Event core (the extension's core id) - not a term of the extension itself."),
  emof             = c(eventID = "Link back to the Event core (the extension's core id) - not a term of the extension itself.")
)

.load_terms <- function(name) {
  utils::read.csv(file.path("reference", paste0("gbif_terms_", name, ".csv")), stringsAsFactors = FALSE)
}

.check_one_table <- function(label, df, terms_name) {
  terms <- .load_terms(terms_name)
  expected <- DWC_EXPECTED_NON_TERMS[[terms_name]]

  rows <- lapply(names(df), function(col) {
    term_type <- terms$type[match(col, terms$term)]

    if (is.na(term_type)) {
      if (col %in% names(expected)) {
        return(data.frame(table = label, column = col, status = "expected", detail = unname(expected[col])))
      }
      return(data.frame(table = label, column = col, status = "not_a_term",
                        detail = "Not a term in this table's Darwin Core definition - the IPT will show it as unmapped. Rename it to the matching term, or remove it."))
    }

    if (term_type %in% c("integer", "decimal")) {
      vals <- unique(stats::na.omit(as.character(df[[col]])))
      vals <- vals[nzchar(vals)]
      pattern_ok <- if (term_type == "integer") grepl("^[+-]?[0-9]+$", vals) else !is.na(suppressWarnings(as.numeric(vals)))
      if (col == "ampliconSize" && any(!pattern_ok) && all(pattern_ok | grepl("^[0-9]+ \\| [0-9]+$", vals))) {
        return(data.frame(table = label, column = col, status = "expected",
                          detail = paste0("Range written as \"min | max\" (e.g. \"", vals[!pattern_ok][1],
                                          "\"), following NOAA Omics' metabarcoding-assay guidance. GBIF types this term as integer, so the IPT may still flag it.")))
      }
      if (any(!pattern_ok)) {
        return(data.frame(table = label, column = col, status = "wrong_type",
                          detail = paste0("Must be ", term_type, " but found e.g. \"", vals[!pattern_ok][1], "\"")))
      }
    }
    data.frame(table = label, column = col, status = "ok", detail = "")
  })
  do.call(rbind, rows)
}

#' @return data.frame(table, column, status, detail); status is one of
#'   "ok", "expected", "not_a_term", "wrong_type"
check_dwc_mapping <- function(event, occurrence_tables, dna_tables, emof = NULL) {
  parts <- list(.check_one_table("Event core", event, "event"))
  for (a in names(occurrence_tables)) {
    parts[[length(parts) + 1]] <- .check_one_table(paste0("Occurrence - ", a), occurrence_tables[[a]], "occurrence")
  }
  for (a in names(dna_tables)) {
    parts[[length(parts) + 1]] <- .check_one_table(paste0("DNA Derived Data - ", a), dna_tables[[a]], "dna_derived_data")
  }
  if (!is.null(emof)) {
    parts[[length(parts) + 1]] <- .check_one_table("Extended MeasurementOrFact", emof, "emof")
  }
  do.call(rbind, parts)
}
