### R/eml_excel.R
#
# Excel round-trip for Step 6's metadata form: a template anyone can
# fill in offline (no need to use the web form directly), and a
# reader that feeds an uploaded, filled-in copy back into the app to
# auto-fill every field. Field keys below are deliberately the same
# string used after the "eml_" prefix on each Shiny input id in app.R
# (e.g. key "creator_given" <-> input$eml_creator_given), so the
# upload-and-autofill logic is a single small loop, not 30 special cases.
#
# Field types drive both how a value is written into Excel and which
# Shiny update*Input() function fills it back in:
#   text     - updateTextInput
#   textarea - updateTextAreaInput (value may contain embedded newlines)
#   date     - updateDateInput (expects/produces "YYYY-MM-DD")
#   logical  - updateCheckboxInput (Excel cell: TRUE/FALSE)
#   select   - updateSelectInput (value must be one of a fixed set - see
#              EML_LICENSE_CHOICES in app.R for "license"; PUBLISHER/
#              AUTHOR/PROCESSOR/EDITOR/REVIEWER/OWNER for "ap_role")

EML_FIELD_DEFS <- list(
  list(key = "title",             label = "Dataset title",                     type = "text"),
  list(key = "abstract",          label = "Abstract",                          type = "textarea", notes = "One paragraph per line"),
  list(key = "keywords",          label = "Keywords",                         type = "text",     notes = "Comma-separated"),
  list(key = "license",           label = "License URL",                       type = "select",   notes = "One of the exact CC license URLs the Multi-Assay Toolkit (MAT) offers"),
  list(key = "pub_date",          label = "Publication date",                  type = "date",     notes = "YYYY-MM-DD"),
  list(key = "distribution_url",  label = "Project/organization website",      type = "text",     notes = "Optional"),
  list(key = "org_name",          label = "Organization name",                 type = "text"),
  list(key = "org_address",       label = "Street address",                    type = "text"),
  list(key = "org_city",          label = "City",                              type = "text"),
  list(key = "org_admin_area",    label = "State/region",                      type = "text"),
  list(key = "org_country",       label = "Country code",                      type = "text",     notes = "e.g. AU"),
  list(key = "org_phone",         label = "Phone number",                      type = "text",     notes = "Optional - shared by everyone listed, like the address"),
  list(key = "mp_same",           label = "Metadata provider same as creator?", type = "logical",  notes = "\"Creator\" here means the FIRST row of the separate 'creators' sheet"),
  list(key = "mp_given",          label = "Metadata provider - given name",    type = "text",     notes = "Only if not same as creator"),
  list(key = "mp_sur",            label = "Metadata provider - surname",       type = "text"),
  list(key = "mp_position",       label = "Metadata provider - position",      type = "text"),
  list(key = "mp_email",          label = "Metadata provider - email",         type = "text"),
  list(key = "contact_same",      label = "Contact same as creator?",          type = "logical",  notes = "\"Creator\" here means the FIRST row of the separate 'creators' sheet"),
  list(key = "contact_given",     label = "Contact - given name",              type = "text",     notes = "Only if not same as creator"),
  list(key = "contact_sur",       label = "Contact - surname",                 type = "text"),
  list(key = "contact_position",  label = "Contact - position",                type = "text"),
  list(key = "contact_email",     label = "Contact - email",                   type = "text"),
  list(key = "methods",           label = "Method steps",                      type = "textarea", notes = "One step per line, in order"),
  list(key = "study_extent",      label = "Study extent description",          type = "textarea"),
  list(key = "sampling_desc",     label = "Sampling description",              type = "textarea"),
  list(key = "project_id",        label = "Project ID",                        type = "text",     notes = "Leave blank to omit the whole Project section"),
  list(key = "project_title",     label = "Project title",                     type = "text"),
  list(key = "project_abstract",  label = "Project abstract",                  type = "textarea"),
  list(key = "funding",           label = "Funding",                          type = "textarea"),
  list(key = "study_area_desc",   label = "Study area description",           type = "textarea"),
  list(key = "design_desc",       label = "Design description",                type = "textarea")
)

# EML allows multiple <creator> elements (a dataset commonly has several
# authors) and multiple <project><personnel> elements, which don't fit
# the one-row-per-field "metadata" sheet used for everything else - so
# each gets its own sheet, one row per person, via the same generic
# read/write helpers below (write_person_rows_sheet / read_person_rows_sheet).

# Each creator has their OWN organization/address (not the shared
# "Organization / address" block used for everyone else), since
# co-authors commonly belong to different institutions.
CREATOR_COLUMNS <- c("given_name", "sur_name", "email", "position",
                     "organization", "delivery_point", "city", "admin_area", "country", "phone")
CREATOR_COLUMN_LABELS <- c(
  given_name = "Given name", sur_name = "Surname", email = "Email",
  position = "Position", organization = "Organization",
  delivery_point = "Street address", city = "City",
  admin_area = "State/region", country = "Country code", phone = "Phone"
)
CREATOR_COLUMN_WIDTHS <- c(16, 16, 28, 22, 24, 22, 14, 14, 12, 14)

# Personnel share the "Organization / address" block (see app.R's org
# list), so they only need their own name/position/email/role.
PERSONNEL_COLUMNS <- c("given_name", "sur_name", "email", "position", "role")
PERSONNEL_COLUMN_LABELS <- c(
  given_name = "Given name", sur_name = "Surname", email = "Email",
  position = "Position", role = "Role"
)
PERSONNEL_COLUMN_WIDTHS <- c(16, 16, 28, 24, 22)

# Associated parties also share the "Organization / address" block, same
# shape as personnel - a dataset commonly credits several people here,
# each in a DIFFERENT role (author, editor, reviewer, processor,
# curator, programmer, content provider, publisher, ...). A row can be
# left fully blank (or the sheet can have zero data rows) to include
# none at all - it's entirely optional, unlike creators.
AP_COLUMNS <- c("given_name", "sur_name", "email", "position", "role")
AP_COLUMN_LABELS <- c(
  given_name = "Given name", sur_name = "Surname", email = "Email",
  position = "Position", role = "Role"
)
AP_COLUMN_WIDTHS <- c(16, 16, 28, 24, 22)

# Writes one row per person to its own sheet in `wb` (used for both
# "creators" and "personnel" - same shape, different columns).
write_person_rows_sheet <- function(wb, sheet_name, people, columns, column_labels, widths, header_style) {
  if (is.null(people) || length(people) == 0) people <- list(list())
  df <- do.call(rbind, lapply(people, function(p) {
    as.data.frame(setNames(lapply(columns, function(col) {
      v <- p[[col]]
      if (is.null(v) || (length(v) == 1 && is.na(v))) "" else as.character(v)
    }), columns), stringsAsFactors = FALSE)
  }))
  names(df) <- column_labels[columns]

  openxlsx::addWorksheet(wb, sheet_name)
  openxlsx::writeData(wb, sheet_name, df)
  openxlsx::setColWidths(wb, sheet_name, cols = seq_along(columns), widths = widths)
  openxlsx::addStyle(wb, sheet_name, header_style, rows = 1, cols = seq_along(columns), gridExpand = TRUE)
}

# Reads a person-rows sheet back into a list of person lists, dropping
# fully-blank rows (e.g. the template's single empty starter row).
# Returns list() (not an error) if the sheet is missing entirely, so an
# older template without it still loads.
read_person_rows_sheet <- function(path, sheet_name, columns, column_labels) {
  tryCatch({
    raw <- openxlsx::read.xlsx(path, sheet = sheet_name, colNames = TRUE)
    # openxlsx sanitizes header cells with spaces into dots on read
    # ("Given name" -> "Given.name"), so match on that same form.
    sanitized_labels <- gsub(" ", ".", column_labels[columns], fixed = TRUE)
    names(raw) <- columns[match(names(raw), sanitized_labels)]
    rows <- lapply(seq_len(nrow(raw)), function(i) {
      row <- as.list(raw[i, ])
      lapply(row, function(v) if (is.na(v)) "" else as.character(v))
    })
    Filter(function(r) any(nzchar(trimws(unlist(r)))), rows)
  }, error = function(e) list())
}

#' Write the metadata Excel template (or a pre-filled copy, if `values` is given).
#'
#' @param path Output .xlsx path
#' @param values Optional named list: field key -> value, to pre-fill (e.g. from
#'   an existing eml.xml). Logical values may be TRUE/FALSE or "TRUE"/"FALSE".
#' @param creators Optional list of creator lists (each with given_name,
#'   sur_name, position, email, phone, organization, delivery_point, city,
#'   admin_area, country) - one row per creator on the "creators" sheet.
#'   Defaults to one blank row for the template.
#' @param personnel Optional list of person lists (each with given_name,
#'   sur_name, position, email, role) - one row per person on the
#'   "personnel" sheet (project personnel). Defaults to one blank row.
#' @param associated_parties Optional list of person lists (each with
#'   given_name, sur_name, position, email, role) - one row per person on
#'   the "associated_parties" sheet. Defaults to one blank row (leave
#'   blank, or delete data rows entirely, to include none).
write_eml_metadata_excel <- function(path, values = NULL, creators = NULL, personnel = NULL, associated_parties = NULL) {
  df <- data.frame(
    Field = vapply(EML_FIELD_DEFS, function(f) f$label, character(1)),
    Value = vapply(EML_FIELD_DEFS, function(f) {
      v <- if (!is.null(values)) values[[f$key]] else NULL
      if (is.null(v) || (length(v) == 1 && is.na(v))) "" else paste(v, collapse = if (f$type %in% c("textarea")) "\n" else ", ")
    }, character(1)),
    Notes = vapply(EML_FIELD_DEFS, function(f) if (!is.null(f$notes)) f$notes else "", character(1)),
    key = vapply(EML_FIELD_DEFS, function(f) f$key, character(1)),
    stringsAsFactors = FALSE
  )

  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "metadata")
  openxlsx::writeData(wb, "metadata", df[, c("Field", "Value", "Notes")])
  openxlsx::setColWidths(wb, "metadata", cols = 1:3, widths = c(32, 60, 40))
  wrap_style <- openxlsx::createStyle(wrapText = TRUE, valign = "top")
  openxlsx::addStyle(wb, "metadata", wrap_style, rows = 2:(nrow(df) + 1), cols = 2, gridExpand = TRUE)
  header_style <- openxlsx::createStyle(textDecoration = "bold", fgFill = "#eceae4")
  openxlsx::addStyle(wb, "metadata", header_style, rows = 1, cols = 1:3, gridExpand = TRUE)
  # A hidden 4th column keeps the field key next to its row, so the
  # reader below doesn't have to re-derive it from the (editable) label.
  openxlsx::writeData(wb, "metadata", data.frame(key = df$key), startCol = 4)
  openxlsx::setColWidths(wb, "metadata", cols = 4, hidden = TRUE)

  write_person_rows_sheet(wb, "creators", creators, CREATOR_COLUMNS, CREATOR_COLUMN_LABELS, CREATOR_COLUMN_WIDTHS, header_style)
  write_person_rows_sheet(wb, "personnel", personnel, PERSONNEL_COLUMNS, PERSONNEL_COLUMN_LABELS, PERSONNEL_COLUMN_WIDTHS, header_style)
  write_person_rows_sheet(wb, "associated_parties", associated_parties, AP_COLUMNS, AP_COLUMN_LABELS, AP_COLUMN_WIDTHS, header_style)

  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
  invisible(path)
}

#' Read a filled-in metadata Excel file back into a named list of values,
#' keyed the same way as EML_FIELD_DEFS (and thus the same as the "eml_"
#' Shiny input ids, minus the prefix), plus a $creators element (list of
#' creator lists, one per non-blank row of the "creators" sheet).
#'
#' @param path Path to the uploaded .xlsx
#' @return Named list: field key -> value (character, or TRUE/FALSE for
#'   logical fields; missing/blank cells are omitted), plus $creators
#'   and $personnel (each a list of person lists)
read_eml_metadata_excel <- function(path) {
  raw <- openxlsx::read.xlsx(path, sheet = 1, colNames = TRUE)
  if (!"key" %in% names(raw)) {
    stop("This file doesn't look like a Multi-Assay Toolkit (MAT) metadata template - missing the hidden 'key' column (column D). Start from the downloaded template rather than a blank workbook.")
  }

  values <- list()
  for (i in seq_len(nrow(raw))) {
    key <- raw$key[i]
    def <- Filter(function(f) f$key == key, EML_FIELD_DEFS)
    if (length(def) == 0) next
    def <- def[[1]]

    val <- raw$Value[i]
    if (is.na(val) || !nzchar(trimws(as.character(val)))) next
    val <- gsub("\r\n", "\n", as.character(val))

    values[[key]] <- switch(def$type,
      logical = toupper(trimws(val)) %in% c("TRUE", "YES", "Y", "1"),
      val
    )
  }

  values$creators           <- read_person_rows_sheet(path, "creators", CREATOR_COLUMNS, CREATOR_COLUMN_LABELS)
  values$personnel          <- read_person_rows_sheet(path, "personnel", PERSONNEL_COLUMNS, PERSONNEL_COLUMN_LABELS)
  values$associated_parties <- read_person_rows_sheet(path, "associated_parties", AP_COLUMNS, AP_COLUMN_LABELS)

  values
}
