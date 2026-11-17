### scripts/update_reference_terms.R
#
# Refreshes the local copies of the official Darwin Core / GBIF definitions
# in reference/, which the app's mapping check (R/check_dwc_mapping.R) and
# column-name suggestions (R/faire_column_suggestions.R) read offline.
#
# Run from the repo root:   Rscript scripts/update_reference_terms.R [summary.md]
#
# Run every 3 months by .github/workflows/update-reference-terms.yml, which
# opens a pull request when anything changed - so a person reviews every
# change before it reaches the app. Nothing here changes what MAT publishes.
#
# Writes:
#   reference/gbif_terms_<table>.csv   term, type          (latest GBIF definition)
#   reference/gbif_vocabularies.csv    table, term, vocabulary, value
#   reference/dwc_term_versions.csv    term, issued, version_iri, definition, comments
#                                      (current Darwin Core recommendation for each
#                                       DwC term MAT writes - from tdwg/dwc on GitHub)
#   reference/reference_sources.csv    file, title, issued, url
# and, if a path is given, a markdown summary for the pull request.
#
# Everything is downloaded first; if any download fails, nothing is written.

suppressPackageStartupMessages({
  library(xml2)
  library(jsonlite)
})

REGISTRY_URL      <- "https://rs.gbif.org/extensions.json"
DWC_VERSIONS_URL  <- "https://raw.githubusercontent.com/tdwg/dwc/master/vocabulary/term_versions.csv"
DWC_NAMESPACE     <- "http://rs.tdwg.org/dwc/terms/"
# FAIRe checklist (term definitions, units, MIxS links) - used for the
# eMoF rows MAT writes from FAIRe columns (R/faire_column_suggestions.R).
FAIRE_CHECKLIST_DIR <- "https://api.github.com/repos/FAIR-eDNA/FAIRe_checklist/contents/latest_checklist"

# Which GBIF definition each archive file is checked against (by rowType).
DEFINITIONS <- c(
  event            = "http://rs.tdwg.org/dwc/terms/Event",
  occurrence       = "http://rs.tdwg.org/dwc/terms/Occurrence",
  dna_derived_data = "http://rs.gbif.org/terms/1.0/DNADerivedData",
  emof             = "http://rs.iobis.org/obis/terms/ExtendedMeasurementOrFact"
)

# Fixed values MAT writes into vocabulary-controlled terms (R/build_*.R).
# Listed in the summary when they aren't in the GBIF vocabulary.
MAT_FIXED_VALUES <- c(
  basisOfRecord    = "MaterialSample",
  occurrenceStatus = "detected"
)

ref <- function(...) file.path("reference", ...)
https <- function(u) sub("^http://", "https://", u)

read_csv_or_null <- function(path) {
  if (file.exists(path)) utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE) else NULL
}

# ---- 1. Download everything --------------------------------------------------

message("Reading the GBIF registry...")
registry <- fromJSON(REGISTRY_URL)$extensions

latest_definition <- function(row_type) {
  hits <- registry[registry$identifier == row_type & isTRUE_vec(registry$isLatest), , drop = FALSE]
  if (nrow(hits) == 0) hits <- registry[registry$identifier == row_type, , drop = FALSE]
  if (nrow(hits) == 0) stop("No GBIF definition found for ", row_type)
  hits[order(hits$issued, decreasing = TRUE), , drop = FALSE][1, ]
}
isTRUE_vec <- function(x) !is.na(x) & x %in% TRUE

definitions <- lapply(names(DEFINITIONS), function(tbl) {
  info <- latest_definition(DEFINITIONS[[tbl]])
  message("  ", tbl, ": ", info$url)
  doc <- read_xml(https(info$url))
  xml_ns_strip(doc)
  props <- xml_find_all(doc, "//property")
  type <- xml_attr(props, "type")
  list(
    table  = tbl,
    info   = info,
    terms  = data.frame(term = xml_attr(props, "name"),
                        type = ifelse(is.na(type) | !nzchar(type), "string", type),
                        stringsAsFactors = FALSE),
    thesaurus = data.frame(term = xml_attr(props, "name"), url = xml_attr(props, "thesaurus"),
                           stringsAsFactors = FALSE)
  )
})
names(definitions) <- names(DEFINITIONS)

message("Reading the vocabularies they use...")
thesauri <- unique(do.call(rbind, lapply(definitions, function(d) {
  t <- d$thesaurus[!is.na(d$thesaurus$url) & nzchar(d$thesaurus$url), , drop = FALSE]
  if (nrow(t) == 0) return(NULL)
  cbind(table = d$table, t)
})))
vocab_values <- lapply(unique(thesauri$url), function(u) {
  doc <- read_xml(https(u))
  xml_ns_strip(doc)
  v <- xml_attr(xml_find_all(doc, "//concept"), "identifier")
  sort(unique(v[!is.na(v)]))
})
names(vocab_values) <- unique(thesauri$url)
vocabularies <- do.call(rbind, lapply(seq_len(nrow(thesauri)), function(i) {
  vals <- vocab_values[[thesauri$url[i]]]
  if (length(vals) == 0) return(NULL)
  data.frame(table = thesauri$table[i], term = thesauri$term[i], vocabulary = thesauri$url[i],
             value = vals, stringsAsFactors = FALSE)
}))
vocabularies <- vocabularies[order(vocabularies$table, vocabularies$term, vocabularies$value), ]

message("Reading Darwin Core term history...")
tv <- utils::read.csv(DWC_VERSIONS_URL, stringsAsFactors = FALSE, check.names = FALSE)
used <- utils::read.csv(ref("mat_terms_used.csv"), stringsAsFactors = FALSE)
dwc_used <- sort(unique(used$term))
dwc <- tv[tv$status == "recommended" & tv$term_iri %in% paste0(DWC_NAMESPACE, dwc_used), , drop = FALSE]
dwc <- data.frame(term = sub(DWC_NAMESPACE, "", dwc$term_iri, fixed = TRUE), issued = dwc$issued,
                  version_iri = dwc$iri, definition = dwc$definition, comments = dwc$comments,
                  stringsAsFactors = FALSE)
dwc <- dwc[order(dwc$term), ]

message("Reading the FAIRe checklist...")
faire_files <- fromJSON(FAIRE_CHECKLIST_DIR)
faire_url <- faire_files$download_url[grepl("^FAIRe_checklist_v[0-9.]+[.]xlsx$", faire_files$name)]
if (length(faire_url) == 0) stop("No FAIRe checklist found in ", FAIRE_CHECKLIST_DIR)
faire_url <- sort(faire_url, decreasing = TRUE)[1]
faire_tmp <- tempfile(fileext = ".xlsx")
utils::download.file(faire_url, faire_tmp, mode = "wb", quiet = TRUE)
faire <- as.data.frame(readxl::read_excel(faire_tmp, sheet = "checklist"))
faire <- faire[, intersect(c("term_name", "data_type", "section", "description", "term_type", "unit", "URI", "example"), names(faire))]
faire <- faire[!is.na(faire$term_name), ]
faire$version <- sub(".*(v[0-9.]+)[.]xlsx$", "\\1", faire_url)

# ---- 2. Compare with what's in reference/ now ------------------------------

old <- list(
  terms   = lapply(names(definitions), function(t) read_csv_or_null(ref(paste0("gbif_terms_", t, ".csv")))),
  vocab   = read_csv_or_null(ref("gbif_vocabularies.csv")),
  dwc     = read_csv_or_null(ref("dwc_term_versions.csv")),
  sources = read_csv_or_null(ref("reference_sources.csv"))
)
names(old$terms) <- names(definitions)

lines <- c("## Darwin Core / GBIF reference check", "",
           paste0("Run on ", format(Sys.Date()), ". Review the changes below before merging - ",
                  "merging only updates the app's reference lists; it doesn't change what MAT publishes."), "")

lines <- c(lines, "### Definitions used", "", "| File | Definition | Issued |", "|---|---|---|")
for (d in definitions) {
  lines <- c(lines, sprintf("| gbif_terms_%s.csv | [%s](%s) | %s |", d$table, d$info$title, https(d$info$url), d$info$issued))
}
lines <- c(lines, "")

for (d in definitions) {
  o <- old$terms[[d$table]]
  if (is.null(o)) next
  added   <- setdiff(d$terms$term, o$term)
  removed <- setdiff(o$term, d$terms$term)
  both    <- intersect(d$terms$term, o$term)
  retyped <- both[d$terms$type[match(both, d$terms$term)] != o$type[match(both, o$term)]]
  if (length(c(added, removed, retyped)) == 0) next
  lines <- c(lines, paste0("### ", d$table))
  if (length(added))   lines <- c(lines, paste0("- New terms: ", paste0("`", added, "`", collapse = ", ")))
  if (length(removed)) lines <- c(lines, paste0("- **Removed terms:** ", paste0("`", removed, "`", collapse = ", ")))
  if (length(retyped)) lines <- c(lines, paste0("- Type changed: ", paste0("`", retyped, "`", collapse = ", ")))
  gone_used <- intersect(removed, used$term[used$table == d$table])
  if (length(gone_used)) lines <- c(lines, paste0("- **MAT writes ", paste0("`", gone_used, "`", collapse = ", "),
                                                  " - no longer a term here. The app needs updating.**"))
  lines <- c(lines, "")
}

if (!is.null(old$dwc)) {
  changed <- dwc$term[!(paste(dwc$term, dwc$issued) %in% paste(old$dwc$term, old$dwc$issued))]
  if (length(changed)) {
    lines <- c(lines, "### Darwin Core terms MAT writes that have a new version", "")
    for (t in changed) {
      r <- dwc[dwc$term == t, ][1, ]
      lines <- c(lines, sprintf("- **%s** (%s): %s", t, r$issued, gsub("\\s+", " ", r$comments)))
    }
    lines <- c(lines, "")
  }
}

old_faire <- read_csv_or_null(ref("faire_checklist.csv"))
if (!is.null(old_faire)) {
  if (!identical(old_faire$version[1], faire$version[1])) {
    lines <- c(lines, paste0("### FAIRe checklist: ", old_faire$version[1], " -> ", faire$version[1]), "")
  }
  key <- function(d) paste(d$term_name, d$unit, d$URI)
  changed <- setdiff(faire$term_name[!(key(faire) %in% key(old_faire))], character())
  if (length(changed)) {
    lines <- c(lines, "FAIRe terms that are new or have a new unit / link: ",
               paste0("`", head(changed, 40), "`", collapse = ", "), "")
  }
}

mismatch <- character()
for (term in names(MAT_FIXED_VALUES)) {
  allowed <- unique(vocabularies$value[vocabularies$term == term])
  if (length(allowed) && !(MAT_FIXED_VALUES[[term]] %in% allowed)) {
    mismatch <- c(mismatch, sprintf("- `%s` = `%s` - the GBIF vocabulary lists: %s", term, MAT_FIXED_VALUES[[term]],
                                    paste0("`", allowed, "`", collapse = ", ")))
  }
}
if (length(mismatch)) {
  lines <- c(lines, "### Values MAT writes that aren't in the GBIF vocabulary", "",
             "Not an error by itself (e.g. Darwin Core may have changed before GBIF caught up), but worth checking:", "",
             mismatch, "")
}

# ---- 3. Write --------------------------------------------------------------

write_ref <- function(df, name) utils::write.csv(df, ref(name), row.names = FALSE, na = "", fileEncoding = "UTF-8")
for (d in definitions) write_ref(d$terms, paste0("gbif_terms_", d$table, ".csv"))
write_ref(vocabularies, "gbif_vocabularies.csv")
write_ref(dwc, "dwc_term_versions.csv")
write_ref(faire, "faire_checklist.csv")
write_ref(data.frame(file = paste0("gbif_terms_", names(definitions), ".csv"),
                     title = vapply(definitions, function(d) d$info$title, ""),
                     issued = vapply(definitions, function(d) d$info$issued, ""),
                     url = vapply(definitions, function(d) https(d$info$url), ""),
                     stringsAsFactors = FALSE), "reference_sources.csv")

args <- commandArgs(trailingOnly = TRUE)
if (length(args) >= 1) writeLines(lines, args[1]) else cat(lines, sep = "\n")
message("Done.")
