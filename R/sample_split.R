### R/sample_split.R
#
# How MAT tells real samples from controls, as one small function the
# Step 1 "Sample handling" card uses to show the user what it found.
# The rule is the same one build_event_core() / get_real_event_ids() /
# validate_faire_files() apply: a sampleMetadata row is a real sample only
# when samp_category is exactly SAMPLE_CATEGORY_KEEP ("sample"); every other
# value, blanks included, is left out of the archive.

#' Split one FAIRe file's sampleMetadata into kept samples and removed rows.
#'
#' @param path Path to a FAIRe .xlsx file
#' @param keep The samp_category value that marks a real sample
#' @return list(categories = tibble(samp_category, n, kept),
#'   kept = character samp_names, removed = tibble(samp_name, samp_category),
#'   near_misses = character values that look like "sample" but aren't exact)
summarise_sample_split <- function(path, keep = "sample") {
  sm <- suppressMessages(read_faire_sheet(path, "sampleMetadata"))
  if (!all(c("samp_name", "samp_category") %in% names(sm))) {
    stop("sampleMetadata has no samp_name / samp_category column.")
  }
  sm <- sm[!is.na(sm$samp_name) & nzchar(trimws(sm$samp_name)), c("samp_name", "samp_category")]
  cat_label <- ifelse(is.na(sm$samp_category) | !nzchar(trimws(sm$samp_category)), "(blank)", sm$samp_category)
  is_kept <- !is.na(sm$samp_category) & sm$samp_category == keep

  categories <- dplyr::tibble(samp_category = cat_label, kept = is_kept) %>%
    dplyr::count(samp_category, kept, name = "n") %>%
    dplyr::arrange(dplyr::desc(kept), dplyr::desc(n))

  # Values that a person probably meant as "sample" (e.g. "Sample", "sample ")
  # but that don't match exactly - worth pointing out, since they get removed.
  removed_values <- unique(cat_label[!is_kept])
  near_misses <- removed_values[tolower(trimws(removed_values)) == tolower(keep)]

  list(
    categories  = categories,
    kept        = sm$samp_name[is_kept],
    removed     = dplyr::tibble(samp_name = sm$samp_name[!is_kept], samp_category = cat_label[!is_kept]),
    near_misses = near_misses
  )
}

#' Do several assay files list the same samples?
#'
#' Every assay in a MAT archive must describe the SAME physical samples,
#' because the Event core is written once and every assay links to it.
#'
#' @param names_by_assay Named list: assay name -> character vector of samp_name
#' @return list(same = TRUE/FALSE, n_union = total distinct samples,
#'   per_assay = tibble(assay, n_samples, n_missing, missing (list of samp_names
#'   this assay lacks), n_duplicated, duplicated (list)))
compare_sample_names <- function(names_by_assay) {
  clean <- lapply(names_by_assay, function(x) { x <- as.character(x); x[!is.na(x) & nzchar(trimws(x))] })
  all_names <- unique(unlist(clean, use.names = FALSE))
  per_assay <- dplyr::tibble(
    assay        = names(clean),
    n_samples    = vapply(clean, function(x) length(unique(x)), integer(1)),
    missing      = lapply(clean, function(x) sort(setdiff(all_names, x))),
    duplicated   = lapply(clean, function(x) sort(unique(x[duplicated(x)])))
  )
  per_assay$n_missing    <- lengths(per_assay$missing)
  per_assay$n_duplicated <- lengths(per_assay$duplicated)
  list(
    same      = all(per_assay$n_missing == 0) && all(per_assay$n_duplicated == 0),
    n_union   = length(all_names),
    per_assay = per_assay
  )
}

#' Read samp_name from each uploaded file and compare them.
#' @param paths Named list: assay (or file) label -> path to a FAIRe .xlsx
compare_sample_files <- function(paths) {
  compare_sample_names(lapply(paths, function(p) {
    sm <- suppressMessages(read_faire_sheet(p, "sampleMetadata"))
    sm$samp_name
  }))
}

# ---- Uneven sample lists (some samples not analysed with every assay) ----
.has_name <- function(t) !is.na(t$samp_name) & nzchar(trimws(as.character(t$samp_name)))
# Allowed only when the user confirms it in Step 2. Shared samples must still
# agree exactly; the Event core then uses every sample from every file.

#' Columns whose values differ between two sampleMetadata tables, compared
#' over the samples BOTH tables list (so uneven sample lists are fine here).
shared_metadata_mismatches <- function(a, b, exclude = "assay_name") {
  a <- a[.has_name(a), , drop = FALSE]; b <- b[.has_name(b), , drop = FALSE]
  shared <- intersect(a$samp_name, b$samp_name)
  cols <- intersect(setdiff(names(a), exclude), setdiff(names(b), exclude))
  a_sub <- a[a$samp_name %in% shared, cols, drop = FALSE] %>% dplyr::arrange(samp_name)
  b_sub <- b[b$samp_name %in% shared, cols, drop = FALSE] %>% dplyr::arrange(samp_name)
  names(which(!mapply(identical, a_sub, b_sub)))
}

#' One row per sample across all files: the first file's rows, then any
#' sample the first file doesn't have, taken from the next file that does.
union_sample_table <- function(sample_tables) {
  out <- sample_tables[[1]]
  for (t in sample_tables[-1]) {
    extra <- t[.has_name(t) & !t$samp_name %in% out$samp_name, , drop = FALSE]
    if (nrow(extra) > 0) {
      out <- dplyr::bind_rows(dplyr::mutate(out, dplyr::across(dplyr::everything(), as.character)),
                              dplyr::mutate(extra, dplyr::across(dplyr::everything(), as.character)))
    }
  }
  out
}

#' Which assays list each sample. Returns tibble(samp_name, assays_run, assays_not_run)
#' with comma-separated assay names (assays_not_run is "" when all were run).
sample_assay_coverage <- function(sample_tables) {
  all_names <- unique(unlist(lapply(sample_tables, function(t) t$samp_name[.has_name(t)]), use.names = FALSE))
  run <- lapply(all_names, function(s) names(sample_tables)[vapply(sample_tables, function(t) s %in% t$samp_name, logical(1))])
  dplyr::tibble(
    samp_name      = all_names,
    assays_run     = vapply(run, paste, character(1), collapse = ", "),
    assays_not_run = vapply(run, function(r) paste(setdiff(names(sample_tables), r), collapse = ", "), character(1))
  )
}
