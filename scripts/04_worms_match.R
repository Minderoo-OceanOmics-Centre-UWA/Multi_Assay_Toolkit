### 04_worms_match.R
#
# CLI wrapper around match_worms() (R/worms_match.R). Matches every
# unique scientificName produced by 02_build_occurrence.R against
# WoRMS, and fills in scientificNameID for all three Occurrence tables.
# Run this AFTER 02, as its own separate, reviewable step - see
# the project notes ("Taxonomy - handle as a SEPARATE step") for why this isn't
# folded into script 02.
#
# Uses the `worrms` R package (wraps the WoRMS REST API
# AphiaRecordsByName / matchAphiaRecordsByNames services), the tool
# recommended in the OBIS course for this exact task.
#
# NOT using obistools::match_taxa() here - checked its signature
# (match_taxa(names, ask = TRUE)): it's an interactive tool that
# prompts per-name for confirmation, built for a human reviewing a
# small list live in an R session. It doesn't scale to running this
# pipeline non-interactively (Rscript, ~670 names) with a saved audit
# trail, which is what this script needs. match_worms()'s
# name_corrections / manual_aphia_overrides / auto-resolve-on-single-
# accepted logic achieves the same "never guess" goal, just
# reproducibly and with every resolution logged to output/worms_match/
# for review.
#
# Output:
#   - Updates each Occurrence_<assay>.csv IN PLACE with scientificNameID
#     filled in for matched names
#   - output/worms_match/unmatched_names.csv   - names with NO WoRMS match at all
#   - output/worms_match/ambiguous_names.csv   - names with >1 possible match AND
#                                                 no single 'accepted' record among them
#   - output/worms_match/ambiguous_resolved.csv - names with >1 WoRMS record where
#                                                 exactly one was 'accepted' or a manual
#                                                 override applied - logged for audit
#   - output/worms_match/matched_names.csv     - the successful name -> LSID table (for reference/audit)
#   - output/worms_match/name_corrections_applied.csv - raw -> corrected name mappings applied
#
# IMPORTANT: unmatched_names.csv and ambiguous_names.csv need MANUAL
# REVIEW before you finalize the archive. Do not treat every row in
# matched_names.csv as necessarily correct either - spot check a sample,
# especially anything matched at genus/family level.
#
# name_corrections / manual_aphia_overrides below: found by manually
# reviewing a prior run's unmatched_names.csv / ambiguous_names.csv
# against WoRMS + FishBase. See each entry's comment for the specific
# reasoning - none of these are guesses.

library(dplyr)
library(readr)
library(worrms)

source("R/worms_match.R")

WORMS_OUTPUT_DIR <- file.path(OUTPUT_FOLDER, "worms_match")
dir.create(WORMS_OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

# ----------------------------------------------------------------------
# Manual name corrections (see R/worms_match.R's strip_voucher_code()
# for the generic voucher-code-stripping logic applied to any name
# containing a digit - these are the specific, reviewed overrides):
#   - "Lampanyctus reinhardti" -> "Hygophum reinhardtii" (WoRMS AphiaID
#     126604 - Lampanyctus reinhardtii is only an unaccepted synonym,
#     and the source data also had it misspelled with one "i").
#   - "Centropogon australis (in: eudicots)" -> "Centropogon australis":
#     "(in: eudicots)" is NCBI Taxonomy's own homonym-disambiguation tag
#     (NCBI has two unrelated "Centropogon" genera - a eudicot plant and
#     the ray-finned fish genus). The reference database behind
#     taxaFinal did a name-based rather than TaxID-based lookup and
#     landed on the wrong (plant) homonym. This is fish eDNA data, so
#     the real organism is the marine fish Centropogon australis
#     (White, 1790) - confirmed accepted, WoRMS AphiaID 280056.
# ----------------------------------------------------------------------
name_corrections <- c(
  "Lampanyctus reinhardti" = "Hygophum reinhardtii",
  "Centropogon australis (in: eudicots)" = "Centropogon australis"
)

# ----------------------------------------------------------------------
# Manual AphiaID overrides for names where WoRMS returns >1 'accepted'
# record for genuinely different organisms (a homonym across kingdoms,
# same pattern as the Centropogon fix above). Checked via WoRMS'
# AphiaID -> external-ID (NCBI) cross-reference first - no NCBI link
# exists for this taxon in either direction, so resolved instead from
# the WoRMS records' own family field, which cleanly separates the two
# organisms:
#   - Howella: AphiaID 126040 (Ogilby, 1899) is family Howellidae, a
#     fish genus - matches this project's 12S rRNA fish mtDNA data.
#     AphiaID 1647658 is family Halymeniaceae, a red-algae genus -
#     wrong kingdom entirely, not a candidate.
# ----------------------------------------------------------------------
manual_aphia_overrides <- c(
  "Howella" = 126040
)

# ----------------------------------------------------------------------
# Load Occurrence tables
# ----------------------------------------------------------------------
occurrence_files <- list.files(OCCURRENCE_OUTPUT_DIR, pattern = "^Occurrence_.*\\.csv$", full.names = TRUE)
if (length(occurrence_files) == 0) {
  stop("No Occurrence_*.csv files found in ", OCCURRENCE_OUTPUT_DIR, " - run 02_build_occurrence.R first.")
}
cat("Found", length(occurrence_files), "occurrence file(s):\n")
cat(paste(" -", basename(occurrence_files)), sep = "\n")

occurrence_tables <- lapply(occurrence_files, read_csv, show_col_types = FALSE)
names(occurrence_tables) <- basename(occurrence_files)

# ----------------------------------------------------------------------
# Match
# ----------------------------------------------------------------------
result <- match_worms(
  occurrence_tables      = occurrence_tables,
  name_corrections       = name_corrections,
  manual_aphia_overrides = manual_aphia_overrides
)

cat("\n", paste(result$messages, collapse = "\n"), "\n\n")

# ----------------------------------------------------------------------
# Write review files
# ----------------------------------------------------------------------
write_csv(result$matched_df, file.path(WORMS_OUTPUT_DIR, "matched_names.csv"))

if (nrow(result$resolved_ambiguous_df) > 0) {
  write_csv(result$resolved_ambiguous_df, file.path(WORMS_OUTPUT_DIR, "ambiguous_resolved.csv"))
  cat("AUTO/MANUALLY-RESOLVED (see output/worms_match/ambiguous_resolved.csv for audit)\n")
}

if (nrow(result$ambiguous_df) > 0) {
  write_csv(result$ambiguous_df, file.path(WORMS_OUTPUT_DIR, "ambiguous_names.csv"))
  cat("REVIEW NEEDED: output/worms_match/ambiguous_names.csv\n")
}

if (length(result$unmatched_names) > 0) {
  write_csv(tibble(scientificName = result$unmatched_names), file.path(WORMS_OUTPUT_DIR, "unmatched_names.csv"))
  cat("REVIEW NEEDED: output/worms_match/unmatched_names.csv\n")
}

if (nrow(result$non_marine_df) > 0) {
  write_csv(result$non_marine_df, file.path(WORMS_OUTPUT_DIR, "non_marine_matches.csv"))
  cat("REVIEW NEEDED:", nrow(result$non_marine_df), "matched names flagged non-marine - see output/worms_match/non_marine_matches.csv\n")
}

# ----------------------------------------------------------------------
# Write updated Occurrence tables back in place
# ----------------------------------------------------------------------
for (fname in names(result$occurrence_tables)) {
  out_path <- file.path(OCCURRENCE_OUTPUT_DIR, fname)
  write_csv(result$occurrence_tables[[fname]], out_path, na = "")
  n_filled <- sum(!is.na(result$occurrence_tables[[fname]]$scientificNameID))
  cat("Updated", fname, "-", n_filled, "/", nrow(result$occurrence_tables[[fname]]), "rows now have scientificNameID\n")
}

cat("\nDone. Before publishing: review ambiguous_names.csv, unmatched_names.csv,",
    "and non_marine_matches.csv in output/worms_match/.\n")
