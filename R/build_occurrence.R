### R/build_occurrence.R
#
# Refactored from scripts/02_build_occurrence.R (now a thin CLI wrapper
# around build_occurrence() below). Pure function - see that script for
# the full design rationale (the "dropped" placeholder handling, etc.).

resolve_scientific_name <- function(taxa) {
  # nt-database files label unidentified sequences "not applicable: domain
  # level dropped"; curated files use plain "not applicable". Same meaning.
  taxa$taxonRank <- dplyr::if_else(
    !is.na(taxa$taxonRank) & startsWith(taxa$taxonRank, "not applicable"),
    "not applicable", taxa$taxonRank
  )

  rank_col <- list(
    species = "scientificName",
    genus   = "genus",
    family  = "family",
    order   = "order",
    class   = "class",
    phylum  = "phylum",
    domain  = "domain"
  )

  # Vectorized equivalent of the original rowwise() version -
  # rowwise() processes one row-group at a time and carries real
  # per-row overhead (both speed and memory) on a table with one row
  # per ASV, which can run into the thousands. case_when() picks the
  # same "value for this row's taxonRank" without that overhead.
  val_for_rank <- dplyr::case_when(
    taxa$taxonRank == "species" ~ taxa$scientificName,
    taxa$taxonRank == "genus"   ~ taxa$genus,
    taxa$taxonRank == "family"  ~ taxa$family,
    taxa$taxonRank == "order"   ~ taxa$order,
    taxa$taxonRank == "class"   ~ taxa$class,
    taxa$taxonRank == "phylum"  ~ taxa$phylum,
    taxa$taxonRank == "domain"  ~ taxa$domain,
    TRUE ~ NA_character_
  )

  taxa %>%
    dplyr::mutate(
      resolvedScientificName = dplyr::case_when(
        # WoRMS AphiaID 12's actual scientificName is "Biota incertae
        # sedis" (verified directly against the live record) - not
        # "Incertae sedis" alone. OBIS's own DNA-derived-data guidance
        # (manual.obis.org/dna_data) requires this exact string for
        # wholly unknown sequences, paired with this LSID.
        taxonRank == "not applicable" ~ "Biota incertae sedis",
        taxonRank %in% names(rank_col) & !val_for_rank %in% c("dropped", "not applicable") ~ val_for_rank,
        TRUE ~ NA_character_
      ),
      resolvedScientificNameID = dplyr::if_else(
        taxonRank == "not applicable", "urn:lsid:marinespecies.org:taxname:12", NA_character_
      )
    )
}

get_real_event_ids <- function(source_assay_path, sample_category_keep) {
  samples <- read_faire_sheet(source_assay_path, "sampleMetadata")
  samples %>%
    dplyr::filter(samp_category == sample_category_keep) %>%
    dplyr::pull(samp_name)
}

#' Build one Darwin Core Occurrence extension per assay.
#'
#' @param input_files Named list: assay name -> path to that assay's FAIRe .xlsx
#' @param real_event_ids Character vector of real (non-control) eventIDs (see
#'   get_real_event_ids(), or reuse build_event_core()'s output eventIDs)
#' @param associated_sequences_uri Constant value for associatedSequences (e.g. an
#'   ENA/SRA project accession link)
#' @return list(occurrence = named list of per-assay tibbles, messages = character())
build_occurrence <- function(input_files, real_event_ids, associated_sequences_uri) {
  messages <- character()
  log_msg <- function(...) messages <<- c(messages, paste0(...))
  occurrence_tables <- list()

  for (assay in names(input_files)) {
    log_msg("Processing assay: ", assay)
    path <- input_files[[assay]]

    taxa <- read_faire_sheet(path, "taxaFinal")
    taxa <- resolve_scientific_name(taxa)
    n_unresolved <- sum(is.na(taxa$resolvedScientificName))
    log_msg("  Taxa rows: ", nrow(taxa), " | unresolved after fallback: ", n_unresolved)

    otu <- readxl::read_excel(path, sheet = "otuFinal", col_names = TRUE)
    names(otu)[1] <- "seq_id"
    keep_cols <- c("seq_id", intersect(names(otu), real_event_ids))
    otu <- otu[, keep_cols]

    sample_totals <- otu %>%
      dplyr::select(-seq_id) %>%
      dplyr::summarise(dplyr::across(dplyr::everything(), ~ sum(.x, na.rm = TRUE))) %>%
      tidyr::pivot_longer(dplyr::everything(), names_to = "eventID", values_to = "sampleSizeValue")

    # eDNA read-count tables are very sparse (mostly zeros) - pivoting
    # to long format FIRST and filtering afterward (the original
    # approach) briefly materializes one row per ASV x sample
    # combination, including every zero, which can be 20-100x larger
    # than the final non-zero result and was the dominant memory cost
    # in this whole pipeline (traced to an out-of-memory crash on a
    # 1GB-limited deployment). Extracting only the non-zero cells
    # directly from the matrix, via which(..., arr.ind = TRUE), never
    # creates that dense intermediate at all.
    otu_mat <- as.matrix(otu[, setdiff(names(otu), "seq_id")])
    storage.mode(otu_mat) <- "double"
    otu_mat[is.na(otu_mat)] <- 0
    nz <- which(otu_mat != 0, arr.ind = TRUE)
    otu_long <- tibble::tibble(
      seq_id           = otu$seq_id[nz[, "row"]],
      eventID          = colnames(otu_mat)[nz[, "col"]],
      organismQuantity = otu_mat[nz]
    )
    rm(otu_mat, nz)

    log_msg("  Non-zero ASV-by-sample detections: ", nrow(otu_long))

    occurrence <- otu_long %>%
      dplyr::left_join(taxa, by = "seq_id") %>%
      dplyr::left_join(sample_totals, by = "eventID") %>%
      dplyr::transmute(
        # eventID + seq_id alone is NOT globally unique: seq_id (the
        # ASV id, e.g. "ASV_2774") is numbered independently within
        # EACH assay's own pipeline run, not across assays - so two
        # different assays commonly reuse the same ASV number for
        # entirely different sequences, producing identical
        # occurrenceIDs once eventID (shared across assays, same
        # physical sample) is combined with just seq_id. Confirmed as
        # a real collision in production data (OBIS flagged duplicate
        # occurrenceIDs across Occurrence_16SFishD.csv and
        # Occurrence_MiFishUE2.csv for the same eventID+ASV number).
        # Including the assay name makes it unique across the whole
        # archive, not just within one assay's own file.
        occurrenceID = paste(eventID, assay, seq_id, sep = "_"),
        eventID,
        # seq_id (ASV id) is not a DwC term, so the IPT lists it as unmapped - expected and harmless (Sachit confirmed). Kept because build_dna_extension() needs it.
        seq_id,
        basisOfRecord = "MaterialSample",
        # Darwin Core 2026-05-26 recommends detected / notDetected (was present /
        # absent) - dwc.tdwg.org/terms/#dwc:occurrenceStatus. Asked for by OBIS AU.
        occurrenceStatus = "detected",
        scientificName = resolvedScientificName,
        scientificNameID = resolvedScientificNameID,
        taxonRank,
        taxonConceptID = ifelse(
          !is.na(taxonID) & !is.na(taxonID_db),
          paste(taxonID_db, taxonID, sep = ":"),
          NA_character_
        ),
        verbatimIdentification,
        scientificNameAuthorship = ifelse(
          is.na(scientificNameAuthorship) | grepl("^not applicable", scientificNameAuthorship),
          NA_character_, scientificNameAuthorship
        ),
        identificationReferences = ifelse(
          !is.na(accession_id) & accession_id != "not applicable" &
            !is.na(accession_id_ref_db) & accession_id_ref_db != "not applicable",
          paste(accession_id_ref_db, accession_id, sep = ":"),
          NA_character_
        ),
        identificationRemarks = dplyr::case_when(
          is.na(percent_match) | percent_match == "not applicable" ~ identificationRemarks,
          TRUE ~ paste0(
            identificationRemarks,
            ". Reference-database match: ", percent_match, "% identity, ",
            percent_query_cover, "% query coverage, confidence score ", confidence_score,
            dplyr::if_else(unusual_size == "TRUE", " (flagged: unusual sequence length).", ".")
          )
        ),
        organismQuantity,
        organismQuantityType = "DNA sequence reads",
        sampleSizeValue,
        sampleSizeUnit = "DNA sequence reads",
        associatedSequences = associated_sequences_uri
      )

    n_missing_name <- sum(is.na(occurrence$scientificName))
    if (n_missing_name > 0) {
      log_msg("  WARNING: ", n_missing_name, " rows have no resolvable scientificName - review before publishing.")
    }

    occurrence_tables[[assay]] <- occurrence
    log_msg("  Built ", nrow(occurrence), " rows for ", assay)
  }

  list(occurrence = occurrence_tables, messages = messages)
}
