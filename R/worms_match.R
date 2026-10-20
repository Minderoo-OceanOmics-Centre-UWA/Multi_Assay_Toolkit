### R/worms_match.R
#
# Refactored from scripts/04_worms_match.R (now a thin CLI wrapper
# around match_worms() below). Pure function - see that script for the
# full "never guess" design rationale.

strip_voucher_code <- function(name) {
  if (!grepl("[0-9]", name)) return(name)
  cleaned <- sub("\\s+gen\\.\\s*sp\\.\\s*\\S+$", "", name)
  cleaned <- sub("\\s+[A-Za-z]+:?[0-9]+-?[0-9]*$", "", cleaned)
  trimws(cleaned)
}

# NCBI Taxonomy's own homonym-disambiguation suffix, e.g.
# "Centropogon australis (in: eudicots)" - NCBI shows this as
# "Centropogon <eudicots>" on its own site when a name is ambiguous
# across kingdoms; some reference databases export it with this
# "(in: X)" suffix instead. Generic and safe to strip for ANY name,
# not just this one case: WoRMS only registers marine species, so
# querying the stripped name can only resolve to the marine organism
# even if the tag pointed at a non-marine kingdom (there's no marine
# "eudicots" entry to collide with) - it can't silently pick the wrong
# side of the homonym the way a voucher-code strip theoretically could.
strip_ncbi_homonym_tag <- function(name) {
  trimws(sub("\\s*\\(in:\\s*[^)]+\\)\\s*$", "", name))
}

clean_name_for <- function(name, name_corrections) {
  if (name %in% names(name_corrections)) return(unname(name_corrections[name]))
  name <- strip_ncbi_homonym_tag(name)
  strip_voucher_code(name)
}

#' Suggest a correction for a name with NO exact WoRMS match, using
#' WoRMS' own fuzzy-matching (taxamatch) service - typo-tolerant,
#' no AI needed. Only returns a suggestion when the fuzzy search finds
#' exactly one candidate (same "don't guess" rule as everywhere else in
#' this file - a fuzzy search returning several candidates is a real
#' ambiguity, not something to pick from automatically), and always
#' follows through to the CURRENT accepted name (valid_name) via
#' WoRMS' own synonym resolution, not just the literal fuzzy-matched
#' spelling - e.g. "Lampanyctus reinhardti" (misspelled, wrong/outdated
#' genus) correctly suggests "Hygophum reinhardtii", not merely the
#' spelling-corrected but still-outdated "Lampanyctus reinhardtii".
#' A SUGGESTION only - the caller must still show it for confirmation,
#' never apply it silently.
suggest_correction_via_taxamatch <- function(name) {
  result <- tryCatch(worrms::wm_records_taxamatch(name = name, marine_only = FALSE), error = function(e) NULL)
  if (is.null(result) || length(result) == 0) return(NA_character_)
  recs <- result[[1]]
  if (is.null(recs) || nrow(recs) != 1) return(NA_character_)
  valid_name <- recs$valid_name[1]
  if (is.na(valid_name) || valid_name == "") return(NA_character_)
  valid_name
}

#' Every close name WoRMS' typo-tolerant search finds for `name`, each
#' followed through to its current accepted name. Unlike
#' suggest_correction_via_taxamatch() this keeps several candidates, so a
#' misspelling with more than one plausible fix is still recognised as a
#' misspelling (the reviewer picks). Empty when nothing is close.
fuzzy_candidates_via_taxamatch <- function(name) {
  result <- tryCatch(worrms::wm_records_taxamatch(name = name, marine_only = FALSE), error = function(e) NULL)
  if (is.null(result) || length(result) == 0 || is.null(result[[1]]) || nrow(result[[1]]) == 0) return(character())
  v <- result[[1]]$valid_name
  unique(v[!is.na(v) & nzchar(v)])
}

#' Match every unique scientificName across a set of Occurrence tables
#' against WoRMS, resolving as much as possible without guessing.
#'
#' @param occurrence_tables Named list of per-assay Occurrence tibbles
#' @param name_corrections Named character vector: raw scientificName -> corrected
#'   name to query instead (spelling fixes, stripped voucher codes, wrong-kingdom
#'   homonym corrections - each entry should be a reviewed, justified correction,
#'   never a guess)
#' @param manual_aphia_overrides Named vector (name -> AphiaID) for names where
#'   WoRMS returns >1 'accepted' record for genuinely different organisms (a
#'   cross-kingdom homonym) - the auto-resolve-on-single-accepted rule can't
#'   disambiguate these, so each one is a reviewed manual decision
#' @param batch_size WoRMS query batch size (wm_records_names)
#' @return list(occurrence_tables (updated with scientificName/scientificNameID),
#'   matched_df, ambiguous_df, unmatched_names, resolved_ambiguous_df,
#'   non_marine_df, messages)
match_worms <- function(occurrence_tables,
                         name_corrections = character(),
                         manual_aphia_overrides = c(),
                         batch_size = 50) {
  messages <- character()
  log_msg <- function(...) messages <<- c(messages, paste0(...))

  all_names <- unique(unlist(lapply(occurrence_tables, function(x) x$scientificName)))
  all_names <- all_names[!is.na(all_names)]

  # "Biota incertae sedis" already has its fixed WoRMS LSID (AphiaID 12)
  # assigned upstream - don't waste an API call on it.
  raw_names_to_match <- setdiff(all_names, "Biota incertae sedis")
  clean_lookup <- setNames(
    vapply(raw_names_to_match, clean_name_for, character(1), name_corrections = name_corrections),
    raw_names_to_match
  )

  corrections_made <- clean_lookup[clean_lookup != names(clean_lookup)]
  if (length(corrections_made) > 0) {
    log_msg("Applying ", length(corrections_made), " name correction(s) before WoRMS matching:")
    for (raw in names(corrections_made)) {
      log_msg("  '", raw, "' -> '", corrections_made[[raw]], "'")
    }
  }

  names_to_match <- unique(unname(clean_lookup))
  log_msg("Unique scientificName values across all assays: ", length(all_names))
  log_msg("Sending to WoRMS for matching: ", length(names_to_match), " (after corrections)")

  batches <- split(names_to_match, ceiling(seq_along(names_to_match) / batch_size))

  matched_rows       <- list()
  unmatched_names    <- character()
  ambiguous_names    <- list()
  resolved_ambiguous <- list()

  # One matched_df row from one WoRMS record. valid_* is WoRMS' current
  # accepted name for that record (the same record when it is accepted).
  matched_row <- function(queried_name, rec, aphia_id = rec$AphiaID[1]) {
    tibble::tibble(
      queriedName           = queried_name,
      scientificNameID      = paste0("urn:lsid:marinespecies.org:taxname:", aphia_id),
      worms_status          = rec$status[1],
      worms_valid_name      = rec$valid_name[1],
      worms_valid_AphiaID   = rec$valid_AphiaID[1],
      worms_valid_authority = rec$valid_authority[1],
      worms_rank            = rec$rank[1],
      worms_isMarine        = rec$isMarine[1],
      worms_isBrackish      = rec$isBrackish[1],
      worms_isFreshwater    = rec$isFreshwater[1],
      worms_isTerrestrial   = rec$isTerrestrial[1]
    )
  }

  for (i in seq_along(batches)) {
    batch <- batches[[i]]
    log_msg("Matching batch ", i, " of ", length(batches), " (", length(batch), " names)...")

    result <- tryCatch(
      worrms::wm_records_names(name = batch, marine_only = FALSE),
      error = function(e) {
        log_msg("  WARNING: batch ", i, " failed: ", conditionMessage(e))
        NULL
      }
    )
    if (is.null(result)) next

    for (j in seq_along(batch)) {
      queried_name <- batch[j]
      recs <- result[[j]]

      if (is.null(recs) || nrow(recs) == 0) {
        unmatched_names <- c(unmatched_names, queried_name)

      } else if (nrow(recs) == 1) {
        matched_rows[[queried_name]] <- matched_row(queried_name, recs)

      } else {
        # More than one WoRMS record shares this name. Three ways this
        # gets resolved, in order - never by guessing: (1) a manual
        # AphiaID override, (2) exactly one candidate is 'accepted'
        # (WoRMS' own canonical pick), (3) otherwise genuinely
        # ambiguous - flagged for manual review.
        accepted_rows <- recs %>% dplyr::filter(status == "accepted")

        if (queried_name %in% names(manual_aphia_overrides)) {
          chosen_id  <- unname(manual_aphia_overrides[queried_name])
          chosen_row <- recs %>% dplyr::filter(AphiaID == chosen_id)
          matched_rows[[queried_name]] <- matched_row(queried_name, chosen_row, chosen_id)
          resolved_ambiguous[[queried_name]] <- recs %>%
            dplyr::mutate(queriedName = queried_name, chosenAphiaID = chosen_id, resolution = "manual_override") %>%
            dplyr::select(queriedName, AphiaID, scientificname, status, rank, isMarine, chosenAphiaID, resolution)

        } else if (nrow(accepted_rows) == 1) {
          matched_rows[[queried_name]] <- matched_row(queried_name, accepted_rows)
          resolved_ambiguous[[queried_name]] <- recs %>%
            dplyr::mutate(queriedName = queried_name, chosenAphiaID = accepted_rows$AphiaID[1], resolution = "single_accepted") %>%
            dplyr::select(queriedName, AphiaID, scientificname, status, rank, isMarine, chosenAphiaID, resolution)

        } else {
          ambiguous_names[[queried_name]] <- recs %>%
            dplyr::mutate(queriedName = queried_name) %>%
            dplyr::select(queriedName, AphiaID, scientificname, status, rank, isMarine)
        }
      }
    }
  }

  log_msg("Matching complete.")
  log_msg("  Unique matches: ", length(matched_rows))
  log_msg("  - of which auto-resolved from >1 WoRMS record: ", length(resolved_ambiguous))
  log_msg("  Ambiguous (>1 accepted, or 0): ", length(ambiguous_names))
  log_msg("  No match found: ", length(unmatched_names))

  # For every unmatched name, try WoRMS' own fuzzy-match service as a
  # suggested correction (see suggest_correction_via_taxamatch()) - a
  # suggestion surfaced to the caller for confirmation, not applied here.
  # fuzzy_candidates keeps every close name (one WoRMS call per name);
  # suggested_corrections keeps the old single-candidate meaning for scripts.
  suggested_corrections <- character()
  fuzzy_candidates <- list()
  if (length(unmatched_names) > 0) {
    log_msg("Looking up fuzzy-match suggestions for unmatched names...")
    fuzzy_candidates <- setNames(lapply(unmatched_names, fuzzy_candidates_via_taxamatch), unmatched_names)
    fuzzy_candidates <- fuzzy_candidates[lengths(fuzzy_candidates) > 0]
    single <- fuzzy_candidates[lengths(fuzzy_candidates) == 1]
    suggested_corrections <- vapply(single, function(x) x[1], character(1))
    if (length(fuzzy_candidates) > 0) {
      log_msg("  Found close names for ", length(fuzzy_candidates), " name(s).")
    }
  }

  matched_df <- dplyr::bind_rows(matched_rows)
  ambiguous_df <- if (length(ambiguous_names) > 0) dplyr::bind_rows(ambiguous_names) else tibble::tibble()
  resolved_ambiguous_df <- if (length(resolved_ambiguous) > 0) dplyr::bind_rows(resolved_ambiguous) else tibble::tibble()
  # Anything WoRMS doesn't mark as marine is flagged, including brackish-only
  # records (e.g. Albula glossodonta: isMarine = 0, isBrackish = 1). The user
  # reviews each one in the app and accepts it - not decided by a rule here.
  non_marine_df <- if (nrow(matched_df) > 0) {
    matched_df %>% dplyr::filter(worms_isMarine == 0 | is.na(worms_isMarine))
  } else {
    tibble::tibble()
  }
  # Names WoRMS knows, but lists under a different current accepted name
  # (synonyms, outdated combinations). The user decides per name whether
  # to switch - see apply_name_choices().
  synonym_df <- if (nrow(matched_df) > 0) {
    matched_df %>% dplyr::filter(!is.na(worms_valid_name), worms_valid_name != "",
                                 worms_valid_name != queriedName, !is.na(worms_valid_AphiaID))
  } else {
    tibble::tibble()
  }

  # Apply matched scientificNameIDs (and corrected scientificName) back
  # into each Occurrence table. A correction that did NOT find a match
  # is left completely untouched (raw name, no ID).
  lookup <- setNames(matched_df$scientificNameID, matched_df$queriedName)

  updated_tables <- lapply(occurrence_tables, function(occ) {
    occ %>%
      dplyr::mutate(
        cleanName = dplyr::case_when(
          scientificName == "Biota incertae sedis" ~ scientificName,
          scientificName %in% names(clean_lookup)  ~ unname(clean_lookup[scientificName]),
          TRUE                                      ~ scientificName
        ),
        scientificNameID = dplyr::case_when(
          scientificName == "Biota incertae sedis" ~ scientificNameID,
          cleanName %in% names(lookup)             ~ unname(lookup[cleanName]),
          TRUE ~ scientificNameID
        ),
        scientificName = dplyr::if_else(
          scientificName != "Biota incertae sedis" & !is.na(scientificNameID),
          cleanName,
          scientificName
        )
      ) %>%
      dplyr::select(-cleanName)
  })

  list(
    occurrence_tables      = updated_tables,
    matched_df             = matched_df,
    ambiguous_df           = ambiguous_df,
    unmatched_names        = unmatched_names,
    resolved_ambiguous_df  = resolved_ambiguous_df,
    non_marine_df          = non_marine_df,
    synonym_df             = synonym_df,
    suggested_corrections  = suggested_corrections,
    fuzzy_candidates       = fuzzy_candidates,   # unmatched name -> every close WoRMS name
    name_lookup          = clean_lookup,   # raw scientificName -> name sent to WoRMS
    messages               = messages
  )
}

#' Apply the reviewer's per-name choices to match_worms() output.
#'
#' @param worms_result match_worms() output
#' @param use_accepted Names (as sent to WoRMS) to switch to WoRMS' current
#'   accepted name. Anything not listed keeps the reviewer's own name, still
#'   linked to its WoRMS record (OBIS shows it under the accepted name).
#' @param keep_names Unmatched names the reviewer confirmed are real but not
#'   yet in WoRMS. Kept as-is with scientificNameID left empty - OBIS's own
#'   guidance for names without an AphiaID (manual.obis.org/dna_data): the
#'   NCBI ID stays in taxonConceptID and OBIS links the record to the nearest
#'   higher rank it finds in WoRMS. Only a remark is added.
#' @param unknown_names Unmatched names to publish as "Biota incertae sedis"
#'   (AphiaID 12) instead - OBIS's name for unknown sequences.
#' @return The occurrence tables, with each change noted in
#'   identificationRemarks. verbatimIdentification is never touched, so the
#'   original identification is always kept.
#' @param auto_id_off Auto-resolved names (several WoRMS records, one
#'   accepted) whose automatically chosen ID the reviewer turned off. Their
#'   scientificNameID is left empty, so OBIS matches the name itself on
#'   ingestion. The name is unchanged either way.
apply_name_choices <- function(worms_result, use_accepted = character(), unknown_names = character(),
                               keep_names = character(), auto_id_off = character()) {
  tables <- worms_result$occurrence_tables
  if (length(use_accepted) == 0 && length(unknown_names) == 0 && length(keep_names) == 0 &&
      length(auto_id_off) == 0) return(tables)

  add_remark <- function(old, note) {
    ifelse(is.na(old) | old == "", note, paste(old, note, sep = " | "))
  }
  syn <- worms_result$synonym_df
  syn <- if (!is.null(syn) && nrow(syn) > 0) syn[syn$queriedName %in% use_accepted, , drop = FALSE] else NULL
  lookup <- worms_result$name_lookup
  unknown_raw <- c(unknown_names, names(lookup)[lookup %in% unknown_names])
  keep_raw <- c(keep_names, names(lookup)[lookup %in% keep_names])
  incertae_id <- "urn:lsid:marinespecies.org:taxname:12"

  lapply(tables, function(occ) {
    if (!is.null(syn) && nrow(syn) > 0) {
      i <- match(occ$scientificName, syn$queriedName)
      hit <- !is.na(i)
      if (any(hit)) {
        old_name <- occ$scientificName[hit]
        occ$scientificName[hit]   <- syn$worms_valid_name[i[hit]]
        occ$scientificNameID[hit] <- paste0("urn:lsid:marinespecies.org:taxname:", syn$worms_valid_AphiaID[i[hit]])
        if ("scientificNameAuthorship" %in% names(occ)) occ$scientificNameAuthorship[hit] <- syn$worms_valid_authority[i[hit]]
        if ("identificationRemarks" %in% names(occ)) {
          occ$identificationRemarks[hit] <- add_remark(occ$identificationRemarks[hit],
            paste0("Name updated from '", old_name, "' to the WoRMS accepted name '", syn$worms_valid_name[i[hit]], "'."))
        }
      }
    }
    hit <- occ$scientificName %in% unknown_raw & (is.na(occ$scientificNameID) | occ$scientificNameID == "")
    if (any(hit)) {
      old_name <- occ$scientificName[hit]
      occ$scientificName[hit]   <- "Biota incertae sedis"
      occ$scientificNameID[hit] <- incertae_id
      if ("taxonRank" %in% names(occ)) occ$taxonRank[hit] <- NA_character_
      if ("scientificNameAuthorship" %in% names(occ)) occ$scientificNameAuthorship[hit] <- NA_character_
      if ("identificationRemarks" %in% names(occ)) {
        occ$identificationRemarks[hit] <- add_remark(occ$identificationRemarks[hit],
          paste0("Identified as '", old_name, "', which is not yet registered in WoRMS; recorded as Biota incertae sedis until it is."))
      }
    }
    hit <- occ$scientificName %in% auto_id_off
    if (any(hit)) {
      occ$scientificNameID[hit] <- NA_character_
      if ("identificationRemarks" %in% names(occ)) {
        occ$identificationRemarks[hit] <- add_remark(occ$identificationRemarks[hit],
          "WoRMS has several records for this name; the WoRMS ID is left for OBIS to assign.")
      }
    }
    hit <- occ$scientificName %in% keep_raw & (is.na(occ$scientificNameID) | occ$scientificNameID == "")
    if (any(hit) && "identificationRemarks" %in% names(occ)) {
      occ$identificationRemarks[hit] <- add_remark(occ$identificationRemarks[hit],
        "Name not yet registered in WoRMS; scientificNameID left empty (taxonConceptID holds the reference-database ID).")
    }
    occ
  })
}
