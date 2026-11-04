### R/build_emof.R
#
# Optional extendedMeasurementOrFact (eMoF) extension: sampling facts that
# have no Darwin Core column of their own. Right now that is the vessel
# (and voyage) the samples were collected on, written the way the OBIS
# manual shows it (manual.obis.org/format_emof.html, "Sampling platform
# name" example): one row per event, linked by eventID, occurrenceID left
# blank because the fact belongs to the sample, not to one detection.
#
# Vocabulary (checked on vocab.nerc.ac.uk 2026-11-04):
#   measurementTypeID  = Q01 Q0100001 "Sampling platform name"
#   measurementValueID = the vessel's ICES platform code, NERC C17
#                        (e.g. RV Investigator = .../C17/current/096U/)
# The voyage is its own row, measurementType "Voyage" (asked for by the
# OBIS Australia data manager, 2026-11-04). measurementTypeID is left
# blank: NERC has no eMoF-style term for a voyage ID (the nearest, W01
# "Cruise Name", is a SensorML vocabulary, not meant for this).
#
# Returns NULL when neither a vessel nor a voyage is given - then no eMoF
# file is written.

EMOF_PLATFORM_TYPE    <- "Sampling platform name"
EMOF_PLATFORM_TYPE_ID <- "http://vocab.nerc.ac.uk/collection/Q01/current/Q0100001/"
EMOF_VOYAGE_TYPE      <- "Voyage"

#' @param event_ids  eventIDs of the Event core
#' @param vessel_name e.g. "RV Investigator"; NA/blank = no vessel row
#' @param vessel_uri  optional NERC C17 platform URI for the vessel
#' @param voyage_id   optional voyage / cruise ID, e.g. "IN2024_V04"; NA/blank = no voyage row
build_emof <- function(event_ids, vessel_name, vessel_uri = NA_character_, voyage_id = NA_character_) {
  blank <- function(x) is.null(x) || length(x) == 0 || is.na(x) || !nzchar(trimws(x))
  if ((blank(vessel_name) && blank(voyage_id)) || length(event_ids) == 0) return(NULL)

  emof_rows <- function(type, type_id, value, value_id) data.frame(
    eventID            = event_ids,
    occurrenceID       = NA_character_,
    measurementType    = type,
    measurementTypeID  = type_id,
    measurementValue   = value,
    measurementValueID = value_id,
    measurementUnit    = NA_character_,
    measurementUnitID  = NA_character_,
    # No measurementRemarks column: it was always empty once the voyage got
    # its own row, so it was dropped (2026-11-04).
    stringsAsFactors   = FALSE
  )

  parts <- list()
  if (!blank(vessel_name)) {
    parts$vessel <- emof_rows(EMOF_PLATFORM_TYPE, EMOF_PLATFORM_TYPE_ID, trimws(vessel_name),
                              if (blank(vessel_uri)) NA_character_ else trimws(vessel_uri))
  }
  if (!blank(voyage_id)) {
    parts$voyage <- emof_rows(EMOF_VOYAGE_TYPE, NA_character_, trimws(voyage_id), NA_character_)
  }
  out <- do.call(rbind, unname(parts))
  # Keep each sample's rows together (vessel, then voyage).
  out <- out[order(match(out$eventID, event_ids)), , drop = FALSE]
  rownames(out) <- NULL
  out
}

# A methods sentence for eml.xml, so the vessel/voyage also shows in the
# dataset description. NULL when neither is given.
platform_method_note <- function(vessel_name, voyage_id = NA_character_) {
  blank <- function(x) is.null(x) || length(x) == 0 || is.na(x) || !nzchar(trimws(x))
  if (blank(vessel_name) && blank(voyage_id)) return(NULL)
  paste0(
    "Samples were collected",
    if (!blank(vessel_name)) paste0(" aboard ", trimws(vessel_name)) else "",
    if (!blank(voyage_id)) paste0(" during voyage ", trimws(voyage_id)) else "",
    ". ",
    if (!blank(vessel_name) && !blank(voyage_id)) "The vessel and voyage are" else if (!blank(vessel_name)) "The vessel is" else "The voyage is",
    " also recorded for every sample in the extendedMeasurementOrFact extension."
  )
}
