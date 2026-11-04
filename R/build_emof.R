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
# There is no NERC term for a voyage/cruise ID, so the voyage goes in
# measurementRemarks on the same row (and in the eml.xml methods), rather
# than as a separate row with no vocabulary.
#
# Returns NULL when no vessel is given - then no eMoF file is written.

EMOF_PLATFORM_TYPE    <- "Sampling platform name"
EMOF_PLATFORM_TYPE_ID <- "http://vocab.nerc.ac.uk/collection/Q01/current/Q0100001/"

#' @param event_ids  eventIDs of the Event core (one row each)
#' @param vessel_name e.g. "RV Investigator"; NA/blank = no eMoF
#' @param vessel_uri  optional NERC C17 platform URI for the vessel
#' @param voyage_id   optional voyage / cruise ID, e.g. "IN2024_V04"
build_emof <- function(event_ids, vessel_name, vessel_uri = NA_character_, voyage_id = NA_character_) {
  blank <- function(x) is.null(x) || length(x) == 0 || is.na(x) || !nzchar(trimws(x))
  if (blank(vessel_name) || length(event_ids) == 0) return(NULL)

  remarks <- if (blank(voyage_id)) NA_character_ else paste0("Voyage ", trimws(voyage_id))

  data.frame(
    eventID            = event_ids,
    occurrenceID       = NA_character_,
    measurementType    = EMOF_PLATFORM_TYPE,
    measurementTypeID  = EMOF_PLATFORM_TYPE_ID,
    measurementValue   = trimws(vessel_name),
    measurementValueID = if (blank(vessel_uri)) NA_character_ else trimws(vessel_uri),
    measurementUnit    = NA_character_,
    measurementUnitID  = NA_character_,
    measurementRemarks = remarks,
    stringsAsFactors   = FALSE
  )
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
    if (!blank(vessel_name)) ". The vessel is also recorded for every sample in the extendedMeasurementOrFact extension." else "."
  )
}
