### 01_build_event_core.R
#
# CLI wrapper around build_event_core() (R/build_event_core.R). Builds
# a single Darwin Core Event core table from the sampleMetadata sheet
# shared across this project's FAIRe assay files.
#
# Because all three assays sequence the *same physical samples*, the
# sampleMetadata sheet should be identical across files. build_event_core():
#   1. Reads sampleMetadata from every file in INPUT_FILES
#   2. Cross-checks that they agree (stops with a clear error if not -
#      silently trusting mismatched metadata would be worse than failing)
#   3. Builds eventID from samp_name
#   4. Splits into: real-sample events (-> Event core) vs control-sample
#      events (-> controls/ reference file, NOT part of the mapped archive)
#   5. Maps FAIRe/MIxS field names to Darwin Core Event/Location terms
#
# This script then writes output/event_core/Event.csv.
#
# NOTE on eventID: samp_name is used as eventID as-is (e.g.
# "OcOm_2408_1_1"), since project_id is already embedded in samp_name
# and it was already confirmed globally unique across all 628 samples
# in all 3 assay files. If you'd rather prefix it differently, change
# build_event_id() in R/build_event_core.R.
#
# Field mapping notes (see build_event_core() for the actual code):
#   samp_name          -> (source for) eventID
#   eventDate          -> eventDate            (already DwC-named in FAIRe)
#   decimalLatitude/Longitude -> decimalLatitude/decimalLongitude (already DwC-named)
#   env_broad_scale / env_local_scale / env_medium -> NOT in the Event core;
#                       published in the DNA Derived Data extension instead
#   verbatimEventDate + verbatimEventTime -> merged into verbatimEventDate
#   habitat_natural_artificial_0_1 -> habitat ("natural" for 0, "artificial" for 1)
#   minimumDepthInMeters / maximumDepthInMeters -> kept as-is
#   samp_collect_method / samp_collect_device -> samplingProtocol
#   samp_size / samp_size_unit -> sampleSizeValue / sampleSizeUnit
#   site_id -> locationID
#   geo_loc_name -> locality
#   waterTemperature/salinity/ph/dissolvedOxygen/turbidity/tidal_stage/
#     water_current/samp_weather -> dropped when entirely empty; a warning is
#     logged if populated (they'd need an eMoF extension)
#   verbatimLatitude/Longitude, verbatimCoordinateSystem, verbatimSRS -> kept as verbatim* fields
#
# Audited every other sampleMetadata column (146 total) against actual
# data before deciding what else belongs here: the entire environmental
# chemistry block (nutrients, chlorophyll, wind/light, etc.) and all DNA
# extraction fields (nucl_acid_ext, concentration, samp_vol_we_dna_ext,
# materialSampleID, etc.) are 0/628 populated in this project's FAIRe
# files - not worth adding an eMoF extension or DNA-extension fields for
# columns that are entirely empty. Re-check this if reusing the pipeline
# for a project that actually fills those in.
#
# Anything not mapped here is left out of the Event core for now; if you
# want to keep it, add it to build_event_core() or route it to eMoF if
# it's a measurement.

library(readxl)
library(dplyr)
library(tibble)

source("R/faire_io.R")
source("R/sample_split.R")
source("R/build_event_core.R")

result <- build_event_core(
  input_files             = INPUT_FILES,
  event_core_source_assay = EVENT_CORE_SOURCE_ASSAY,
  sample_category_keep    = SAMPLE_CATEGORY_KEEP,
  georeference_sources    = GEOREFERENCE_SOURCES
)

cat(paste(result$messages, collapse = "\n"), "\n")

dir.create(dirname(EVENT_CORE_OUTPUT), recursive = TRUE, showWarnings = FALSE)
dir.create(CONTROLS_OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

write.csv(result$event_core, EVENT_CORE_OUTPUT, row.names = FALSE, na = "")
cat("Written:", EVENT_CORE_OUTPUT, "\n")

controls_out <- file.path(CONTROLS_OUTPUT_DIR, "sample_controls_reference.csv")
write.csv(result$controls, controls_out, row.names = FALSE, na = "")
cat("Written:", controls_out, "(reference only - not part of the mapped Darwin Core archive)\n")
