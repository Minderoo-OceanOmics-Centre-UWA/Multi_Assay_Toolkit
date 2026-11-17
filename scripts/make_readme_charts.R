### make_readme_charts.R
#
# Generates the README's chart images (docs/img/) from the pipeline's real
# output files (output/event_core, output/occurrence, output/controls) -
# the same OcOm_2408 run documented throughout the README.
#
# Deliberately reuses R/build_report.R's own chart-drawing functions
# (.chart_samples, .chart_detections, .chart_resolution, .chart_top_taxa,
# .chart_map) rather than a separate ggplot recipe, so the README shows
# the SAME chart style the app's Step 7 analysis report produces - not a
# look-alike that can drift from it. Run manually after re-running the
# pipeline on new/updated data, to refresh these images.
#
# Not part of the conversion pipeline itself.

suppressMessages({
  library(ggplot2)
  library(dplyr)
  library(tibble)
})

source("R/build_report.R")  # for REPORT_COLORS, .report_theme(), .chart_*()

dir.create("docs/img", recursive = TRUE, showWarnings = FALSE)

event_core <- read.csv("output/event_core/Event.csv", stringsAsFactors = FALSE)
n_controls <- nrow(read.csv("output/controls/sample_controls_reference.csv", stringsAsFactors = FALSE))
n_real     <- nrow(event_core)

assays <- c("16SFishD", "MarVer1", "MiFishUE2")
occurrence_tables <- setNames(
  lapply(assays, function(a) read.csv(file.path("output/occurrence", paste0("Occurrence_", a, ".csv")), stringsAsFactors = FALSE)),
  assays
)
det_df <- data.frame(assay = assays, n = vapply(occurrence_tables, nrow, integer(1)))

save_chart <- function(plot, file, width, height) {
  ggsave(file.path("docs/img", file), plot, width = width, height = height, dpi = 200, bg = "white")
  cat("Written docs/img/", file, "\n", sep = "")
}

save_chart(.chart_samples(n_real, n_controls), "samples_breakdown.png", width = 6.2, height = 4.2)
save_chart(.chart_detections(det_df), "detections_per_assay.png", width = 6.2, height = 4.2)
save_chart(.chart_resolution(occurrence_tables), "taxonomic_resolution.png", width = 8.2, height = 4.0)
save_chart(.chart_top_taxa(occurrence_tables), "top_taxa.png", width = 6.6, height = 4.4)
save_chart(.chart_map(event_core, width_in = 6.6, height_in = 4.2), "sampling_map.png", width = 6.6, height = 4.2)
