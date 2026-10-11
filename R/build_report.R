### R/build_report.R
#
# Draws the one-page analysis report for a finished archive as a PNG:
# header (project, description, date), headline numbers, charts, and
# summary tables. A PNG (not a PDF) so it displays the same everywhere -
# in the app's Step 7, in a popup from the Draft/Publish tabs, and when
# downloaded. Built with ggplot2 + base grid only (no rmarkdown/LaTeX,
# which the hosting environment doesn't have).

REPORT_COLORS <- list(
  navy = "#04182f", blue = "#0a3d62", teal = "#0f7a8c", accent = "#2a78d6",
  light = "#f3f6f8", line = "#dfe4e8", text = "#1f2d3a", muted = "#6b7885", grey = "#c3c2b7"
)

.rank_group <- function(rank) {
  r <- tolower(as.character(rank))
  dplyr::case_when(
    is.na(r) ~ "Unidentified",
    r == "species" ~ "Species",
    r == "genus" ~ "Genus",
    r == "family" ~ "Family",
    r == "order" ~ "Order",
    grepl("^not applicable", r) ~ "Unidentified",
    TRUE ~ "Class or higher"
  )
}
RANK_LEVELS <- c("Species", "Genus", "Family", "Order", "Class or higher", "Unidentified")
RANK_COLORS <- c(Species = "#0a3d62", Genus = "#2a78d6", Family = "#0f7a8c",
                 Order = "#5fb3a8", `Class or higher` = "#a9c9e6", Unidentified = "#c3c2b7")

.report_theme <- function() {
  ggplot2::theme_minimal(base_size = 9) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 10.5, color = REPORT_COLORS$blue),
      plot.subtitle = ggplot2::element_text(size = 8, color = REPORT_COLORS$muted),
      plot.background = ggplot2::element_rect(fill = "white", color = REPORT_COLORS$line, linewidth = 0.6),
      plot.margin = ggplot2::margin(8, 10, 6, 8),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      legend.position = "none",
      text = ggplot2::element_text(color = REPORT_COLORS$text)
    )
}

.shorten <- function(x, n = 26) ifelse(nchar(x) > n, paste0(substr(x, 1, n - 1), "..."), x)

.chart_samples <- function(n_real, n_controls) {
  d <- data.frame(category = factor(c("Real samples", "Controls (excluded)"), levels = c("Real samples", "Controls (excluded)")),
                  n = c(n_real, n_controls), real = c(TRUE, FALSE))
  ggplot2::ggplot(d, ggplot2::aes(category, n, fill = real)) +
    ggplot2::geom_col(width = 0.55) +
    ggplot2::geom_text(ggplot2::aes(label = format(n, big.mark = ",")), vjust = -0.5, fontface = "bold", size = 3.4) +
    ggplot2::scale_fill_manual(values = c(`TRUE` = REPORT_COLORS$accent, `FALSE` = REPORT_COLORS$grey)) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.18))) +
    ggplot2::labs(title = "Samples in the archive") +
    .report_theme()
}

.chart_detections <- function(det_df) {
  ggplot2::ggplot(det_df, ggplot2::aes(assay, n)) +
    ggplot2::geom_col(fill = REPORT_COLORS$teal, width = 0.55) +
    ggplot2::geom_text(ggplot2::aes(label = format(n, big.mark = ",")), vjust = -0.5, fontface = "bold", size = 3.4) +
    ggplot2::scale_y_continuous(labels = scales::comma, expand = ggplot2::expansion(mult = c(0, 0.18))) +
    ggplot2::labs(title = "Detections per assay") +
    .report_theme()
}

.chart_resolution <- function(occurrence_tables) {
  d <- dplyr::bind_rows(lapply(names(occurrence_tables), function(a) {
    tibble::tibble(assay = a, group = .rank_group(occurrence_tables[[a]]$taxonRank))
  })) %>%
    dplyr::count(assay, group) %>%
    dplyr::group_by(assay) %>%
    dplyr::mutate(pct = n / sum(n)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(group = factor(group, levels = rev(RANK_LEVELS)))
  d$assay <- factor(d$assay, levels = rev(names(occurrence_tables)))

  ggplot2::ggplot(d, ggplot2::aes(pct, assay, fill = group)) +
    ggplot2::geom_col(width = 0.6, color = "white", linewidth = 0.3) +
    ggplot2::geom_text(ggplot2::aes(label = ifelse(pct >= 0.06, paste0(round(pct * 100), "%"), "")),
                       position = ggplot2::position_stack(vjust = 0.5), size = 3, color = "white", fontface = "bold") +
    ggplot2::scale_fill_manual(values = RANK_COLORS, breaks = RANK_LEVELS, drop = FALSE) +
    ggplot2::scale_x_continuous(labels = scales::percent, expand = ggplot2::expansion(mult = c(0, 0.01))) +
    ggplot2::labs(title = "How far each detection was identified", subtitle = "Share of detections by the taxonomic rank they resolved to") +
    .report_theme() +
    ggplot2::theme(legend.position = "bottom", legend.title = ggplot2::element_blank(),
                   legend.key.size = ggplot2::unit(0.32, "cm"), legend.text = ggplot2::element_text(size = 7.5),
                   panel.grid.major.x = ggplot2::element_line(color = REPORT_COLORS$line, linewidth = 0.3),
                   panel.grid.major.y = ggplot2::element_blank())
}

.chart_top_taxa <- function(occurrence_tables) {
  d <- dplyr::bind_rows(lapply(occurrence_tables, function(x) x[, c("scientificName", "organismQuantity")])) %>%
    dplyr::filter(!is.na(scientificName), scientificName != "Biota incertae sedis") %>%
    dplyr::group_by(scientificName) %>%
    dplyr::summarise(reads = sum(as.numeric(organismQuantity), na.rm = TRUE), .groups = "drop") %>%
    dplyr::arrange(dplyr::desc(reads)) %>%
    utils::head(10)
  d$label <- factor(.shorten(d$scientificName), levels = rev(.shorten(d$scientificName)))

  ggplot2::ggplot(d, ggplot2::aes(reads, label)) +
    ggplot2::geom_col(fill = REPORT_COLORS$accent, width = 0.65) +
    ggplot2::scale_x_continuous(labels = scales::label_number(scale_cut = scales::cut_short_scale()), expand = ggplot2::expansion(mult = c(0, 0.05))) +
    ggplot2::labs(title = "Ten most-detected taxa", subtitle = "Total DNA sequence reads, all assays combined") +
    .report_theme() +
    ggplot2::theme(axis.text.y = ggplot2::element_text(face = "italic", size = 7.5),
                   panel.grid.major.x = ggplot2::element_line(color = REPORT_COLORS$line, linewidth = 0.3),
                   panel.grid.major.y = ggplot2::element_blank())
}

.chart_map <- function(event_core, width_in, height_in) {
  pts <- event_core %>%
    dplyr::transmute(lon = as.numeric(decimalLongitude), lat = as.numeric(decimalLatitude)) %>%
    dplyr::filter(!is.na(lon), !is.na(lat)) %>%
    dplyr::distinct()
  pad <- max(2, 0.15 * max(diff(range(pts$lon)), diff(range(pts$lat))))
  xl <- range(pts$lon) + c(-pad, pad); yl <- range(pts$lat) + c(-pad, pad)

  # Widen the shorter axis so the map's true proportions fill the card (a fixed-aspect
  # coord would shrink the plot and leave the card half empty).
  target <- (height_in - 0.9) / (width_in - 0.55)                 # panel height / width, inches
  mid_lat <- mean(yl); dx <- diff(xl); dy <- diff(yl)
  ratio <- dy / (dx * cos(mid_lat * pi / 180))
  if (ratio > target) { need <- dy / (target * cos(mid_lat * pi / 180)); xl <- mean(xl) + c(-1, 1) * need / 2 }
  else                { need <- target * dx * cos(mid_lat * pi / 180); yl <- mean(yl) + c(-1, 1) * need / 2 }

  p <- ggplot2::ggplot()
  # ggplot2::map_data() needs the maps package; naming it here also lets the
  # deploy step see and install it. Without it the map still draws, minus coastline.
  world <- if (requireNamespace("maps", quietly = TRUE)) tryCatch(ggplot2::map_data("world"), error = function(e) NULL) else NULL
  if (!is.null(world)) {
    p <- p + ggplot2::geom_polygon(data = world, ggplot2::aes(long, lat, group = group), fill = "#e7ebee", color = "#c9d0d6", linewidth = 0.2)
  }
  p +
    ggplot2::geom_point(data = pts, ggplot2::aes(lon, lat), color = REPORT_COLORS$teal, alpha = 0.55, size = 1.6) +
    ggplot2::coord_cartesian(xlim = xl, ylim = yl, expand = FALSE) +
    ggplot2::labs(title = "Sampling locations", subtitle = paste0(format(nrow(pts), big.mark = ","), " distinct positions")) +
    .report_theme() +
    ggplot2::theme(panel.grid.major = ggplot2::element_line(color = REPORT_COLORS$line, linewidth = 0.25),
                   axis.text = ggplot2::element_text(size = 7))
}

# Small key/value table drawn with grid, inside the viewport it is called from.
.draw_table <- function(df, W_in, title, row_h = 0.27) {
  grid::grid.roundrect(x = 0, y = 1, width = 1, height = 1, just = c("left", "top"), r = grid::unit(0.05, "in"),
                       gp = grid::gpar(fill = "white", col = REPORT_COLORS$line))
  grid::grid.text(title, x = grid::unit(0.12, "in"), y = grid::unit(1, "npc") - grid::unit(0.14, "in"), just = c("left", "top"),
                  gp = grid::gpar(fontface = "bold", fontsize = 10.5, col = REPORT_COLORS$blue))
  top <- 0.5
  for (i in seq_len(nrow(df))) {
    y <- top + (i - 1) * row_h
    if (i %% 2 == 1) grid::grid.rect(x = grid::unit(0.06, "in"), y = grid::unit(1, "npc") - grid::unit(y, "in"), width = grid::unit(W_in - 0.12, "in"),
                                     height = grid::unit(row_h, "in"), just = c("left", "top"), gp = grid::gpar(fill = REPORT_COLORS$light, col = NA))
    grid::grid.text(df[[1]][i], x = grid::unit(0.14, "in"), y = grid::unit(1, "npc") - grid::unit(y + row_h / 2, "in"), just = c("left", "center"),
                    gp = grid::gpar(fontsize = 8.5, col = REPORT_COLORS$text))
    grid::grid.text(df[[2]][i], x = grid::unit(W_in - 0.14, "in"), y = grid::unit(1, "npc") - grid::unit(y + row_h / 2, "in"), just = c("right", "center"),
                    gp = grid::gpar(fontsize = 8.5, fontface = "bold", col = REPORT_COLORS$blue))
  }
}

#' Render the report to a PNG file.
#'
#' @param path Output .png path
#' @param info list with: project_id, title, description (may be NULL/empty), generated (POSIXct),
#'   reference_db_label, event_core, n_controls, occurrence_tables (post-WoRMS), taxonomy_df
#'   (data.frame Resolution/Names), qc (NULL or list(passed, total, skipped)),
#'   mapping (NULL or list(ok, total, issues))
#' @return path, invisibly
build_report_png <- function(path, info) {
  occ <- info$occurrence_tables
  assays <- names(occ)
  n_real <- nrow(info$event_core)
  det_df <- data.frame(assay = assays, n = vapply(occ, nrow, integer(1)))

  all_names <- unique(unlist(lapply(occ, function(x) x$scientificName)))
  all_names <- all_names[!is.na(all_names)]
  n_taxa <- length(setdiff(all_names, "Biota incertae sedis"))
  id_tbl <- dplyr::bind_rows(lapply(occ, function(x) x[, c("scientificName", "scientificNameID")])) %>% dplyr::distinct()
  pct_id <- if (nrow(id_tbl) > 0) round(100 * mean(!is.na(id_tbl$scientificNameID) & nzchar(id_tbl$scientificNameID))) else 0

  dates <- suppressWarnings(as.Date(substr(as.character(info$event_core$eventDate), 1, 10)))
  date_range <- if (all(is.na(dates))) "not recorded" else {
    r <- range(dates, na.rm = TRUE)
    if (r[1] == r[2]) format(r[1], "%d %b %Y") else paste0(format(r[1], "%d %b %Y"), " - ", format(r[2], "%d %b %Y"))
  }

  W <- 8.27; H <- 14.85; M <- 0.4
  dev_args <- list(filename = path, width = W, height = H, units = "in", res = 200)
  if (isTRUE(capabilities("cairo"))) dev_args$type <- "cairo"
  do.call(grDevices::png, dev_args)
  on.exit(grDevices::dev.off(), add = TRUE)
  grid::grid.newpage()

  vp_at <- function(x, y_top, w, h) grid::viewport(x = grid::unit(x, "in"), y = grid::unit(H - y_top, "in"),
                                                   width = grid::unit(w, "in"), height = grid::unit(h, "in"), just = c("left", "top"))
  txt <- function(label, x, y, size, col = "white", face = "plain", just = c("left", "top")) {
    grid::grid.text(label, x = grid::unit(x, "in"), y = grid::unit(H - y, "in"), just = just,
                    gp = grid::gpar(fontsize = size, col = col, fontface = face))
  }

  # ---- header banner ----
  grid::grid.rect(x = 0, y = 1, width = 1, height = grid::unit(1.75, "in"), just = c("left", "top"), gp = grid::gpar(fill = REPORT_COLORS$blue, col = NA))
  grid::grid.rect(x = 0, y = grid::unit(H - 1.75, "in"), width = 1, height = grid::unit(0.07, "in"), just = c("left", "top"), gp = grid::gpar(fill = REPORT_COLORS$teal, col = NA))
  txt("eDNA PUBLICATION REPORT", M, 0.28, 9, "#9fd6dc", "bold")
  title_txt <- if (nzchar(info$title %||% "")) info$title else info$project_id
  if (nchar(title_txt) > 62) title_txt <- paste0(substr(title_txt, 1, 59), "...")
  txt(title_txt, M, if (nchar(title_txt) <= 34) 0.5 else 0.55, if (nchar(title_txt) <= 34) 21 else 16, "white", "bold")
  if (nzchar(info$description %||% "")) {
    lines <- strwrap(gsub("\\s+", " ", info$description), width = 112)
    if (length(lines) > 3) lines <- c(lines[1:2], paste0(lines[3], "..."))
    txt(paste(lines, collapse = "\n"), M, 1.0, 8.5, "#dbe9ef")
  }
  txt(paste0("Project ", info$project_id, "   |   ", format(info$generated, "%d %B %Y, %H:%M", tz = "UTC"), " UTC   |   ", info$reference_db_label),
      M, 1.53, 8, "#9fd6dc")

  # ---- headline numbers ----
  kpis <- list(
    list(format(n_real, big.mark = ","), "real samples"),
    list(format(info$n_controls, big.mark = ","), "controls excluded"),
    list(length(assays), if (length(assays) == 1) "assay" else "assays"),
    list(format(sum(det_df$n), big.mark = ","), "detections"),
    list(format(n_taxa, big.mark = ","), "identified taxa"),
    list(paste0(pct_id, "%"), "names with WoRMS ID")
  )
  tile_w <- (W - 2 * M - 5 * 0.12) / 6
  for (i in seq_along(kpis)) {
    x <- M + (i - 1) * (tile_w + 0.12); y <- 1.98
    grid::grid.roundrect(x = grid::unit(x, "in"), y = grid::unit(H - y, "in"), width = grid::unit(tile_w, "in"), height = grid::unit(0.85, "in"),
                         just = c("left", "top"), r = grid::unit(0.06, "in"), gp = grid::gpar(fill = REPORT_COLORS$light, col = REPORT_COLORS$line))
    txt(kpis[[i]][[1]], x + tile_w / 2, y + 0.14, 19, REPORT_COLORS$blue, "bold", c("center", "top"))
    txt(kpis[[i]][[2]], x + tile_w / 2, y + 0.60, 7.5, REPORT_COLORS$muted, "plain", c("center", "top"))
  }
  txt(paste0("Sampling period: ", date_range), M, 2.93, 8.5, REPORT_COLORS$muted)

  # ---- charts ----
  half <- (W - 2 * M - 0.15)
  print(.chart_samples(n_real, info$n_controls), vp = vp_at(M, 3.2, half * 0.42, 2.55))
  print(.chart_detections(det_df), vp = vp_at(M + half * 0.42 + 0.15, 3.2, half * 0.58, 2.55))
  print(.chart_resolution(occ), vp = vp_at(M, 5.9, W - 2 * M, 2.75))
  print(.chart_top_taxa(occ), vp = vp_at(M, 8.8, half * 0.52, 3.1))
  print(.chart_map(info$event_core, half * 0.48, 3.1), vp = vp_at(M + half * 0.52 + 0.15, 8.8, half * 0.48, 3.1))

  # ---- summary tables ----
  tw <- (W - 2 * M - 0.15) / 2
  tax <- info$taxonomy_df
  tax_rows <- data.frame(a = tax$Resolution, b = format(tax$Names, big.mark = ","), stringsAsFactors = FALSE)

  q <- data.frame(a = character(), b = character(), stringsAsFactors = FALSE)
  add_q <- function(a, b) q[nrow(q) + 1, ] <<- c(a, b)
  if (!is.null(info$mapping)) {
    add_q("Columns mapping cleanly to Darwin Core", paste0(info$mapping$ok, " / ", info$mapping$total))
    add_q("Columns needing attention", as.character(info$mapping$issues))
  }
  if (!is.null(info$qc)) {
    add_q("OBIS QC checks passed", paste0(info$qc$passed, " / ", info$qc$total))
    if (info$qc$skipped > 0) add_q("QC checks skipped (not verified)", as.character(info$qc$skipped))
  } else add_q("OBIS QC checks", "not run")
  add_q("Assays combined in one Event core", as.character(length(assays)))
  add_q("Names with a WoRMS ID", paste0(pct_id, "%"))

  table_h <- 0.55 + 0.27 * max(nrow(tax_rows), nrow(q))
  grid::pushViewport(vp_at(M, 12.05, tw, table_h)); .draw_table(tax_rows, tw, "Taxonomy matching (WoRMS)"); grid::popViewport()
  grid::pushViewport(vp_at(M + tw + 0.15, 12.05, tw, table_h)); .draw_table(q, tw, "Quality checks"); grid::popViewport()

  # ---- footer ----
  grid::grid.lines(x = grid::unit(c(M, W - M), "in"), y = grid::unit(H - (H - 0.42), "in"), gp = grid::gpar(col = REPORT_COLORS$line))
  txt("Generated by MAT  |  Minderoo OceanOmics Centre at UWA", M, H - 0.34, 7.5, REPORT_COLORS$muted)
  txt(paste0("Project ", info$project_id), W - M, H - 0.34, 7.5, REPORT_COLORS$muted, "plain", c("right", "top"))

  invisible(path)
}
