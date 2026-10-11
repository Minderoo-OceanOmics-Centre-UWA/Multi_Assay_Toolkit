### R/taxon_lookup.R
#
# In-app name checker for the Taxonomy Review step: look a name or an
# AphiaID up and show it without leaving the app. Tabs:
#   WoRMS      - the record our matching uses (worrms API, shown natively)
#   OBIS       - the habitat flags OBIS itself stores for the taxon, what its
#                QC will do with it, and how many OBIS records exist
#   FishBase / SeaLifeBase - embedded species page (FishBase for fish,
#                SeaLifeBase for other animals - FishBase only covers fish)
#   GBIF       - classification, common name and habitat for ANY organism
#   Wikipedia  - short description and photo
#
# What can be embedded: WoRMS and IRMNG block framing (CSP frame-ancestors
# only allows VLIZ domains) and gbif.org returns 403 to non-browsers, so
# those are fetched through their public APIs and shown natively.
# FishBase, SeaLifeBase and Wikipedia allow framing. No API keys needed.

worms_taxon_url <- function(aphia_id) {
  paste0("https://www.marinespecies.org/aphia.php?p=taxdetails&id=", aphia_id)
}

fishbase_species_url <- function(species_name, site = "fishbase") {
  if (is.na(species_name) || !grepl("^[A-Z][a-z-]+ [a-z-]+$", species_name)) return(NA_character_)
  paste0("https://www.", site, ".se/summary/", sub(" ", "-", species_name), ".html")
}

# WoRMS classification names that mean "this is a fish" (FishBase covers
# these; every other animal is in SeaLifeBase instead).
FISH_GROUPS <- c("Actinopterygii", "Teleostei", "Chondrichthyes", "Elasmobranchii", "Holocephali",
                 "Myxini", "Petromyzonti", "Coelacanthi", "Dipneusti", "Sarcopterygii")

#' GET a JSON API with a short timeout. NULL on any failure - a slow or
#' down external service must never break the popup.
.get_json <- function(url, timeout = 10) {
  h <- curl::new_handle(timeout = timeout, useragent = "MAT (Minderoo OceanOmics Centre, UWA)")
  res <- tryCatch(curl::curl_fetch_memory(url, handle = h), error = function(e) NULL)
  if (is.null(res) || res$status_code != 200) return(NULL)
  txt <- rawToChar(res$content)
  Encoding(txt) <- "UTF-8"
  tryCatch(jsonlite::fromJSON(txt, simplifyVector = TRUE), error = function(e) NULL)
}

#' @param query A scientific name, or an AphiaID (digits only)
#' @return list(records = data.frame or NULL, best = 1-row data.frame or NULL,
#'   classification = tibble or NULL, fuzzy = logical, note = character)
lookup_taxon <- function(query) {
  query <- trimws(query)
  if (!nzchar(query)) return(list(records = NULL, note = "Type a scientific name or an AphiaID."))

  fuzzy <- FALSE
  if (grepl("^[0-9]+$", query)) {
    records <- tryCatch(worrms::wm_record(as.integer(query)), error = function(e) NULL)
  } else {
    records <- tryCatch(worrms::wm_records_name(query, fuzzy = FALSE, marine_only = FALSE), error = function(e) NULL)
    if (is.null(records) || nrow(records) == 0) {
      records <- tryCatch(worrms::wm_records_name(query, fuzzy = TRUE, marine_only = FALSE), error = function(e) NULL)
      fuzzy <- TRUE
    }
  }
  if (is.null(records) || nrow(records) == 0) {
    return(list(records = NULL, note = paste0("WoRMS has no record for \"", query, "\".")))
  }

  records <- as.data.frame(records)
  best <- records[order(records$status != "accepted"), ][1, , drop = FALSE]
  classification <- tryCatch(worrms::wm_classification(best$AphiaID), error = function(e) NULL)

  list(records = records, best = best, classification = classification, fuzzy = fuzzy, note = NULL)
}

#' OBIS's own view of a taxon (by AphiaID - OBIS uses WoRMS IDs).
#' @return list(taxon, records, dropped) or NULL
lookup_obis <- function(aphia_id) {
  if (is.null(aphia_id) || is.na(aphia_id)) return(NULL)
  taxon <- .get_json(paste0("https://api.obis.org/v3/taxon/", aphia_id))
  if (is.null(taxon) || length(taxon$results) == 0 || NROW(taxon$results) == 0) return(NULL)
  stats   <- .get_json(paste0("https://api.obis.org/v3/statistics?taxonid=", aphia_id))
  dropped <- .get_json(paste0("https://api.obis.org/v3/occurrence?taxonid=", aphia_id, "&dropped=true&size=1"))
  list(taxon = as.list(taxon$results[1, , drop = FALSE]),
       records = if (is.null(stats)) NA else stats$records,
       dropped = if (is.null(dropped)) NA else dropped$total)
}

#' Same rule as OBIS's QC (iobis/obis-qc, obisqc/taxonomy.py).
#' @return list(label, class, reason)
obis_verdict <- function(is_marine, is_brackish) {
  val <- function(x) if (is.null(x) || length(x) == 0 || is.na(x)) NA else as.logical(x)
  m <- val(is_marine); b <- val(is_brackish)
  if (isFALSE(m) && isFALSE(b)) {
    list(label = "Will be dropped", class = "bg-danger",
         reason = "WoRMS says this taxon is neither marine nor brackish, so OBIS's quality check (NOT_MARINE) drops these records. They stay in the dataset download but are hidden from OBIS searches.")
  } else if (!isTRUE(m) && !isTRUE(b)) {
    list(label = "Kept - marked unsure", class = "bg-warning text-dark",
         reason = "WoRMS doesn't record this taxon as marine or brackish, but doesn't say it isn't either. OBIS publishes the records with a MARINE_UNSURE flag.")
  } else {
    list(label = "Kept", class = "bg-success",
         reason = paste0("WoRMS records this taxon as ", if (isTRUE(m)) "marine" else "brackish", ", so OBIS publishes it normally."))
  }
}

#' GBIF backbone match for any organism, plus English name and habitat.
#' @return list(match, common_name, habitat) or NULL
lookup_gbif <- function(name) {
  if (is.null(name) || is.na(name) || !nzchar(name)) return(NULL)
  m <- .get_json(paste0("https://api.gbif.org/v1/species/match?verbose=false&name=", utils::URLencode(name, reserved = TRUE)))
  if (is.null(m) || is.null(m$usageKey)) return(NULL)

  vern <- .get_json(paste0("https://api.gbif.org/v1/species/", m$usageKey, "/vernacularNames?limit=100"))
  common <- NA_character_
  if (!is.null(vern) && NROW(vern$results) > 0 && "language" %in% names(vern$results)) {
    eng <- vern$results$vernacularName[vern$results$language %in% "eng"]
    if (length(eng)) common <- names(sort(table(eng), decreasing = TRUE))[1]
  }

  prof <- .get_json(paste0("https://api.gbif.org/v1/species/", m$usageKey, "/speciesProfiles?limit=100"))
  habitat <- NULL
  if (!is.null(prof) && NROW(prof$results) > 0) {
    p <- prof$results
    habitat <- vapply(c(Marine = "marine", Freshwater = "freshwater", Terrestrial = "terrestrial"), function(col) {
      v <- if (col %in% names(p)) p[[col]] else logical()
      paste0(sum(v %in% TRUE), " yes / ", sum(v %in% FALSE), " no")
    }, character(1))
  }
  list(match = m, common_name = common, habitat = habitat)
}

#' Wikipedia page summary (English).
#' @return list(title, description, extract, image, url) or NULL
lookup_wikipedia <- function(name) {
  if (is.null(name) || is.na(name) || !nzchar(name)) return(NULL)
  w <- .get_json(paste0("https://en.wikipedia.org/api/rest_v1/page/summary/",
                        utils::URLencode(gsub(" ", "_", name), reserved = TRUE)))
  if (is.null(w) || identical(w$type, "disambiguation") || is.null(w$extract)) return(NULL)
  list(title = w$title, description = w$description %||% "", extract = w$extract,
       image = w$thumbnail$source %||% NA_character_, url = w$content_urls$desktop$page %||% NA_character_)
}

#' Everything the popup shows. progress: optional function(detail) called
#' before each slow step, so the caller can update a progress bar.
#' extended = FALSE fetches WoRMS only (the search box's WoRMS + FishBase
#' popup); TRUE also fetches OBIS, GBIF and Wikipedia (the Check buttons).
lookup_taxon_all <- function(query, extended = TRUE, progress = function(detail) NULL) {
  progress("WoRMS")
  res <- lookup_taxon(query)
  res$extended <- extended
  if (!extended) return(res)
  name <- if (!is.null(res$best)) res$best$valid_name %||% res$best$scientificname else trimws(query)
  if (grepl("^[0-9]+$", name)) name <- NA_character_
  aphia <- if (!is.null(res$best)) res$best$valid_AphiaID %||% res$best$AphiaID else NA

  progress("OBIS");      res$obis <- lookup_obis(aphia)
  progress("GBIF");      res$gbif <- lookup_gbif(name)
  progress("Wikipedia"); res$wiki <- lookup_wikipedia(name)
  res$name <- name
  res
}

.habitat_badges <- function(rec) {
  flags <- c(Marine = rec$isMarine, Brackish = rec$isBrackish, Freshwater = rec$isFreshwater, Terrestrial = rec$isTerrestrial)
  on <- names(flags)[!is.na(flags) & flags == 1]
  lapply(on, function(x) span(class = "badge bg-info-subtle text-info-emphasis me-1", x))
}

.status_badge <- function(status) {
  cls <- if (identical(status, "accepted")) "bg-success" else "bg-warning text-dark"
  span(class = paste("badge", cls), status)
}

.ext_link <- function(href, label, class = NULL) {
  tags$a(href = href, target = "_blank", rel = "noopener noreferrer", class = class, label)
}

.unavailable <- function(what, link = NULL) {
  div(class = "alert alert-secondary", what, if (!is.null(link)) tagList(" ", link))
}

.worms_tab <- function(res, query) {
  if (is.null(res$records)) {
    return(div(class = "alert alert-warning", res$note, " Check the spelling, or search WoRMS directly: ",
               .ext_link(paste0("https://www.marinespecies.org/aphia.php?p=taxlist&tName=", utils::URLencode(query, reserved = TRUE)),
                         "open WoRMS search")))
  }
  recs <- utils::head(res$records, 25)
  tagList(
    if (isTRUE(res$fuzzy)) div(class = "alert alert-warning py-2", "No exact match - these are WoRMS' closest (fuzzy) matches. Verify before using one."),
    if (!is.null(res$classification)) {
      div(class = "mb-3",
          div(class = "muted", "Classification of the top record"),
          div(paste(res$classification$scientificname, collapse = "  ›  ")))
    },
    tags$table(class = "table table-sm align-middle",
      tags$thead(tags$tr(tags$th("AphiaID"), tags$th("Name"), tags$th("Status"), tags$th("Rank"), tags$th("Habitat"), tags$th("Accepted name"), tags$th())),
      tags$tbody(lapply(seq_len(nrow(recs)), function(i) {
        r <- recs[i, ]
        tags$tr(
          tags$td(tags$code(r$AphiaID)),
          tags$td(tags$em(r$scientificname), " ", span(class = "muted", r$authority)),
          tags$td(.status_badge(r$status)),
          tags$td(r$rank),
          tags$td(.habitat_badges(r)),
          tags$td(if (!identical(r$status, "accepted")) paste0(r$valid_name, " (", r$valid_AphiaID, ")") else ""),
          tags$td(.ext_link(worms_taxon_url(r$AphiaID), "WoRMS ↗"))
        )
      }))
    ),
    if (nrow(res$records) > nrow(recs)) p(class = "muted", paste0("Showing the first ", nrow(recs), " of ", nrow(res$records), " records."))
  )
}

.obis_tab <- function(res) {
  o <- res$obis
  if (is.null(o)) {
    return(.unavailable(if (is.null(res$best)) "OBIS uses WoRMS IDs, and this name has no WoRMS record - so OBIS can't match it at all."
                        else "OBIS has no information on this taxon, or OBIS couldn't be reached."))
  }
  t <- o$taxon
  v <- obis_verdict(t$is_marine, t$is_brackish)
  flag <- function(x) {
    if (is.null(x) || length(x) == 0 || is.na(x)) span(class = "muted", "not set")
    else if (isTRUE(as.logical(x))) span(class = "badge bg-info-subtle text-info-emphasis", "yes")
    else span(class = "badge bg-secondary-subtle text-secondary-emphasis", "no")
  }
  tagList(
    div(class = "d-flex align-items-center gap-2 mb-2", strong("What OBIS will do:"), span(class = paste("badge fs-6", v$class), v$label)),
    p(v$reason),
    tags$table(class = "table table-sm w-auto",
      tags$thead(tags$tr(tags$th("OBIS habitat flag"), tags$th("Value"))),
      tags$tbody(
        tags$tr(tags$td("Marine"), tags$td(flag(t$is_marine))),
        tags$tr(tags$td("Brackish"), tags$td(flag(t$is_brackish))),
        tags$tr(tags$td("Freshwater"), tags$td(flag(t$is_freshwater))),
        tags$tr(tags$td("Terrestrial"), tags$td(flag(t$is_terrestrial)))
      )),
    p(class = "muted", "OBIS only drops a taxon when Marine and Brackish are both \"no\". \"Not set\" isn't the same as \"no\"."),
    p(strong("Already in OBIS: "),
      if (is.na(o$records)) "unknown" else paste0(format(o$records, big.mark = ","), " published record(s)"),
      if (!is.na(o$dropped) && o$dropped > 0) paste0(", ", format(o$dropped, big.mark = ","), " dropped")),
    .ext_link(paste0("https://obis.org/taxon/", t$taxonID), "Open in OBIS ↗", class = "btn btn-sm btn-outline-secondary")
  )
}

.species_page_tab <- function(res) {
  cls <- if (!is.null(res$classification)) res$classification$scientificname else character()
  is_fish <- any(cls %in% FISH_GROUPS)
  is_animal <- "Animalia" %in% cls
  site <- if (is_fish) "fishbase" else "sealifebase"
  site_label <- if (is_fish) "FishBase" else "SeaLifeBase"
  sp <- if (!is.null(res$best) && identical(res$best$rank, "Species")) res$best$valid_name else NA_character_
  url <- fishbase_species_url(sp, site)

  if (!is.null(res$best) && !is_fish && !is_animal) {
    return(.unavailable("FishBase covers fish and SeaLifeBase covers other animals. WoRMS doesn't place this taxon in the animal kingdom, so neither has a page for it. Use the GBIF and Wikipedia tabs."))
  }
  if (is.na(url)) {
    return(.unavailable(paste0(site_label, " has pages for species only, and the top WoRMS record isn't a species."),
                        .ext_link(paste0("https://www.", site, ".se/search.php"), paste0("Search ", site_label, " yourself ↗"))))
  }
  tagList(
    div(class = "d-flex justify-content-between align-items-center mb-2",
        span(class = "muted", paste0(site_label, " page for ", sp, ". If it says the species isn't found, ", site_label, " doesn't list it.")),
        .ext_link(url, paste0("Open in ", site_label, " ↗"), class = "btn btn-sm btn-outline-secondary")),
    tags$iframe(src = url, loading = "lazy", referrerpolicy = "no-referrer",
                sandbox = "allow-scripts allow-same-origin allow-popups allow-forms",
                style = "width:100%; height:60vh; border:1px solid #eceae4; border-radius:10px;")
  )
}

.gbif_tab <- function(res) {
  g <- res$gbif
  if (is.null(g)) return(.unavailable("GBIF has no match for this name, or GBIF couldn't be reached."))
  m <- g$match
  ranks <- c("kingdom", "phylum", "class", "order", "family", "genus")
  path <- unlist(lapply(ranks, function(r) m[[r]]))
  tagList(
    div(class = "mb-2", tags$em(m$scientificName %||% m$canonicalName),
        if (!is.na(g$common_name)) tagList(" - ", strong(g$common_name))),
    div(class = "mb-3 muted", paste(path, collapse = "  ›  ")),
    tags$table(class = "table table-sm w-auto",
      tags$tbody(
        tags$tr(tags$td("Rank"), tags$td(tolower(m$rank %||% ""))),
        tags$tr(tags$td("Status"), tags$td(tolower(m$status %||% ""))),
        tags$tr(tags$td("Match"), tags$td(tolower(m$matchType %||% ""),
                                         if (!identical(m$matchType, "EXACT")) span(class = "badge bg-warning text-dark ms-1", "check this is the same taxon")))
      )),
    if (!is.null(g$habitat)) {
      tagList(
        strong("Habitat, as recorded by GBIF's source checklists"),
        tags$table(class = "table table-sm w-auto mt-1",
          tags$tbody(lapply(names(g$habitat), function(h) tags$tr(tags$td(h), tags$td(g$habitat[[h]]))))),
        p(class = "muted", "Counts of checklists saying yes / no. GBIF doesn't filter by habitat - it publishes every record.")
      )
    },
    .ext_link(paste0("https://www.gbif.org/species/", m$usageKey), "Open in GBIF ↗", class = "btn btn-sm btn-outline-secondary")
  )
}

.wikipedia_tab <- function(res) {
  w <- res$wiki
  if (is.null(w)) return(.unavailable("No English Wikipedia article found for this name."))
  div(class = "d-flex gap-3 align-items-start",
      if (!is.na(w$image)) tags$img(src = w$image, alt = w$title, style = "max-width: 220px; border-radius: 8px;"),
      div(
        tags$h5(w$title, if (nzchar(w$description)) span(class = "muted fs-6", paste0(" - ", w$description))),
        p(w$extract),
        if (!is.na(w$url)) .ext_link(w$url, "Read on Wikipedia ↗", class = "btn btn-sm btn-outline-secondary")
      ))
}

#' Body of the "Check name" modal. Without res$extended (the search box),
#' only the WoRMS and FishBase tabs are shown.
taxon_lookup_body <- function(res, query) {
  if (!isTRUE(res$extended)) {
    url <- fishbase_species_url(if (!is.null(res$best) && identical(res$best$rank, "Species")) res$best$valid_name else NA_character_)
    fishbase_tab <- if (is.na(url)) {
      .unavailable("FishBase has pages for fish species only. The top WoRMS record here is not a species, so there is nothing to show.",
                   .ext_link("https://www.fishbase.se/search.php", "Search FishBase yourself ↗"))
    } else {
      tags$iframe(src = url, loading = "lazy", referrerpolicy = "no-referrer",
                  sandbox = "allow-scripts allow-same-origin allow-popups allow-forms",
                  style = "width:100%; height:60vh; border:1px solid #eceae4; border-radius:10px;")
    }
    return(bslib::navset_tab(
      bslib::nav_panel("WoRMS record", div(class = "pt-3", .worms_tab(res, query))),
      bslib::nav_panel("FishBase", div(class = "pt-3", fishbase_tab))
    ))
  }

  cls <- if (!is.null(res$classification)) res$classification$scientificname else character()
  species_tab_label <- if (any(cls %in% FISH_GROUPS) || is.null(res$best)) "FishBase" else "SeaLifeBase"
  bslib::navset_tab(
    bslib::nav_panel("WoRMS record", div(class = "pt-3", .worms_tab(res, query))),
    bslib::nav_panel("OBIS", div(class = "pt-3", .obis_tab(res))),
    bslib::nav_panel(species_tab_label, div(class = "pt-3", .species_page_tab(res))),
    bslib::nav_panel("GBIF", div(class = "pt-3", .gbif_tab(res))),
    bslib::nav_panel("Wikipedia", div(class = "pt-3", .wikipedia_tab(res)))
  )
}
