### R/build_eml.R
#
# Builds a GBIF/OBIS-profile EML (Ecological Metadata Language) XML
# document describing the DATASET itself - creators, abstract, methods,
# license, project info. This is what IPT calls eml.xml, separate from
# the occurrence/event data files - IPT lets you import one directly
# instead of retyping everything into its metadata screens.
#
# Structure and element order follow a real, previously-accepted
# eml.xml the user provided as a sample (GBIF EML profile 1.3,
# eml-2.2.0 namespace) - matched precisely rather than guessed, since
# EML validators are strict about element order (XML Schema sequence).
# The generated document is parsed with xml2::read_xml() before being
# returned, so a structural mistake fails loudly here rather than
# shipping broken XML in the archive.

xml_escape <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x) || x == "") return("")
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub('"', "&quot;", x, fixed = TRUE)
  x <- gsub("'", "&apos;", x, fixed = TRUE)
  x
}

# Renders a person block (creator / metadataProvider / contact /
# associatedParty). `role` is only meaningful for associatedParty.
person_xml <- function(tag, person, role = NULL) {
  paste0(
    "\t\t<", tag, ">\n",
    "\t\t\t<individualName>\n",
    "\t\t\t\t<givenName>", xml_escape(person$given_name), "</givenName>\n",
    "\t\t\t\t<surName>", xml_escape(person$sur_name), "</surName>\n",
    "\t\t\t</individualName>\n",
    if (nzchar(xml_escape(person$organization))) paste0("\t\t\t<organizationName>", xml_escape(person$organization), "</organizationName>\n") else "",
    if (nzchar(xml_escape(person$position)))     paste0("\t\t\t<positionName>", xml_escape(person$position), "</positionName>\n") else "",
    "\t\t\t<address>\n",
    if (nzchar(xml_escape(person$delivery_point))) paste0("\t\t\t\t<deliveryPoint>", xml_escape(person$delivery_point), "</deliveryPoint>\n") else "",
    if (nzchar(xml_escape(person$city)))           paste0("\t\t\t\t<city>", xml_escape(person$city), "</city>\n") else "",
    if (nzchar(xml_escape(person$admin_area)))     paste0("\t\t\t\t<administrativeArea>", xml_escape(person$admin_area), "</administrativeArea>\n") else "",
    if (nzchar(xml_escape(person$country)))        paste0("\t\t\t\t<country>", xml_escape(person$country), "</country>\n") else "",
    "\t\t\t</address>\n",
    if (nzchar(xml_escape(person$phone))) paste0("\t\t\t<phone>", xml_escape(person$phone), "</phone>\n") else "",
    if (nzchar(xml_escape(person$email))) paste0("\t\t\t<electronicMailAddress>", xml_escape(person$email), "</electronicMailAddress>\n") else "",
    if (!is.null(role) && nzchar(role))   paste0("\t\t\t<role>", xml_escape(role), "</role>\n") else "",
    "\t\t</", tag, ">\n"
  )
}

random_uuid <- function() {
  hex <- function(n) paste(sample(c(0:9, letters[1:6]), n, replace = TRUE), collapse = "")
  paste(hex(8), hex(4), hex(4), hex(4), hex(12), sep = "-")
}

#' Build a GBIF/OBIS-profile eml.xml document describing a dataset.
#'
#' @param package_id A UUID for this metadata package (auto-generated if NULL)
#' @param title Dataset title
#' @param creator A LIST of person lists - one <creator> element per entry,
#'   in order (EML allows multiple creators; a dataset commonly has several).
#'   Each person list has: given_name, sur_name, organization, position,
#'   email, phone, delivery_point, city, admin_area, country
#' @param metadata_provider,contact A single person list (same elements as
#'   above) - EML only ever has one of each of these, unlike creator
#' @param associated_party A LIST of person lists (as `creator`), each with
#'   an optional `role` element (defaults to "PUBLISHER") - one
#'   <associatedParty> element per entry. A dataset commonly credits
#'   several people here in different roles (author, editor, reviewer,
#'   processor, curator, programmer, content provider, ...)
#' @param pub_date Publication date (Date or "YYYY-MM-DD" string)
#' @param abstract_paragraphs Character vector - one <para> per element
#' @param keywords Character vector
#' @param license_url,license_title Intellectual rights license URL + display title
#' @param distribution_url Optional informational URL for the dataset
#' @param method_steps Character vector - one <methodStep><description> per element
#' @param study_extent_description,sampling_description Free text
#' @param project_id,project_title,project_abstract,funding,
#'   study_area_description,design_description Free text project fields
#'   (project block omitted entirely if project_id is NULL/empty)
#' @param project_personnel A LIST of person lists (as `creator`), each with
#'   an optional `role` element (defaults to "POINT_OF_CONTACT") - one
#'   <personnel> element per entry inside <project>. Independent of
#'   `creator`/`contact` - a project's personnel can be different people
#'   in different roles. Ignored if project_id is NULL/empty.
#' @param coverage Optional list from eml_coverage_from_data() (geographic,
#'   temporal and taxonomic coverage). NULL/empty parts are left out.
#' @return A single character string: the complete eml.xml document
build_eml_xml <- function(package_id = NULL,
                           title,
                           creator,
                           metadata_provider,
                           contact,
                           associated_party = list(),
                           pub_date = Sys.Date(),
                           abstract_paragraphs,
                           keywords = character(),
                           license_url = "https://creativecommons.org/licenses/by/4.0/legalcode",
                           license_title = license_url,
                           distribution_url = NULL,
                           method_steps = character(),
                           study_extent_description = "",
                           sampling_description = "",
                           project_id = NULL,
                           project_title = NULL,
                           project_abstract = NULL,
                           funding = NULL,
                           study_area_description = NULL,
                           design_description = NULL,
                           project_personnel = list(),
                           coverage = NULL) {

  if (is.null(package_id) || !nzchar(package_id)) package_id <- random_uuid()

  abstract_paragraphs <- abstract_paragraphs[nzchar(abstract_paragraphs)]
  abstract_xml <- paste(
    vapply(abstract_paragraphs, function(p) paste0("\t\t\t<para>", xml_escape(p), "</para>"), character(1)),
    collapse = "\n"
  )

  keywords <- keywords[nzchar(keywords)]
  keywords_xml <- if (length(keywords) > 0) {
    paste0(
      "\t\t<keywordSet>\n",
      paste(vapply(keywords, function(k) paste0("\t\t\t<keyword>", xml_escape(k), "</keyword>"), character(1)), collapse = "\n"), "\n",
      "\t\t\t<keywordThesaurus>N/A</keywordThesaurus>\n",
      "\t\t</keywordSet>\n"
    )
  } else ""

  method_steps <- method_steps[nzchar(method_steps)]
  methods_xml <- paste(vapply(method_steps, function(s) {
    paste0(
      "\t\t<methodStep>\n\t\t\t<description>\n\t\t\t\t<para>", xml_escape(s), "</para>\n\t\t\t</description>\n\t\t</methodStep>\n"
    )
  }, character(1)), collapse = "")

  # Zero or more people with a role on the DATASET (as opposed to
  # project_personnel, which is project-specific) - a real dataset
  # commonly credits several people in different roles (author, editor,
  # reviewer, processor, curator, programmer, content provider, ...).
  associated_party_xml <- if (length(associated_party) > 0) {
    paste(vapply(associated_party, function(p) {
      person_xml("associatedParty", p, role = if (!is.null(p$role) && nzchar(p$role)) p$role else "PUBLISHER")
    }, character(1)), collapse = "")
  } else ""

  coverage_xml <- coverage_to_xml(coverage)

  distribution_xml <- if (!is.null(distribution_url) && nzchar(distribution_url)) {
    paste0(
      "\t<distribution scope=\"document\">\n\t\t<online>\n\t\t\t<url function=\"information\">", xml_escape(distribution_url), "</url>\n\t\t</online>\n\t</distribution>\n"
    )
  } else ""

  # The project's personnel - zero or more people with a role on THIS
  # project specifically (EML schema: project/personnel comes right
  # after project/title, before abstract). A separate list from
  # `creator`/`contact`, since a project's personnel commonly differ in
  # number and role from the dataset-level people (e.g. a point of
  # contact who isn't a creator, or several people with different roles).
  project_personnel_xml <- if (!is.null(project_id) && nzchar(project_id) && length(project_personnel) > 0) {
    paste(vapply(project_personnel, function(p) {
      person_xml("personnel", p, role = if (!is.null(p$role) && nzchar(p$role)) p$role else "POINT_OF_CONTACT")
    }, character(1)), collapse = "")
  } else ""

  project_xml <- if (!is.null(project_id) && nzchar(project_id)) {
    paste0(
      "\t<project id=\"", xml_escape(project_id), "\">\n",
      "\t\t<title>", xml_escape(project_title), "</title>\n",
      project_personnel_xml,
      "\t\t<abstract>\n\t\t\t<para>", xml_escape(project_abstract), "</para>\n\t\t</abstract>\n",
      if (!is.null(funding) && nzchar(funding)) paste0("\t\t<funding>\n\t\t\t<para>", xml_escape(funding), "</para>\n\t\t</funding>\n") else "",
      if (!is.null(study_area_description) && nzchar(study_area_description)) paste0(
        "\t\t<studyAreaDescription>\n\t\t\t<descriptor name=\"generic\" citableClassificationSystem=\"false\">\n\t\t\t\t<descriptorValue>",
        xml_escape(study_area_description), "</descriptorValue>\n\t\t\t</descriptor>\n\t\t</studyAreaDescription>\n"
      ) else "",
      if (!is.null(design_description) && nzchar(design_description)) paste0(
        "\t\t<designDescription>\n\t\t\t<description>\n\t\t\t\t<para>", xml_escape(design_description), "</para>\n\t\t\t</description>\n\t\t</designDescription>\n"
      ) else "",
      "\t</project>\n"
    )
  } else ""

  xml <- paste0(
    '<?xml version="1.0" encoding="UTF-8"?>\n',
    '<eml:eml xmlns:eml="https://eml.ecoinformatics.org/eml-2.2.0"\n',
    '         xmlns:dc="http://purl.org/dc/terms/"\n',
    '         xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"\n',
    '         xsi:schemaLocation="https://eml.ecoinformatics.org/eml-2.2.0 https://rs.gbif.org/schema/eml-gbif-profile/1.3/eml.xsd"\n',
    '         packageId="', xml_escape(package_id), '" system="http://gbif.org" scope="system"\n',
    '         xml:lang="eng">\n',
    '\t<dataset>\n',
    '\t\t<title>', xml_escape(title), '</title>\n',
    paste(vapply(creator, function(p) person_xml("creator", p), character(1)), collapse = ""),
    person_xml("metadataProvider", metadata_provider),
    associated_party_xml,
    '\t\t<pubDate>\n\t\t  ', format(as.Date(pub_date), "%Y-%m-%d"), '\n\t\t  </pubDate>\n',
    '\t\t<language>ENGLISH</language>\n',
    '\t\t<abstract>\n', abstract_xml, '\n\t\t</abstract>\n',
    keywords_xml,
    '\t\t<intellectualRights>\n\t\t\t<para>This work is licensed under a\n',
    '                    <ulink url="', xml_escape(license_url), '">\n',
    '\t\t<citetitle>', xml_escape(license_title), '</citetitle>\n\t</ulink>.\n',
    '                </para>\n\t</intellectualRights>\n',
    distribution_xml,
    coverage_xml,
    '\t<maintenance>\n\t\t<description>\n\t\t\t<para></para>\n\t\t</description>\n',
    '\t\t<maintenanceUpdateFrequency>unknown</maintenanceUpdateFrequency>\n\t</maintenance>\n',
    person_xml("contact", contact),
    '\t<methods>\n', methods_xml,
    '\t\t<sampling>\n\t\t\t<studyExtent>\n\t\t\t\t<description>\n\t\t\t\t\t<para>', xml_escape(study_extent_description), '</para>\n',
    '\t\t\t\t</description>\n\t\t\t</studyExtent>\n',
    '\t\t\t<samplingDescription>\n\t\t\t\t<para>', xml_escape(sampling_description), '</para>\n\t\t\t</samplingDescription>\n',
    '\t\t</sampling>\n\t</methods>\n',
    project_xml,
    '\t</dataset>\n',
    '<additionalMetadata>\n\t<metadata>\n\t\t<gbif>\n\t\t\t<dateStamp>', format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3", tz = "UTC"), 'Z</dateStamp>\n',
    '\t\t</gbif>\n\t</metadata>\n</additionalMetadata>\n</eml:eml>\n'
  )

  # Fail loudly on a structural mistake here, rather than shipping
  # broken XML in the archive.
  xml2::read_xml(xml)

  xml
}


# ---- Coverage (geographic / temporal / taxonomic) --------------------------
# Worked out from the data itself, so it's always filled in and always
# matches what is published. Before this, coverage was only ever typed into
# the IPT by hand - and uploading a new eml.xml replaced it with nothing.

#' @param event Event core table (decimalLatitude, decimalLongitude, eventDate, geo_loc_name)
#' @param occurrence_tables Named list of final Occurrence tables (per assay)
#' @param matched_df match_worms()'s matched_df (worms_kingdom / worms_class), or NULL
#' @return list(geo = list(description, west, east, north, south) or NULL,
#'              temporal = list(begin, end) or NULL,
#'              taxonomic = list(description, classification = data.frame(rank, value)) or NULL)
eml_coverage_from_data <- function(event, occurrence_tables = list(), matched_df = NULL) {
  out <- list(geo = NULL, temporal = NULL, taxonomic = NULL)

  # Geographic: bounding box of every published sample.
  lat <- suppressWarnings(as.numeric(event$decimalLatitude))
  lon <- suppressWarnings(as.numeric(event$decimalLongitude))
  ok  <- !is.na(lat) & !is.na(lon) & abs(lat) <= 90 & abs(lon) <= 180
  if (any(ok)) {
    places <- if ("geo_loc_name" %in% names(event)) unique(stats::na.omit(trimws(as.character(event$geo_loc_name)))) else character()
    places <- places[nzchar(places)]
    out$geo <- list(
      description = paste0(
        if (length(places)) paste0(paste(places, collapse = "; "), ". ") else "",
        "Bounding box of the ", sum(ok), " sampling locations in this dataset."),
      west = round(min(lon[ok]), 4), east = round(max(lon[ok]), 4),
      north = round(max(lat[ok]), 4), south = round(min(lat[ok]), 4)
    )
  }

  # Temporal: first and last sampling day. eventDate may be a date, a
  # date-time or an interval ("a/b"); only full YYYY-MM-DD parts are used.
  d <- unlist(strsplit(as.character(stats::na.omit(event$eventDate)), "/", fixed = TRUE))
  d <- substr(trimws(d), 1, 10)
  d <- d[grepl("^\\d{4}-\\d{2}-\\d{2}$", d)]
  d <- d[!is.na(suppressWarnings(as.Date(d, optional = TRUE)))]
  if (length(d)) out$temporal <- list(begin = min(d), end = max(d))

  # Taxonomic: kingdoms and classes of the published records, from WoRMS.
  ids <- unique(unlist(lapply(occurrence_tables, function(t) t$scientificNameID)))
  ids <- ids[!is.na(ids) & nzchar(ids)]
  if (!is.null(matched_df) && nrow(matched_df) > 0 && length(ids) && "worms_class" %in% names(matched_df)) {
    # A record switched to WoRMS' accepted name carries that name's AphiaID.
    key <- c(matched_df$scientificNameID,
             paste0("urn:lsid:marinespecies.org:taxname:", matched_df$worms_valid_AphiaID))
    tax <- rbind(matched_df[, c("worms_kingdom", "worms_class")], matched_df[, c("worms_kingdom", "worms_class")])
    tax <- unique(tax[key %in% ids, , drop = FALSE])
    kingdoms <- sort(unique(stats::na.omit(tax$worms_kingdom)))
    classes  <- sort(unique(stats::na.omit(tax$worms_class)))
    if (length(kingdoms) || length(classes)) {
      assays <- names(occurrence_tables)
      out$taxonomic <- list(
        description = paste0(
          "Taxa detected by eDNA metabarcoding",
          if (length(assays)) paste0(" (", paste(assays, collapse = ", "), " assays)") else "",
          if (length(classes)) paste0(", in the classes: ", paste(classes, collapse = ", ")) else "",
          ". Classes are taken from WoRMS for the names published in this dataset.",
          " Sequences that could not be identified are recorded as Biota incertae sedis."),
        classification = rbind(
          data.frame(rank = rep("kingdom", length(kingdoms)), value = kingdoms, stringsAsFactors = FALSE),
          data.frame(rank = rep("class", length(classes)), value = classes, stringsAsFactors = FALSE))
      )
    }
  }
  out
}

coverage_to_xml <- function(coverage) {
  if (is.null(coverage) || (is.null(coverage$geo) && is.null(coverage$temporal) && is.null(coverage$taxonomic))) return("")
  g <- coverage$geo; t <- coverage$temporal; x <- coverage$taxonomic
  paste0(
    "\t<coverage>\n",
    if (!is.null(g)) paste0(
      "\t\t<geographicCoverage>\n",
      "\t\t\t<geographicDescription>", xml_escape(g$description), "</geographicDescription>\n",
      "\t\t\t<boundingCoordinates>\n",
      "\t\t\t\t<westBoundingCoordinate>", g$west, "</westBoundingCoordinate>\n",
      "\t\t\t\t<eastBoundingCoordinate>", g$east, "</eastBoundingCoordinate>\n",
      "\t\t\t\t<northBoundingCoordinate>", g$north, "</northBoundingCoordinate>\n",
      "\t\t\t\t<southBoundingCoordinate>", g$south, "</southBoundingCoordinate>\n",
      "\t\t\t</boundingCoordinates>\n",
      "\t\t</geographicCoverage>\n") else "",
    if (!is.null(t)) paste0(
      "\t\t<temporalCoverage>\n\t\t\t<rangeOfDates>\n",
      "\t\t\t\t<beginDate>\n\t\t\t\t\t<calendarDate>", t$begin, "</calendarDate>\n\t\t\t\t</beginDate>\n",
      "\t\t\t\t<endDate>\n\t\t\t\t\t<calendarDate>", t$end, "</calendarDate>\n\t\t\t\t</endDate>\n",
      "\t\t\t</rangeOfDates>\n\t\t</temporalCoverage>\n") else "",
    if (!is.null(x)) paste0(
      "\t\t<taxonomicCoverage>\n",
      "\t\t\t<generalTaxonomicCoverage>", xml_escape(x$description), "</generalTaxonomicCoverage>\n",
      paste(vapply(seq_len(nrow(x$classification)), function(i) paste0(
        "\t\t\t<taxonomicClassification>\n",
        "\t\t\t\t<taxonRankName>", xml_escape(x$classification$rank[i]), "</taxonRankName>\n",
        "\t\t\t\t<taxonRankValue>", xml_escape(x$classification$value[i]), "</taxonRankValue>\n",
        "\t\t\t</taxonomicClassification>\n"), character(1)), collapse = ""),
      "\t\t</taxonomicCoverage>\n") else "",
    "\t</coverage>\n"
  )
}
