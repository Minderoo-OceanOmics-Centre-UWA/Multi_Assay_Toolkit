### app.R
#
# Multi-Assay Toolkit (MAT) - interactive web app.
#
# Wraps the same R/*.R functions the CLI pipeline (scripts/01-05) uses,
# in a guided, validated workflow instead of hand-editing config.R and
# running scripts one at a time. Deploy as-is to Posit Cloud / Connect /
# shinyapps.io - app.R sits at the repo root alongside R/ and config.R
# so relative paths (source("R/...")) work the same way they do for the
# CLI scripts, with no extra packaging step.
#
# Status: all seven steps are functional - Upload & Configure, Validate
# & Fix, Build Core Files, interactive WoRMS Taxonomy Review, QC
# Checks, Metadata (generates eml.xml), and Results & Download.
#
# UI architecture note: each step has its OWN persistent output
# (step2_body/step3_body via renderUI, step 1 fully static), shown or
# hidden with conditionalPanel rather than swapped in and out of a
# single shared output. A single shared "switch(current_step, ...)"
# output looked simpler at first but re-renders whichever step becomes
# active from scratch every time - which silently wiped file inputs
# (browsers won't let a re-rendered <input type="file"> keep its
# selection) and would have reset Step 3's dropdowns too, the moment
# Back/Restart navigation was added. Step 1's assay rows are managed
# with insertUI()/removeUI() against a static container div for the
# same reason: adding a row must never touch the DOM nodes of rows
# already there.

library(shiny)
library(bslib)
library(bsicons)
library(DT)
library(dplyr)
library(ggplot2)
library(zip)

# AWS settings for saving to Draft/Publish live in .Renviron next to app.R.
# R only reads that file automatically when it STARTS in this folder, so load
# it explicitly - a local run then uses the same bucket as the deployed app.
if (file.exists(".Renviron")) readRenviron(".Renviron")

source("R/faire_io.R")
source("R/sample_split.R")
source("R/validate_faire_files.R")
source("R/suggest_assay_mapping.R")
source("R/detect_assay_name.R")
source("R/detect_reference_db.R")
source("R/check_dwc_mapping.R")
source("R/taxon_lookup.R")
source("R/build_report.R")
source("R/build_event_core.R")
source("R/build_occurrence.R")
source("R/build_dna_extension.R")
source("R/build_emof.R")
source("R/worms_match.R")
source("R/qc_checks.R")
source("R/build_eml.R")
source("R/eml_excel.R")
source("R/archive_history.R")
source("R/user_auth.R")
source("R/obis_notify.R")
source("R/about_page.R")
source("R/footer_policies.R")

# Converts a scientific name into a safe Shiny input-id fragment (no
# spaces/punctuation) for the Step 4 per-name review widgets.
safe_id <- function(x) gsub("[^A-Za-z0-9]", "_", x)

# Success popup (SweetAlert-style): big green tick, title, details, and an
# optional button that jumps to the tab where the result now lives.
success_modal <- function(title, details, go_tab = NULL, go_label = NULL) {
  modalDialog(
    div(class = "text-center py-3",
        div(style = "font-size: 4.5rem; color: #1a9850; line-height: 1;", bsicons::bs_icon("check-circle-fill")),
        h3(title, class = "mt-3 mb-2", style = "font-weight: 700;"),
        div(class = "text-muted", details)),
    footer = div(class = "w-100 d-flex justify-content-center gap-2",
                 if (!is.null(go_tab)) actionButton("go_to_saved_tab", go_label, class = "btn-primary"),
                 modalButton("Close")),
    size = "m", easyClose = TRUE
  )
}

# Shown when a guest clicks Download / Save / Publish / Move to Publish -
# those need an account, but the rest of the process doesn't. One shared
# button id ("auth_prompt_login_btn"): its one observer (in server()) just
# exits guest mode and shows the login screen, so it works the same
# wherever this modal is opened from.
login_required_modal <- function(action) {
  modalDialog(
    title = tagList(bsicons::bs_icon("lock-fill"), " Log in required"),
    p(paste0("You need to log in to ", action, ". Without an account you can still build and download the archive.")),
    p(class = "muted", "Not part of this organisation, or need access? Contact ",
      tags$a(href = paste0("mailto:", contact_email()), contact_email()), "."),
    footer = tagList(modalButton("Keep browsing as guest"),
                      actionButton("auth_prompt_login_btn", "Log in now", class = "btn-primary")),
    size = "m", easyClose = TRUE
  )
}

# "Check" button for a name in the Taxonomy Review step: opens the WoRMS /
# FishBase lookup popup. If from_input is given, the current text of that
# input (e.g. the user's edited correction) is checked instead of the name.
taxon_check_button <- function(nm, from_input = NULL) {
  js <- if (is.null(from_input)) {
    "Shiny.setInputValue('taxon_check', this.getAttribute('data-taxon'), {priority: 'event'})"
  } else {
    sprintf("var v = document.getElementById('%s').value; Shiny.setInputValue('taxon_check', v || this.getAttribute('data-taxon'), {priority: 'event'})", from_input)
  }
  tags$button(type = "button", class = "btn btn-sm btn-outline-primary", `data-taxon` = nm, onclick = js,
              bsicons::bs_icon("search"), " Check")
}

# Defined once, used both as the Step 6 license selectInput's choices
# (label = name, value = URL) and to look the label back up from the
# chosen URL when generating eml.xml.
# Only rows whose FAIRe samp_category is exactly this are published; controls,
# blanks and anything else are left out. Fixed (not a user setting): GBIF's MDT
# guide says only real biological/environmental samples should be published.
SAMPLE_CATEGORY_KEEP <- "sample"
# One sentence pointing to the raw sequence reads, for the eml.xml abstract
# and methods. Names the archive and accession when the link makes that clear
# (ENA / NCBI SRA / DDBJ); NULL when no link was given.
raw_reads_sentence <- function(uri) {
  if (is.null(uri) || length(uri) == 0 || is.na(uri) || !nzchar(trimws(uri))) return(NULL)
  uri <- trimws(uri)
  archive <- if (grepl("ebi\\.ac\\.uk", uri)) "the European Nucleotide Archive (ENA)"
             else if (grepl("ncbi\\.nlm\\.nih\\.gov", uri)) "the NCBI Sequence Read Archive (SRA)"
             else if (grepl("ddbj\\.nig\\.ac\\.jp", uri)) "the DNA Data Bank of Japan (DDBJ)"
             else NULL
  acc <- regmatches(uri, regexpr("(PRJ[EDN][A-Z][0-9]+|[EDS]RP[0-9]+)", uri))
  if (is.null(archive)) return(paste0("Raw sequence reads for this dataset are publicly archived at: ", uri))
  paste0("Raw sequence reads for this dataset are available from ", archive,
         if (length(acc)) paste0(" under accession ", acc), ": ", uri)
}

# Card header with the standard icon tile, a title and an optional grey
# subtitle underneath (styled by .section-title / .section-sub).
section_header <- function(icon, title, subtitle = NULL) {
  card_header(bsicons::bs_icon(icon, class = "section-icon"),
              div(class = "section-title", span(title), if (!is.null(subtitle)) span(class = "section-sub", subtitle)))
}

# Animated DNA double helix (pure CSS): each base pair is two dots that swap
# sides, joined by a rung that shrinks as the strands cross.
dna_helix <- function(n = 12) {
  span(class = "dna-helix", `aria-hidden` = "true",
       lapply(seq_len(n) - 1, function(i) span(class = "bp", style = paste0("--i:", i), span(class = "rung"))))
}

# A uiOutput that shows the DNA loader until its first result arrives (Shiny
# replaces the contents on render). While it re-calculates later, CSS
# (.loading-output.recalculating) fades the old result and overlays a spinner.
loading_output <- function(id, text = "Loading...") {
  div(id = id, class = "shiny-html-output loading-output",
    div(class = "mat-loader", role = "status", `aria-live` = "polite",
      dna_helix(), span(class = "mat-loader-text", text)))
}

# Field label for an optional input: "Name (optional) (i)", where the (i)
# icon shows an explanation and example on hover or tap.
optional_label <- function(text, ...) {
  tags$span(text, " ", tags$span(class = "optional-tag", "(optional)"), " ",
    bslib::tooltip(
      tags$span(class = "info-tip", tabindex = "0", role = "button", `aria-label` = paste("About", text),
                bsicons::bs_icon("info-circle")),
      paste0(...), placement = "right"))
}

MDT_CONTROLS_GUIDE_URL <-"https://docs.gbif-uat.org/mdt-user-guide/en/#before_you_start"

EML_LICENSE_CHOICES <- c(
  "CC0 1.0 (Public Domain)" = "https://creativecommons.org/publicdomain/zero/1.0/legalcode",
  "CC-BY 4.0"               = "https://creativecommons.org/licenses/by/4.0/legalcode",
  "CC-BY-NC 4.0"            = "https://creativecommons.org/licenses/by-nc/4.0/legalcode"
)

# Shiny's default upload cap is 5MB - FAIRe workbooks with real ASV/OTU
# tables commonly run 10-20MB+. Raised to 100MB per file; adjust if a
# project's files run larger than that.
options(shiny.maxRequestSize = 100 * 1024^2)

# ---------------------------------------------------------------------
# Theme - Oceans Institute brand colours (OI Branding Colours 2026):
# UWA Blue #003087 primary, OI Dark Blue #1c2e5b dark text/hover,
# OI Extra Dark Blue #0a1b2e header/footer, OI Cyan #62d5ff and
# UWA Gold #daaa00 as small accents only.
# ---------------------------------------------------------------------
app_theme <- bs_theme(
  version      = 5,
  bg           = "#f5f7fa",
  fg           = "#0b0b0b",
  primary      = "#003087",
  secondary    = "#898781",
  success      = "#0ca30c",
  warning      = "#fab219",
  danger       = "#d03b3b",
  base_font    = bslib::font_google("Inter"),
  heading_font = bslib::font_google("Inter"),
  "border-radius" = "0.65rem",
  "card-border-color" = "#eceae4"
)

STEPS <- c("Upload", "Validate", "Build", "Taxonomy", "QC Checks", "Metadata", "Results")
ACTIVE_STEPS <- 1:7  # all steps functional

app_css <- "
  body { background-color: #f5f7fa; }
  html, body { height: auto !important; overflow-y: auto !important; }

  /* Sticky footer: the page container fills at least the viewport and the
     footer's auto top margin pushes it to the bottom when content is short
     (min-height, not height, so long pages still just grow and scroll). */
  body > .container-fluid { min-height: 100vh; display: flex; flex-direction: column; padding-left: 0; padding-right: 0; }

  /* Positioning context for the file-input scroll-jack fix in the
     <script> below (that fix has to happen in JS - Shiny's own
     fileInput ships an inline `!important` style that no CSS rule,
     however specific, can override). */
  .btn-file { position: relative; overflow: hidden; }

  /* Header: one straight, solid band (no wave or decoration). */
  .app-header {
    background: #0a1b2e; color: #ffffff; padding: 12px 32px; display: flex; align-items: center; gap: 14px;
    border-bottom: 3px solid #daaa00;
  }
  .app-header .brand-mark { flex-shrink: 0; line-height: 0; }  .app-header h1 { font-size: 1.08rem; font-weight: 700; margin: 0; letter-spacing: -0.005em; }
  .app-header p { color: #a9bbd6; margin: 2px 0 0 0; font-size: 0.8rem; }
  .app-header .header-spacer { flex: 1; }
  .restart-btn { color: #d6e0ee !important; border-color: rgba(255,255,255,0.22) !important; }
  .restart-btn:hover { color: #fff !important; border-color: rgba(255,255,255,0.6) !important; background: rgba(255,255,255,0.08) !important; }

  /* White text on blue buttons, and white cards on the grey page. */
  .btn-primary, .btn-primary:hover, .btn-primary:focus, .btn-primary:active { color: #ffffff !important; }
  .card, .card-body { background-color: #ffffff; }

  /* Main navigation: a white bar with an underline on the active tab. */
  #main_tab.nav-tabs { background: #ffffff; border-bottom: 1px solid #e3e6ea; padding: 0 24px; gap: 2px; }
  #main_tab.nav-tabs .nav-link {
    border: none; border-bottom: 2px solid transparent; border-radius: 0; background: none;
    color: #4f5b66; font-weight: 600; font-size: 0.9rem; padding: 14px 14px 12px 14px; margin-bottom: -1px;
  }
  #main_tab.nav-tabs .nav-link:hover { color: #0a1b2e; border-bottom-color: #cdd4db; }
  #main_tab.nav-tabs .nav-link.active { color: #1c2e5b; border-bottom-color: #003087; background: none; }

  .stepper-flex { display: flex; align-items: flex-start; padding: 26px 48px 22px 48px; background: #ffffff; border-bottom: 1px solid #e3e6ea; }
  .step-item { display: flex; flex-direction: column; align-items: center; }
  .step-circle {
    width: 34px; height: 34px; border-radius: 50%; display: flex; align-items: center; justify-content: center;
    font-weight: 700; font-size: 0.82rem; border: 2px solid #dedcd3; color: #a9a89f; background: #fff; transition: all .15s ease;
  }
  .step-circle.active { border-color: #003087; background: #003087; color: #fff; box-shadow: 0 0 0 5px rgba(0,48,135,0.14); }
  .step-circle.done { border-color: #0ca30c; background: #0ca30c; color: #fff; }
  .step-circle.disabled { border-color: #eceae4; color: #c3c2b7; }
  .step-label { font-size: 0.74rem; margin-top: 7px; color: #a9a89f; text-align: center; max-width: 84px; }
  .step-label.active { color: #0b0b0b; font-weight: 600; }
  .step-line { flex: 1; height: 2px; background: #eceae4; margin: 16px 6px 0 6px; }
  .step-line.done { background: #0ca30c; }

  .content-wrap { max-width: 1080px; margin: 0 auto; padding: 32px 24px 70px 24px; position: relative; z-index: 1; }
  .card {
    box-shadow: 0 1px 2px rgba(11,11,11,0.03), 0 4px 10px rgba(11,11,11,0.03);
    margin-bottom: 18px; transition: box-shadow .18s ease, transform .18s ease;
  }
  .card:hover { box-shadow: 0 1px 2px rgba(11,11,11,0.04), 0 6px 16px rgba(11,11,11,0.06); }
  .card-header { background: #fff; font-weight: 600; font-size: 0.94rem; padding: 14px 20px; display:flex; align-items:center; gap:8px; }
  .card-body { padding: 20px; }
  .section-icon { color: #003087; }

  .issue-card { border-left: 4px solid #d03b3b; }
  .issue-card.warning { border-left-color: #fab219; }

  .assay-row { border: 1px solid #eceae4; border-radius: 10px; padding: 16px; margin-bottom: 12px; background: #fdfdfc; }
  .assay-col-choices .shiny-options-group { display: grid; grid-template-columns: repeat(auto-fill, minmax(280px, 1fr)); gap: 8px; margin-top: 10px; }
  .assay-col-choices .checkbox { margin: 0; }
  .assay-col-choices .checkbox label { display: flex; gap: 10px; align-items: flex-start; width: 100%; height: 100%; padding: 10px 12px;
    border: 1px solid #e3e1da; border-radius: 8px; background: #fff; cursor: pointer; }
  .assay-col-choices .checkbox label:has(input:checked) { border-color: #003087; background: #eef3fb; }
  .assay-col-choices .checkbox input { margin-top: 4px; flex-shrink: 0; }
  .assay-col-choice { font-size: 0.92rem; line-height: 1.45; }
  .assay-col-id { display: inline-block; font-size: 0.75rem; color: #6c757d; background: #f1f0ec; border-radius: 4px; padding: 0 6px; margin-right: 6px; }
  .assay-col-meta { display: flex; flex-direction: column; margin-top: 2px; }
  .btn-white { --bs-btn-bg: #fff; }
  .read-more-link { white-space: nowrap; text-decoration: none; font-weight: 500; }
  .read-more-link:hover { text-decoration: underline; }
  .read-more-link .read-more-chevron { margin-left: 3px; vertical-align: -1px; transition: transform 0.2s; }
  .read-more-link[aria-expanded='true'] .read-more-chevron { transform: rotate(180deg); }
  .read-more-link[aria-expanded='true'] .when-collapsed, .read-more-link[aria-expanded='false'] .when-expanded { display: none; }
  .muted { color: #898781; font-size: 0.86rem; }
  .stat-box { border: 1px solid #eceae4; border-radius: 10px; padding: 16px 18px; text-align: center; background: #fff; }
  .stat-box .stat-value { font-size: 1.6rem; font-weight: 700; color: #0b0b0b; }
  .stat-box .stat-label { font-size: 0.78rem; color: #898781; margin-top: 2px; }

  .btn-primary { font-weight: 600; }
  h4, h5 { font-weight: 700; letter-spacing: -0.01em; }
  .nav-row { display: flex; justify-content: space-between; align-items: center; margin-top: 18px; }

  /* Login/signup/forgot-password card - the one thing a visitor sees before
     anything else, so it gets its own (more deliberate) look rather than
     just reusing the plain default .card used everywhere else. */
  .auth-wrap { min-height: calc(100vh - 250px); display: flex; align-items: center; justify-content: center; padding: 36px 16px; }
  .auth-card { border: none; box-shadow: 0 2px 8px rgba(11,11,11,0.05), 0 16px 40px rgba(28,46,91,0.09); overflow: hidden; }
  .auth-card:hover { transform: none; box-shadow: 0 2px 8px rgba(11,11,11,0.05), 0 16px 40px rgba(28,46,91,0.09); }
  .auth-card .card-header {
    display: flex; flex-direction: column; align-items: center; gap: 10px;
    padding: 30px 24px 22px 24px; text-align: center; border-bottom: 1px solid #eceae4;
  }
  .auth-card .card-header .auth-icon {
    width: 48px; height: 48px; border-radius: 50%; display: flex; align-items: center; justify-content: center;
    background: linear-gradient(160deg, #1c2e5b 0%, #003087 100%); color: #fff; font-size: 1.35rem;
  }
  .auth-card .card-header .auth-title { font-size: 1.12rem; font-weight: 700; letter-spacing: -0.01em; color: #0b0b0b; }
  .auth-card .card-body { padding: 28px 30px 30px 30px; }
  .auth-card .form-label { font-weight: 600; font-size: 0.86rem; color: #0b0b0b; }
  .auth-card .form-control { padding: 10px 14px; }
  .auth-card .shiny-input-container:not(.shiny-input-container-inline) { width: 100%; max-width: 100%; }
  .auth-card .form-group, .auth-card > .card-body > div.form-group { margin-bottom: 16px; }
  .auth-card a { color: #003087; font-weight: 600; text-decoration: none; }
  .auth-card a:hover { text-decoration: underline; }
  .auth-guest-box {
    margin-top: 22px; padding: 14px 16px; border-radius: 10px;
    background: #f4f7f9; border: 1px solid #eceae4; font-size: 0.86rem; color: #63625c;
  }

  .app-footer {
    background: #0a1b2e; border-top: 1px solid #1c2e5b;
    color: #d6e6f5; padding: 22px 32px; margin-top: auto; flex-shrink: 0;
    display: flex; flex-direction: column; align-items: center; gap: 6px;
    text-align: center; font-size: 0.82rem;
  }
  .app-footer a { color: #62d5ff; text-decoration: none; }
  .app-footer a:hover { color: #ffffff; text-decoration: underline; }
  .app-footer .footer-credit { color: #ffffff; font-weight: 600; font-size: 0.86rem; }
  .app-footer .footer-links { display: flex; gap: 6px; align-items: center; flex-wrap: wrap; justify-content: center; }
  .app-footer .footer-sep { color: rgba(255,255,255,0.25); }
  .app-footer .footer-copyright { color: #8b9bb8; font-size: 0.76rem; margin-top: 4px; }
  .footer-partner { background: #ffffff; border-radius: 10px; padding: 10px 22px; margin-bottom: 12px; line-height: 0; }
  .footer-partner img { height: 54px; max-width: 100%; }
  .footer-partner.placeholder {
    background: transparent; border: 1.5px dashed rgba(255,255,255,0.28); color: #8b9bb8;
    line-height: 1.4; font-size: 0.8rem; padding: 14px 26px;
  }

  /* ---- Page background --------------------------------------------------
     Soft brand-coloured glows at the top, fixed to the viewport so they
     stay put on scroll. */
  body {
    background-color: #f3f6f9;
    background-image:
      radial-gradient(1100px 520px at 8% -8%, rgba(0,48,135,0.11), transparent 62%),
      radial-gradient(900px 480px at 96% -4%, rgba(98,213,255,0.12), transparent 60%),
      linear-gradient(180deg, #eef2f9 0%, #f3f6f9 420px);
    background-attachment: fixed;
  }

  .page-photo {
    position: fixed; inset: 0; z-index: -2; pointer-events: none;
    background: url('page_bg.jpg') center / cover no-repeat; opacity: 0.03;
  }

  /* ---- Cards and forms -------------------------------------------------
     Defined last so they override the defaults above. Neutral greys,
     8-10px radii, one accent colour (the primary blue). */
  .content-wrap .card { border: 1px solid #e4e7ec; border-radius: 12px; box-shadow: 0 1px 2px rgba(16,24,40,0.04); overflow: hidden; }
  .content-wrap .card:hover { box-shadow: 0 1px 2px rgba(16,24,40,0.04), 0 4px 12px rgba(16,24,40,0.05); transform: none; }
  /* Card headers: thin brand accent along the top, soft tinted header,
     solid UWA Blue icon tile, larger title, optional grey subtitle
     (section_header()). Amber versions for the Step 4 review cards. */
  .content-wrap .card { position: relative; }
  .content-wrap .card:has(> .card-header)::before {
    content: ''; position: absolute; top: 0; left: 0; right: 0; height: 3px; z-index: 1;
    background: linear-gradient(90deg, #003087 0%, #1c6fd1 55%, #62d5ff 100%);
  }
  /* .content-wrap .bslib-card > .card-header beats bslib's own
     .bslib-card .card-header (which halves the padding and sets a 4px gap). */
  .card-header,
  .content-wrap .bslib-card > .card-header {
    display: flex; align-items: center; gap: 14px; min-height: 76px;
    padding: 18px 24px; padding-block: 18px;
    background: linear-gradient(180deg, #ffffff 0%, #f6f9fd 100%); border-bottom: 1px solid #e6ecf5;
    font-size: 1.05rem; font-weight: 700; color: #0a1b2e; letter-spacing: -0.01em; line-height: 1.3;
  }
  .card-header .section-icon {
    width: 40px !important; height: 40px !important; padding: 10px; border-radius: 11px; flex-shrink: 0;
    background: linear-gradient(145deg, #003087 0%, #1c2e5b 100%); color: #ffffff;
    box-shadow: 0 4px 10px rgba(0,48,135,0.22), inset 0 1px 0 rgba(255,255,255,0.18);
  }
  .card-header .section-title { display: flex; flex-direction: column; line-height: 1.25; }
  .card-header .section-sub { font-size: 0.84rem; font-weight: 400; color: #667085; letter-spacing: 0; margin-top: 3px; line-height: 1.4; }
  .issue-card.warning:has(> .card-header)::before { background: linear-gradient(90deg, #dc8a00 0%, #f5a524 100%); }
  .issue-card.warning .card-header { background: linear-gradient(180deg, #ffffff 0%, #fffaf0 100%); }
  .issue-card.warning .card-header .section-icon {
    background: linear-gradient(145deg, #f5a524 0%, #c27800 100%); box-shadow: 0 4px 10px rgba(194,120,0,0.25);
  }
  .card-body, .content-wrap .bslib-card > .card-body { padding: 22px 24px; }

  .control-label, .form-label { font-size: 0.85rem; font-weight: 600; color: #344054; margin-bottom: 6px; }
  .form-group { margin-bottom: 16px; }
  .content-wrap .shiny-input-container:not(.shiny-input-container-inline) { width: 100%; max-width: 100%; }
  .form-control, .form-select, .selectize-input {
    background-color: #ffffff; border: 1px solid #d0d5dd; border-radius: 8px; padding: 9px 12px; font-size: 0.92rem; color: #101828;
    box-shadow: 0 1px 2px rgba(16,24,40,0.05); transition: border-color .15s ease, box-shadow .15s ease;
  }
  .form-control::placeholder { color: #98a2b3; }
  .form-control:focus, .form-select:focus, .selectize-input.focus {
    border-color: #003087; box-shadow: 0 0 0 4px rgba(0,48,135,0.14); outline: none;
  }
  .form-control[readonly] { background-color: #ffffff; }
  .muted { color: #667085; font-size: 0.85rem; }

  /* File input: a proper button joined to the file-name box */
  .input-group .btn-file {
    background: #f9fafb; border: 1px solid #d0d5dd; color: #344054; font-weight: 600; font-size: 0.88rem;
    border-radius: 8px 0 0 8px; padding: 9px 16px; box-shadow: 0 1px 2px rgba(16,24,40,0.05);
  }
  .input-group .btn-file:hover { background: #f2f4f7; color: #101828; }
  .input-group > .form-control { border-radius: 0 8px 8px 0 !important; border-left: none; }
  .shiny-file-input-progress { border-radius: 999px; background: #eef0f3; margin-top: 8px; }
  .shiny-file-input-progress .progress-bar { background: #003087; border-radius: 999px; font-size: 0.72rem; }

  /* Curated / nt choice as a two-option switch */
  .choice-switch { display: inline-flex; gap: 2px; background: #f2f4f7; padding: 3px; border-radius: 9px; }
  .choice-switch .radio-inline { margin: 0 !important; padding: 0; position: relative; }
  .choice-switch .radio-inline input { position: absolute; opacity: 0; pointer-events: none; }
  .choice-switch .radio-inline span { display: block; padding: 5px 12px; border-radius: 7px; font-size: 0.82rem; font-weight: 600; color: #475467; cursor: pointer; white-space: nowrap; }
  .choice-switch .radio-inline:has(input:checked) span { background: #ffffff; color: #101828; box-shadow: 0 1px 3px rgba(16,24,40,0.14); }
  #reference_db .shiny-options-group { display: inline-flex; gap: 2px; background: #f2f4f7; padding: 4px; border-radius: 10px; margin-top: 2px; }
  #reference_db .radio-inline { margin: 0 !important; padding: 0; position: relative; }
  #reference_db .radio-inline input { position: absolute; opacity: 0; pointer-events: none; }
  #reference_db .radio-inline span { display: block; padding: 7px 16px; border-radius: 7px; font-size: 0.88rem; font-weight: 600; color: #475467; cursor: pointer; transition: all .15s ease; }
  #reference_db .radio-inline:has(input:checked) span { background: #ffffff; color: #101828; box-shadow: 0 1px 3px rgba(16,24,40,0.14); }
  #reference_db .radio-inline:has(input:focus-visible) span { outline: 2px solid #003087; }

  /* One assay's upload row */
  .assay-row { background: #ffffff; border: 1px solid #e4e7ec; border-radius: 10px; padding: 16px 18px 2px 18px; margin-bottom: 12px; }
  .assay-row .form-group { margin-bottom: 10px; }
  .assay-row .shiny-file-input-progress { margin: 6px 0 0 0; height: 6px; }
  .assay-row .shiny-file-input-progress .progress-bar { font-size: 0; }
  .assay-row .btn-outline-danger { border-color: #f4c7c3; color: #d92d20; border-radius: 8px; }
  .assay-row .btn-outline-danger:hover { background: #fef3f2; color: #b42318; }

  /* 'Add another ...' buttons as a dashed drop-in row */
  .content-wrap [id^='add_'].btn {
    display: block; width: 100%; border: 1.5px dashed #c4cdd6; background: #fafbfc; color: #1c2e5b;
    font-weight: 600; border-radius: 10px; padding: 10px;
  }
  .content-wrap [id^='add_'].btn:hover { border-color: #003087; background: #eef3fb; color: #1c2e5b; }

  /* Notices */
  .content-wrap .alert { border: 1px solid; border-left-width: 4px; border-radius: 10px; font-size: 0.9rem; line-height: 1.55; }
  /* Step 1 Sample handling card */
  .auto-callout { display: flex; gap: 12px; align-items: flex-start; background: #eef3fb; border: 1px solid #cfdcf0;
    border-left: 4px solid #003087; border-radius: 10px; padding: 12px 16px; margin-bottom: 16px; color: #0a1b2e; line-height: 1.55; }
  .auto-callout .bi { color: #003087; font-size: 1.25rem; flex-shrink: 0; margin-top: 2px; }
  .split-heading { font-weight: 700; color: #1c2e5b; margin: 6px 0 8px 0; }
  .split-steps { padding-left: 1.2rem; margin-bottom: 6px; line-height: 1.6; }
  .split-empty { border: 1.5px dashed #c4cdd6; border-radius: 10px; padding: 12px 16px; color: #475467; background: #fafbfc; }
  .split-tiles { display: grid; grid-template-columns: 1fr 1fr; gap: 12px; margin-bottom: 12px; }
  .split-tile { border-radius: 10px; padding: 12px 16px; border: 1px solid #e4e7ec; color: #344054; }
  .split-tile .num { font-size: 1.6rem; font-weight: 800; line-height: 1.1; }
  .split-tile.kept { background: #ecfdf3; border-color: #c6f0d6; } .split-tile.kept .num { color: #05603a; }
  .split-tile.removed { background: #f5f7fa; } .split-tile.removed .num { color: #344054; }
  .split-table { max-width: 560px; margin-bottom: 10px; }
  .split-match { border-radius: 10px; padding: 10px 14px; margin-bottom: 10px; font-weight: 600; }
  .optional-fields { margin-top: 14px; padding-top: 12px; border-top: 1px dashed #d0d5dd; }
  /* Loaders for slow outputs (loading_output()): DNA double helix */
  .mat-loader { display: flex; flex-direction: column; align-items: center; gap: 12px; padding: 26px 8px 22px 8px; }
  .mat-loader-text { color: #1c2e5b; font-weight: 600; font-size: 0.92rem; }
  .dna-helix { display: inline-flex; align-items: center; gap: 5px; height: 40px; }
  .dna-helix .bp { position: relative; width: 8px; height: 40px; display: inline-block; --d: calc(var(--i) * -0.11s); }
  .dna-helix .bp::before, .dna-helix .bp::after {
    content: ''; position: absolute; left: 0; top: 16px; width: 8px; height: 8px; border-radius: 50%;
    animation: dna-strand 1.4s ease-in-out infinite; animation-delay: var(--d);
  }
  .dna-helix .bp::before { background: #003087; }
  .dna-helix .bp::after  { background: #62d5ff; animation-name: dna-strand-2; }
  .dna-helix .rung {
    position: absolute; left: 3px; top: 50%; width: 2px; height: 30px; border-radius: 1px; transform: translateY(-50%);
    background: linear-gradient(180deg, #003087, #62d5ff); opacity: 0.45;
    animation: dna-rung 1.4s ease-in-out infinite; animation-delay: var(--d);
  }
  @keyframes dna-strand   { 0%, 100% { transform: translateY(-15px) scale(1);   z-index: 2; } 50% { transform: translateY(15px) scale(0.55); z-index: 0; } }
  @keyframes dna-strand-2 { 0%, 100% { transform: translateY(15px) scale(0.55); z-index: 0; } 50% { transform: translateY(-15px) scale(1);   z-index: 2; } }
  @keyframes dna-rung     { 0%, 50%, 100% { height: 30px; } 25%, 75% { height: 2px; } }
  .loading-output.recalculating { position: relative; opacity: 1 !important; min-height: 48px; }
  .loading-output.recalculating > * { opacity: 0.35; transition: opacity 0.2s; }
  .loading-output.recalculating::after { content: ''; position: absolute; top: 14px; left: 50%; margin-left: -12px;
    width: 24px; height: 24px; border: 3px solid #cfdcf0; border-top-color: #003087; border-radius: 50%; animation: mat-spin 0.8s linear infinite; }
  @keyframes mat-spin { to { transform: rotate(360deg); } }
  @media (prefers-reduced-motion: reduce) { .dna-helix .bp::before, .dna-helix .bp::after, .dna-helix .rung, .loading-output.recalculating::after { animation-duration: 4s; } }
  .optional-tag { color: #98a2b3; font-weight: 400; }
  .info-tip { color: #003087; cursor: help; }
  .info-tip:focus-visible { outline: 2px solid #003087; outline-offset: 2px; border-radius: 50%; }
  .uneven-confirm { background: #fffaeb; border: 1px solid #fdeab8; border-radius: 10px; padding: 10px 14px; }
  .uneven-confirm .form-check-label { font-weight: 600; }
  .split-match.ok { background: #ecfdf3; border: 1px solid #c6f0d6; color: #05603a; }
  .split-details { margin-top: 6px; } .split-details summary { cursor: pointer; color: #003087; font-weight: 600; }
  .split-names { font-size: 0.82rem; color: #475467; margin: 4px 0 8px 0; max-height: 160px; overflow-y: auto; }
  @media (max-width: 520px) { .split-tiles { grid-template-columns: 1fr; } }
  .content-wrap .alert-info    { background: #eef3fb; border-color: #cfdcf0; border-left-color: #003087; color: #0a1b2e; }
  .content-wrap .alert-warning { background: #fffaeb; border-color: #fdeab8; border-left-color: #f5a524; color: #5c4100; }
  .content-wrap .alert-danger  { background: #fef3f2; border-color: #fbd5d2; border-left-color: #d92d20; color: #7a271a; }
  .content-wrap .alert-success { background: #ecfdf3; border-color: #c6f0d6; border-left-color: #12b76a; color: #05603a; }

  /* Buttons */
  .btn { border-radius: 8px; font-weight: 600; }
  .btn-primary { box-shadow: 0 1px 2px rgba(16,24,40,0.08); }
  .content-wrap .btn-primary:not(.btn-sm) { padding: 10px 20px; }

  /* Stepper */
  .stepper-flex { padding: 22px 48px 18px 48px; }
  .step-circle { width: 32px; height: 32px; font-size: 0.8rem; }
  .step-label { font-size: 0.76rem; font-weight: 500; color: #98a2b3; }
  .step-label.active { color: #101828; font-weight: 700; }

  /* Publish tab: one row per archive, its actions on the same row */
  .pub-list { display: flex; flex-direction: column; gap: 12px; }
  .pub-item { display: flex; gap: 16px; align-items: center; flex-wrap: wrap; padding: 16px 18px;
              background: #fff; border: 1px solid #dde3ee; border-radius: 10px;
              transition: border-color .15s, box-shadow .15s; }
  .pub-item:hover { border-color: #003087; box-shadow: 0 4px 14px rgba(0,48,135,.08); }
  .pub-icon { flex: none; width: 46px; height: 46px; border-radius: 10px; background: #e5ecf7; color: #003087;
              display: flex; align-items: center; justify-content: center; font-size: 22px; }
  .pub-main { flex: 1 1 340px; min-width: 0; }
  .pub-title { font-weight: 700; font-size: 1.05rem; color: #0a1b2e; }
  .pub-badge { display: inline-block; margin-left: 8px; padding: 2px 9px; border-radius: 999px; font-size: .72rem;
               font-weight: 600; background: #e7f6ec; color: #1e7a3c; vertical-align: middle; }
  .pub-meta { display: flex; gap: 6px 16px; flex-wrap: wrap; margin-top: 2px; color: #55607a; font-size: .85rem; }
  .pub-meta .bi { margin-right: 4px; }
  .pub-link { display: flex; align-items: center; gap: 6px; margin-top: 10px; padding: 4px 4px 4px 10px;
              background: #f4f6fa; border: 1px solid #e5ecf7; border-radius: 8px; max-width: 640px; }
  .pub-link a.pub-url { flex: 1; min-width: 0; overflow: hidden; text-overflow: ellipsis; white-space: nowrap;
                        font-family: ui-monospace, Consolas, monospace; font-size: .8rem; color: #1c2e5b; text-decoration: none; }
  .pub-link a.pub-url:hover { text-decoration: underline; }
  .pub-link .btn { flex: none; padding: 3px 10px; font-size: .8rem; }
  .pub-actions { display: flex; gap: 8px; flex-wrap: wrap; align-items: center; }
  .pub-actions .btn { white-space: nowrap; }
  .pub-empty { text-align: center; padding: 36px 16px; color: #55607a; }
  .pub-empty .bi { font-size: 2rem; color: #98a2b3; display: block; margin-bottom: 8px; }
"

# ---------------------------------------------------------------------
# One assay upload row's markup. Reused for the first row and for every
# row added via insertUI - each gets a stable id so it (and only it)
# can be removed later without touching its siblings.
# ---------------------------------------------------------------------
assay_row_ui <- function(i) {
  div(id = paste0("assay_row_", i), class = "assay-row",
      fluidRow(
        column(6, fileInput(paste0("assay_file_", i), "FAIRe .xlsx file", accept = ".xlsx")),
        column(5, textInput(paste0("assay_name_", i), "Assay name",
                             placeholder = "Auto-detected after upload - edit if you'd rather use a different name")),
        column(1, style = "padding-top: 32px;",
               actionButton(paste0("remove_assay_", i), bsicons::bs_icon("x-lg"), class = "btn-sm btn-outline-danger"))
      )
  )
}

# One Step 6 creator row's markup - same insertUI/removeUI pattern as
# assay_row_ui above (safe here because step6_body's renderUI has no
# reactive dependency and only ever runs once, so rows added later
# never get wiped out by a re-render). EML allows multiple <creator>
# elements per dataset. Each creator has their OWN organization/address
# fields (NOT the shared "Organization / address" block below, which is
# only for metadata provider/contact/associated party/project personnel)
# - real multi-author datasets commonly have co-authors at different
# institutions (e.g. a funder's staff alongside the university team).
creator_row_ui <- function(i) {
  div(id = paste0("creator_row_", i), class = "assay-row",
      fluidRow(
        column(3, textInput(paste0("eml_creator_given_", i), "Given name")),
        column(3, textInput(paste0("eml_creator_sur_", i), "Surname")),
        column(3, textInput(paste0("eml_creator_email_", i), "Email")),
        column(2, textInput(paste0("eml_creator_position_", i), "Position (optional)")),
        column(1, style = "padding-top: 32px;",
               actionButton(paste0("remove_creator_", i), bsicons::bs_icon("x-lg"), class = "btn-sm btn-outline-danger"))
      ),
      fluidRow(
        column(3, textInput(paste0("eml_creator_org_", i), "Organization (optional)")),
        column(3, textInput(paste0("eml_creator_address_", i), "Street address (optional)")),
        column(2, textInput(paste0("eml_creator_city_", i), "City (optional)")),
        column(2, textInput(paste0("eml_creator_admin_area_", i), "State/region (optional)")),
        column(1, textInput(paste0("eml_creator_country_", i), "Country")),
        column(1, textInput(paste0("eml_creator_phone_", i), "Phone"))
      )
  )
}

# One Step 6 project-personnel row's markup - same insertUI/removeUI
# pattern as creator_row_ui. Unlike creators, personnel SHARE the
# "Organization / address" block below (matches the reference sample:
# project personnel used the same UWA address as the associated party,
# not a separate one) - each row just needs its own name/position/
# email/role, since one project commonly lists several people in
# different roles (point of contact, curator, etc.).
personnel_row_ui <- function(i) {
  div(id = paste0("personnel_row_", i), class = "assay-row",
      fluidRow(
        column(3, textInput(paste0("eml_personnel_given_", i), "Given name")),
        column(3, textInput(paste0("eml_personnel_sur_", i), "Surname")),
        column(3, textInput(paste0("eml_personnel_email_", i), "Email")),
        column(2, textInput(paste0("eml_personnel_position_", i), "Position (optional)")),
        column(1, style = "padding-top: 32px;",
               actionButton(paste0("remove_personnel_", i), bsicons::bs_icon("x-lg"), class = "btn-sm btn-outline-danger"))
      ),
      fluidRow(
        column(3, textInput(paste0("eml_personnel_role_", i), "Role",
                             value = "POINT_OF_CONTACT", placeholder = "e.g. POINT_OF_CONTACT, CURATOR, AUTHOR"))
      )
  )
}

# One Step 6 associated-party row's markup - same pattern as
# personnel_row_ui (shares the "Organization / address" block). A
# dataset commonly credits several people here, each in a DIFFERENT
# role - the full role vocabulary EML/GBIF recognise is listed in the
# dropdown below so every "position type" this project might use
# (author, editor, reviewer, processor, curator, programmer, content
# provider, ...) is visible as an option, not just typed from memory.
ap_row_ui <- function(i) {
  div(id = paste0("ap_row_", i), class = "assay-row",
      fluidRow(
        column(3, textInput(paste0("eml_ap_given_", i), "Given name (optional)")),
        column(3, textInput(paste0("eml_ap_sur_", i), "Surname (optional)")),
        column(3, textInput(paste0("eml_ap_email_", i), "Email (optional)")),
        column(2, textInput(paste0("eml_ap_position_", i), "Position (optional)")),
        column(1, style = "padding-top: 32px;",
               actionButton(paste0("remove_ap_", i), bsicons::bs_icon("x-lg"), class = "btn-sm btn-outline-danger"))
      ),
      fluidRow(
        column(4, selectInput(paste0("eml_ap_role_", i), "Role", choices = c(
          "PUBLISHER", "AUTHOR", "CONTENT_PROVIDER", "CURATOR", "EDITOR",
          "OWNER", "POINT_OF_CONTACT", "PROCESSOR", "PROGRAMMER", "REVIEWER"
        )))
      )
  )
}

# =======================================================================
# UI
# =======================================================================
ui <- page_fluid(
  theme = app_theme,
  title = "Multi-Assay Toolkit (MAT)",
  tags$head(
    tags$link(rel = "icon", type = "image/svg+xml", href = "mat_hex.svg"),
    tags$meta(name = "description", content = "Multi-Assay Toolkit (MAT): publish multi-assay eDNA metabarcoding data in FAIRe format as one Darwin Core Archive for OBIS and GBIF."),
    tags$style(HTML(app_css)),
    tags$style(HTML(about_css)),
    tags$style(HTML(footer_css)),
    # Scientific-looking progress popups (DNA helix + scrolling sequence)
    tags$link(rel = "stylesheet", href = "mat_progress.css"),
    tags$script(src = "mat_progress.js"),
    # Shiny's own fileInput ships an INLINE style with !important
    # (position: absolute !important; top: -99999px !important) to
    # hide the native <input type=file> off-screen. Inline !important
    # beats anything in an external stylesheet, even with !important
    # there too - only JS can override an element's own inline style
    # at runtime, which is what causes the browser to try to scroll
    # that -99999px-positioned element into view on focus/click,
    # yanking the whole page to the top. Re-pin every file input to
    # sit inside its own (clipped, relatively-positioned) button
    # instead, on load and whenever Shiny binds a new one (covers
    # rows added later via insertUI too).
    tags$script(HTML("
      function fixFileInputPosition(el) {
        el.style.setProperty('position', 'absolute', 'important');
        el.style.setProperty('top', '0', 'important');
        el.style.setProperty('left', '0', 'important');
        el.style.setProperty('width', '100%', 'important');
        el.style.setProperty('height', '100%', 'important');
        el.style.setProperty('opacity', '0', 'important');
        var btn = el.closest('.btn-file');
        if (btn) { btn.style.position = 'relative'; btn.style.overflow = 'hidden'; }
      }
      document.addEventListener('shiny:bound', function(e) {
        var inp = e.target.querySelector ? e.target.querySelector('input[type=file]') : null;
        if (inp) fixFileInputPosition(inp);
      });
      document.addEventListener('DOMContentLoaded', function() {
        document.querySelectorAll('input[type=file]').forEach(fixFileInputPosition);
      });

      // 'Stay signed in': the token lives in this browser's localStorage and is
      // sent once per connection; the server checks it (R/user_auth.R). Every
      // storage call is guarded - private windows can block localStorage.
      $(document).on('shiny:connected', function() {
        var v = '';
        try { v = window.localStorage.getItem('mat_session') || ''; } catch (e) {}
        Shiny.setInputValue('auth_resume', v, {priority: 'event'});
      });
      Shiny.addCustomMessageHandler('mat_store_session', function(msg) {
        try { window.localStorage.setItem('mat_session', msg.value); } catch (e) {}
      });
      Shiny.addCustomMessageHandler('mat_clear_session', function(msg) {
        try { window.localStorage.removeItem('mat_session'); } catch (e) {}
      });

      // Home/About buttons switch tabs; start the new tab at the top.
      Shiny.addCustomMessageHandler('scroll_top', function(msg) { window.scrollTo(0, 0); });

      // Step 2: grey out Continue (and show the hint) while an answer box has
      // unchecked text. Retries briefly because the message can arrive in the
      // same flush that (re)creates the buttons.
      Shiny.addCustomMessageHandler('faire_step2_lock', function(msg) {
        var tries = 0;
        (function apply() {
          var btn = document.getElementById('to_step3');
          var hint = document.getElementById('step2_pending_hint');
          if (!btn && tries++ < 20) { setTimeout(apply, 50); return; }
          if (btn) btn.disabled = !!msg.blocked;
          if (hint) hint.style.display = msg.pending ? '' : 'none';
        })();
      });
    "))
  ),

  # Optional background picture, shown very faintly behind the page.
  # www/page_bg.jpg is gitignored (not ours to redistribute), so a fork
  # simply gets the plain background.
  if (file.exists("www/page_bg.jpg")) div(class = "page-photo", `aria-hidden` = "true"),

  div(class = "app-header",
      div(class = "brand-mark", tags$img(src = "mat_hex.svg", height = "46", alt = "Multi-Assay Toolkit (MAT) logo")),
      div(
        h1("Multi-Assay Toolkit (MAT)"),
        p("Publish multi-assay eDNA metabarcoding data as one Darwin Core Archive for OBIS and GBIF")
      ),
      div(class = "header-spacer"),
      conditionalPanel(
        condition = "output.is_logged_in == true",
        div(class = "d-flex align-items-center gap-2",
            span(style = "font-size:0.78rem; color:#d6e6f5; opacity:0.9;", textOutput("logged_in_as", inline = TRUE)),
            actionButton("logout_btn", tagList(bsicons::bs_icon("box-arrow-right"), " Log out"),
                         class = "btn btn-outline-light btn-sm restart-btn"),
            actionButton("restart_app", tagList(bsicons::bs_icon("arrow-counterclockwise"), " Restart"),
                         class = "btn btn-outline-light btn-sm restart-btn")
        )
      ),
      conditionalPanel(
        condition = "output.is_guest == true",
        div(class = "d-flex align-items-center gap-2",
            conditionalPanel(
              condition = "output.accounts_enabled == true",
              div(class = "d-flex align-items-center gap-2",
                  span(style = "font-size:0.78rem; color:#d6e6f5; opacity:0.9;", bsicons::bs_icon("person"), " Browsing without an account"),
                  actionButton("guest_login_btn", "Log in", class = "btn btn-outline-light btn-sm restart-btn"))
            ),
            conditionalPanel(
              condition = "output.accounts_enabled != true",
              span(style = "font-size:0.78rem; color:#d6e6f5; opacity:0.9;",
                   title = "No storage or accounts connected. Archives are built in this session and downloaded to your computer.",
                   bsicons::bs_icon("laptop"), " Local mode")
            ),
            actionButton("restart_app", tagList(bsicons::bs_icon("arrow-counterclockwise"), " Restart"),
                         class = "btn btn-outline-light btn-sm restart-btn")
        )
      )
  ),

  # Top-level tab bar: "Generate" (the step wizard), "Draft" (saved
  # work-in-progress archives) and "Publish" (archives ready to send to
  # OBIS), independent of wherever the wizard currently is. Each tab's
  # content lives in a dedicated uiOutput so switching tabs never disturbs
  # wizard state, the same reasoning as each step having its own renderUI.
  # Auth gate: while output.app_visible is not TRUE (nobody logged in AND not
  # browsing as a guest), only the login/signup form below is shown. A guest
  # (rv$guest_mode) DOES see the real app - see the Step 7 / Draft tab
  # button rendering for how download/save/publish are still gated
  # per-action for a guest and per-role for a logged-in user; nothing in
  # Step 1-6 needs gating, since those steps only affect this browser's own
  # in-memory session, no shared state.
  conditionalPanel(
    condition = "output.app_visible != true && output.auth_checked == true",
    div(class = "auth-wrap", div(style = "width: 100%; max-width: 440px;", uiOutput("auth_gate")))
  ),

  conditionalPanel(
    condition = "output.app_visible == true",
  navset_tab(
    id = "main_tab",
    selected = "Home",
    nav_panel("Home", home_page_ui()),
    nav_panel(
      "Generate",
      div(uiOutput("stepper_ui")),

      div(class = "content-wrap",
          # Step 1 is fully static (no renderUI) - its assay rows are
          # managed imperatively via insertUI/removeUI, and must never be
          # regenerated wholesale (see architecture note above).
          conditionalPanel(
            condition = "output.current_step_num == '1'",
            card(
              section_header("folder2-open", "Project", "Name this publication and add optional location and sequence details"),
              card_body(
                textInput("project_id", "Project ID", value = "", placeholder = "e.g. OcOm_2408"),
                p(class = "muted", "Names saved drafts, the published archive and the report (e.g. OcOm_2408 -> OcOm_2408_MAT_DwCA.zip)."),
                div(class = "optional-fields",
                  textInput("georeference_sources", width = "100%",
                    optional_label("GPS or positioning system",
                      "The equipment used to record each sample's latitude and longitude. It's added to every sample as georeferenceSources,",
                      " so people using the data know how accurate the positions are.",
                      " Example: \"Applanix PosMV WaveMaster (primary), Seapath 380+ (secondary)\" on a research vessel,",
                      " or \"Garmin GPSMAP 66 handheld GPS\" for shore sampling. Leave blank if you don't know."),
                    value = "", placeholder = "e.g. Applanix PosMV WaveMaster"),
                  textInput("associated_sequences_uri", width = "100%",
                    optional_label("Raw sequence archive link (ENA or SRA)",
                      "A link to where the raw sequencing reads are publicly archived, so others can re-analyse them.",
                      " It's added to every record as associatedSequences, and as a sentence at the end of the dataset abstract",
                      " (and methods) in eml.xml, so it shows on the dataset's OBIS and GBIF page.",
                      " Example: https://www.ebi.ac.uk/ena/browser/view/PRJEB107937 (an ENA project).",
                      " Only add it once the reads are public; leave blank if they haven't been released yet."),
                    value = "", placeholder = "e.g. https://www.ebi.ac.uk/ena/browser/view/PRJEB107937"),
                  textInput("vessel_name", width = "100%",
                    optional_label("Vessel",
                      "The ship the samples were collected on. MAT adds it to every sample in an extra file,",
                      " the extendedMeasurementOrFact (eMoF) extension, as \"Sampling platform name\" - the way the OBIS manual",
                      " recommends recording a vessel. It's also added as a sentence to the methods in eml.xml.",
                      " Example: \"RV Investigator\". Leave blank for shore or small-boat sampling."),
                    value = "", placeholder = "e.g. RV Investigator"),
                  textInput("vessel_code_uri", width = "100%",
                    optional_label("Vessel code link (ICES platform code)",
                      "The vessel's entry in the international ICES platform code list (NERC vocabulary C17), so OBIS can link",
                      " the records to that exact ship. Find it by searching the vessel name at vocab.nerc.ac.uk/search_nvs/C17/.",
                      " Example for RV Investigator: http://vocab.nerc.ac.uk/collection/C17/current/096U/.",
                      " Only used together with Vessel."),
                    value = "", placeholder = "e.g. http://vocab.nerc.ac.uk/collection/C17/current/096U/"),
                  textInput("voyage_id", width = "100%",
                    optional_label("Voyage ID",
                      "The voyage (cruise) code the samples were collected on, so people can find the voyage record and its other data.",
                      " It's added to every sample in the eMoF file as its own \"Voyage\" row, and to the eml.xml methods.",
                      " Example: \"IN2024_V04\" (the format RV Investigator voyage codes use)."),
                    value = "", placeholder = "e.g. IN2024_V04")
                )
              )
            ),
            card(
              section_header("file-earmark-spreadsheet", "Assay files", "One FAIRe workbook per assay, all describing the same samples"),
              card_body(
                radioButtons("reference_db", "Type of FAIRe files for this publication",
                             choices = REFERENCE_DB_CHOICES, selected = "curated", inline = TRUE),
                div(class = "alert alert-info py-2",
                    bsicons::bs_icon("info-circle-fill"), " ",
                    tags$b("Use one type only."),
                    " Every assay file in a publication must be a curated-database file OR an NCBI nt file - never a mix of the two. ",
                    "Choose the type above, then upload the matching files (the filename should contain \"curateddb\" or \"nt\")."),
                p(class = "muted", "Add one FAIRe .xlsx file per assay run on this project's samples. All assays must share the same physical samples."),
                div(id = "assay_rows_container", assay_row_ui(1)),
                actionButton("add_assay", tagList(bsicons::bs_icon("plus-lg"), " Add another assay"), class = "btn-outline-secondary btn-sm")
              )
            ),
            div(style = "text-align: right; margin-top: 10px;",
                actionButton("to_step2", tagList("Validate files ", bsicons::bs_icon("arrow-right")), class = "btn-primary")
            )
          ),

          conditionalPanel(condition = "output.current_step_num == '2'",
            card(
              section_header("funnel", "Sample handling", "Which samples MAT publishes, and which controls it leaves out"),
              card_body(
                div(class = "auto-callout",
                    bsicons::bs_icon("magic"),
                    div(tags$b("MAT removes your control samples automatically."),
                        " You don't need to delete them from your FAIRe files first. Only real samples go into the",
                        " Darwin Core Archive; negative and positive controls are lab and field quality checks,",
                        " not biodiversity records, so OBIS and GBIF don't accept them.")),
                loading_output("sample_split_ui", "Checking your samples..."),
                p(class = "muted mb-0 mt-3",
                  "Source: GBIF Metabarcoding Data Toolkit (MDT) user guide, \"Before you start\": ",
                  "\"Only real biological or environmental samples should be published.\" ",
                  tags$a(href = MDT_CONTROLS_GUIDE_URL, target = "_blank", rel = "noopener noreferrer",
                         "Read the guidance ", bsicons::bs_icon("box-arrow-up-right")))
              )
            ),
            loading_output("step2_body", "Loading validation results...")),
          conditionalPanel(condition = "output.current_step_num == '3'", uiOutput("step3_body")),
          conditionalPanel(condition = "output.current_step_num == '4'", uiOutput("step4_body")),
          conditionalPanel(condition = "output.current_step_num == '5'", uiOutput("step5_body")),
          conditionalPanel(condition = "output.current_step_num == '6'", uiOutput("step6_body")),
          conditionalPanel(condition = "output.current_step_num == '7'", uiOutput("step7_body"))
      )
    ),
    nav_panel(
      "Draft",
      div(class = "content-wrap", uiOutput("draft_tab_body"))
    ),
    nav_panel(
      "Publish",
      div(class = "content-wrap", uiOutput("publish_tab_body"))
    ),
    # Name lookup, available at any time (not tied to a wizard step).
    nav_panel(
      "Search",
      div(class = "content-wrap",
        card(
          card_header(bsicons::bs_icon("search", class = "section-icon"), "Search a taxon"),
          card_body(
            p(class = "muted", "Look up any scientific name or WoRMS AphiaID. The result opens with tabs for WoRMS, OBIS, FishBase or SeaLifeBase, GBIF and Wikipedia - ",
              "useful for checking a name before or while you prepare a dataset."),
            div(class = "d-flex gap-2 align-items-start",
                div(style = "flex: 1;", textInput("taxon_query", NULL, placeholder = "e.g. Gadus morhua  or  126436", width = "100%")),
                actionButton("taxon_lookup_btn", tagList(bsicons::bs_icon("search"), " Search"), class = "btn-primary")),
            # Enter in the box runs the search too.
            tags$script(HTML("$(document).on('keydown', '#taxon_query', function(e) { if (e.key === 'Enter') { $('#taxon_lookup_btn').click(); } });"))
          )
        )
      )
    ),
    nav_panel("About", about_page_ui()),
    # Hidden/shown by an observer (see server) based on whether the logged-in
    # user is an admin - starts hidden (nobody is logged in yet at page
    # load). The uiOutput itself ALSO checks is_admin() server-side before
    # rendering anything, as a second line of defence in case the tab is
    # ever reached some other way (e.g. someone re-enables it via the
    # browser console) - see the "visual gate isn't real security" note on
    # the login overlay above; the same reasoning applies here.
    nav_panel(
      "User Management",
      value = "User Management",
      div(class = "content-wrap", uiOutput("user_mgmt_body"))
    )
    )
  ),

  app_footer_ui()
)

# =======================================================================
# Server
# =======================================================================
server <- function(input, output, session) {

  rv <- reactiveValues(
    logged_in       = FALSE,
    logged_in_email = NULL,
    auth_checked    = FALSE,  # TRUE once the browser has reported any "stay signed in" token
    session_token   = NULL,   # this browser's stay-signed-in token, so logout can cancel it
    guest_mode      = FALSE,  # browsing without logging in - can use the wizard (steps 1-6) but not download/save/publish
    auth_view       = "login",  # login | signup | signup_verify | forgot | forgot_reset
    auth_pending_email = NULL,  # email the verify/reset form is currently acting on
    auth_message    = NULL,     # list(type = "success"/"error", text = "...") shown in the auth card
    current_step    = 1,
    assay_ids       = c(1),   # currently-visible assay row ids, in order added
    next_assay_id   = 2,      # ever-increasing - ids are never reused
    creator_ids     = c(1),   # currently-visible Step 6 creator row ids, in order added
    next_creator_id = 2,      # ever-increasing - ids are never reused
    personnel_ids     = c(1), # currently-visible Step 6 project-personnel row ids
    next_personnel_id = 2,    # ever-increasing - ids are never reused
    ap_ids          = c(1),   # currently-visible Step 6 associated-party row ids (can go to zero - optional)
    next_ap_id      = 2,      # ever-increasing - ids are never reused
    validation      = NULL,
    validated_answers = list(georeference_sources = NA_character_, associated_sequences_uri = NA_character_), # answers as of the last validation run - a box that differs from this has unchecked text in it
    answers         = list(georeference_sources = NA_character_, associated_sequences_uri = NA_character_),
    build_result    = NULL,   # list(event_core, controls, occurrence, dna_extension)
    build_error     = NULL,
    worms_result       = NULL,   # match_worms() output
    taxonomy_rechecks  = 0,      # times "Apply & re-check" has been run on the current build
    name_corrections   = character(),  # user-entered corrections for unmatched names, this session
    use_accepted_names = character(),  # synonyms the user switched to WoRMS' accepted name
    auto_id_off        = character(),  # auto-resolved names whose chosen WoRMS ID the reviewer left to OBIS
    unknown_names      = character(),  # unmatched names recorded as "Biota incertae sedis"
    keep_names         = character(),  # unmatched names kept as-is, not yet in WoRMS
    manual_aphia_overrides = c(),      # user-chosen AphiaIDs for ambiguous names, this session
    allow_uneven    = FALSE,         # user confirmed some samples weren't analysed with every assay (Step 2)
    excluded_taxa   = character(),  # matched names the user excluded from the archive (likely contamination), this session
    qc_result      = NULL,  # run_qc_checks() output
    eml_xml         = NULL   # build_eml_xml() output (Step 6)
  )

  # =====================================================================
  # Login / signup / forgot password (R/user_auth.R has the actual logic -
  # everything here is just the UI/wiring for it)
  # =====================================================================
  output$is_logged_in <- reactive({ isTRUE(rv$logged_in) })
  outputOptions(output, "is_logged_in", suspendWhenHidden = FALSE)

  output$is_guest <- reactive({ isTRUE(rv$guest_mode) && !isTRUE(rv$logged_in) })
  outputOptions(output, "is_guest", suspendWhenHidden = FALSE)

  # Gates the auth-screen-vs-real-app conditionalPanels: a logged-in user OR
  # a guest sees the app; anyone else sees only the login/signup screen.
  output$app_visible <- reactive({ isTRUE(rv$logged_in) || isTRUE(rv$guest_mode) })
  outputOptions(output, "app_visible", suspendWhenHidden = FALSE)

  # Local mode (no S3 bucket or no email configured, e.g. a fresh clone):
  # there are no accounts, so skip the login screen and go straight in.
  if (!accounts_enabled()) rv$guest_mode <- TRUE
  output$accounts_enabled <- reactive({ accounts_enabled() })
  outputOptions(output, "accounts_enabled", suspendWhenHidden = FALSE)

  # Home / About navigation buttons (R/about_page.R)
  go_to_tab <- function(tab) {
    bslib::nav_select("main_tab", tab, session = session)
    session$sendCustomMessage("scroll_top", list())
  }
  observeEvent(input$home_go_generate,  go_to_tab("Generate"))
  observeEvent(input$about_go_generate, go_to_tab("Generate"))
  observeEvent(input$home_go_about,     go_to_tab("About"))
  observeEvent(input$footer_about,      go_to_tab("About"))
  # Footer policy links (R/footer_policies.R)
  observeEvent(input$footer_privacy, showModal(policy_modal("privacy")))
  observeEvent(input$footer_terms,   showModal(policy_modal("terms")))
  observeEvent(input$footer_data,    showModal(policy_modal("data")))

  observeEvent(input$auth_continue_guest, { rv$guest_mode <- TRUE })
  # From the header's "Log in" button while browsing as a guest - returns to
  # the login screen WITHOUT touching wizard progress (rv$current_step,
  # rv$build_result, etc. are untouched, so logging in picks up where they
  # left off).
  observeEvent(input$guest_login_btn, { rv$guest_mode <- FALSE; auth_switch_view("login") })
  # Shared "Log in now" button inside login_required_modal() - see its definition above.
  observeEvent(input$auth_prompt_login_btn, {
    removeModal()
    rv$guest_mode <- FALSE
    auth_switch_view("login")
  })

  output$logged_in_as <- renderText({
    req(rv$logged_in_email)
    paste0("Signed in as ", rv$logged_in_email)
  })

  auth_switch_view <- function(view, email = NULL, message = NULL) {
    rv$auth_view <- view
    rv$auth_pending_email <- email
    rv$auth_message <- message
  }

  auth_message_box <- function() {
    m <- rv$auth_message
    if (is.null(m)) return(NULL)
    div(class = if (identical(m$type, "success")) "alert alert-success" else "alert alert-danger",
        m$text)
  }

  output$auth_gate <- renderUI({
    icon_for_view <- switch(rv$auth_view,
                             login = "box-arrow-in-right", signup = "person-plus",
                             signup_verify = "envelope-check", forgot = "key",
                             forgot_reset = "shield-lock")
    card(
      class = "auth-card",
      card_header(
        div(class = "auth-icon", bsicons::bs_icon(icon_for_view)),
        div(class = "auth-title",
            switch(rv$auth_view,
                   login          = "Welcome back",
                   signup         = "Create an account",
                   signup_verify  = "Verify your email",
                   forgot         = "Forgot password",
                   forgot_reset   = "Reset your password"))
      ),
      card_body(
        auth_message_box(),
        switch(rv$auth_view,
          login = tagList(
            textInput("auth_login_email", "Email", placeholder = paste0("you@", allowed_email_domain())),
            passwordInput("auth_login_password", "Password"),
            checkboxInput("auth_remember", paste0("Keep me signed in on this browser for ", SESSION_TTL_DAYS, " days"), value = TRUE),
            div(class = "d-grid mt-2", actionButton("auth_login_submit", "Log in", class = "btn-primary")),
            p(class = "muted mt-3 text-center",
              actionLink("auth_goto_forgot", "Forgot password?"), " · ",
              actionLink("auth_goto_signup", paste0("Create an account (@", allowed_email_domain(), " only)"))),
            div(class = "auth-guest-box",
                bsicons::bs_icon("person"), " ",
                actionLink("auth_continue_guest", "Continue without an account"), " - you can go through the whole process ",
                "and download the finished archive. An account is only needed to save drafts or publish to this team's storage.")
          ),
          signup = tagList(
            p(class = "muted", paste0("Only @", allowed_email_domain(), " email addresses can sign up. ",
                                      "Accounts give access to this team's saved and published archives, so they are limited to its own staff.")),
            textInput("auth_signup_email", "Email", placeholder = paste0("you@", allowed_email_domain())),
            passwordInput("auth_signup_password", "Password", placeholder = "At least 8 characters"),
            passwordInput("auth_signup_password2", "Confirm password"),
            div(class = "d-grid", actionButton("auth_signup_submit", "Send verification code", class = "btn-primary")),
            p(class = "muted mt-3", "Already have an account? ", actionLink("auth_goto_login", "Log in")),
            p(class = "muted", "Not part of this organisation, or need a different level of access? Contact ",
              tags$a(href = paste0("mailto:", contact_email()), contact_email()), ".")
          ),
          signup_verify = tagList(
            p(class = "muted", "Enter the 6-digit code sent to ", tags$b(rv$auth_pending_email), "."),
            textInput("auth_verify_code", "Verification code", placeholder = "123456"),
            div(class = "d-grid gap-2",
                actionButton("auth_verify_submit", "Verify & finish", class = "btn-primary"),
                actionButton("auth_verify_resend", "Re-send code", class = "btn-outline-secondary btn-sm")),
            p(class = "muted mt-3", actionLink("auth_goto_login", "Back to login"))
          ),
          forgot = tagList(
            p(class = "muted", "Enter your email and we'll send a reset code."),
            textInput("auth_forgot_email", "Email", placeholder = paste0("you@", allowed_email_domain())),
            div(class = "d-grid", actionButton("auth_forgot_submit", "Send reset code", class = "btn-primary")),
            p(class = "muted mt-3", actionLink("auth_goto_login", "Back to login"))
          ),
          forgot_reset = tagList(
            p(class = "muted", "Enter the code sent to ", tags$b(rv$auth_pending_email), " and choose a new password."),
            textInput("auth_reset_code", "Reset code", placeholder = "123456"),
            passwordInput("auth_reset_password", "New password", placeholder = "At least 8 characters"),
            passwordInput("auth_reset_password2", "Confirm new password"),
            div(class = "d-grid gap-2",
                actionButton("auth_reset_submit", "Reset password", class = "btn-primary"),
                actionButton("auth_reset_resend", "Re-send code", class = "btn-outline-secondary btn-sm")),
            p(class = "muted mt-3", actionLink("auth_goto_login", "Back to login"))
          )
        )
      )
    )
  })

  observeEvent(input$auth_goto_login,  auth_switch_view("login"))
  observeEvent(input$auth_goto_signup, auth_switch_view("signup"))
  observeEvent(input$auth_goto_forgot, auth_switch_view("forgot"))

  observeEvent(input$auth_login_submit, {
    res <- attempt_login(input$auth_login_email, input$auth_login_password)
    if (isTRUE(res$ok)) {
      email <- tolower(trimws(input$auth_login_email))
      rv$logged_in <- TRUE
      rv$logged_in_email <- email
      rv$auth_message <- NULL
      if (isTRUE(input$auth_remember)) {
        tok <- tryCatch(create_login_token(email), error = function(e) NULL)
        if (!is.null(tok)) {
          rv$session_token <- tok$token
          session$sendCustomMessage("mat_store_session", list(value = paste0(email, "|", tok$token)))
        }
      }
    } else {
      rv$auth_message <- list(type = "error", text = res$message)
    }
  })

  # Sent by the browser on every (re)connect: "email|token", or "" if none.
  # A valid token signs the user straight back in; an invalid or expired one
  # is cleared from the browser. Until this arrives the login screen stays
  # hidden (output.auth_checked), so a signed-in reload doesn't flash it.
  observeEvent(input$auth_resume, {
    v <- input$auth_resume %||% ""
    if (nzchar(v) && accounts_enabled() && !isTRUE(rv$logged_in)) {
      email <- sub("\\|.*$", "", v)
      token <- sub("^[^|]*\\|", "", v)
      ok <- tryCatch(check_login_token(email, token), error = function(e) FALSE)
      if (ok) {
        rv$logged_in <- TRUE
        rv$logged_in_email <- tolower(trimws(email))
        rv$guest_mode <- FALSE
        rv$session_token <- token
      } else {
        session$sendCustomMessage("mat_clear_session", list())
      }
    }
    rv$auth_checked <- TRUE
  })
  # Safety net: if the browser never reports (scripts blocked, very slow
  # connection), show the login form anyway after 2.5 s.
  local({
    first <- TRUE
    observe({
      if (first) { first <<- FALSE; invalidateLater(2500) } else isolate(rv$auth_checked <- TRUE)
    })
  })
  output$auth_checked <- reactive({ isTRUE(rv$auth_checked) })
  outputOptions(output, "auth_checked", suspendWhenHidden = FALSE)

  observeEvent(input$auth_signup_submit, {
    if (!identical(input$auth_signup_password, input$auth_signup_password2)) {
      rv$auth_message <- list(type = "error", text = "Passwords don't match.")
      return()
    }
    res <- withProgress(message = "Sending verification code...", value = 0.5,
                         start_signup(input$auth_signup_email, input$auth_signup_password))
    if (isTRUE(res$ok)) {
      auth_switch_view("signup_verify", email = tolower(trimws(input$auth_signup_email)),
                        message = list(type = "success", text = res$message))
    } else {
      rv$auth_message <- list(type = "error", text = res$message)
    }
  })

  observeEvent(input$auth_verify_submit, {
    req(rv$auth_pending_email)
    res <- verify_signup_code(rv$auth_pending_email, input$auth_verify_code)
    if (isTRUE(res$ok)) {
      auth_switch_view("login", message = list(type = "success", text = res$message))
    } else {
      rv$auth_message <- list(type = "error", text = res$message)
    }
  })

  observeEvent(input$auth_verify_resend, {
    req(rv$auth_pending_email)
    res <- withProgress(message = "Sending...", value = 0.5, resend_signup_code(rv$auth_pending_email))
    rv$auth_message <- list(type = if (isTRUE(res$ok)) "success" else "error", text = res$message)
  })

  observeEvent(input$auth_forgot_submit, {
    email <- tolower(trimws(input$auth_forgot_email))
    res <- withProgress(message = "Sending...", value = 0.5, start_password_reset(email))
    # start_password_reset() always returns ok = TRUE with the same generic
    # message (see its own comment) so this form never reveals which emails
    # have accounts.
    auth_switch_view("forgot_reset", email = email, message = list(type = "success", text = res$message))
  })

  observeEvent(input$auth_reset_resend, {
    req(rv$auth_pending_email)
    res <- withProgress(message = "Sending...", value = 0.5, start_password_reset(rv$auth_pending_email))
    rv$auth_message <- list(type = "success", text = res$message)
  })

  observeEvent(input$auth_reset_submit, {
    req(rv$auth_pending_email)
    if (!identical(input$auth_reset_password, input$auth_reset_password2)) {
      rv$auth_message <- list(type = "error", text = "Passwords don't match.")
      return()
    }
    res <- reset_password(rv$auth_pending_email, input$auth_reset_code, input$auth_reset_password)
    if (isTRUE(res$ok)) {
      auth_switch_view("login", message = list(type = "success", text = res$message))
      if (isTRUE(res$role_was_reset)) {
        showModal(modalDialog(
          title = tagList(bsicons::bs_icon("shield-exclamation"), " Access level reset"),
          p("For security, resetting your password through “Forgot password” also resets your account back to ",
            tags$b("Normal user"), " (download and save to Draft only)."),
          p("If you need publisher or admin access again, ask an admin to restore it from User Management."),
          footer = modalButton("Got it"), easyClose = TRUE
        ))
      }
    } else {
      rv$auth_message <- list(type = "error", text = res$message)
    }
  })

  observeEvent(input$logout_btn, {
    # Logging out ends "stay signed in" for this browser.
    if (!is.null(rv$session_token)) tryCatch(revoke_login_token(rv$logged_in_email, rv$session_token), error = function(e) NULL)
    rv$session_token <- NULL
    session$sendCustomMessage("mat_clear_session", list())
    rv$logged_in <- FALSE
    rv$logged_in_email <- NULL
    auth_switch_view("login")
  })

  # =====================================================================
  # User Management tab (admins only)
  # =====================================================================
  # Shows/hides the TAB ITSELF. Runs once at server start too (rv$logged_in
  # is FALSE then), so the tab starts hidden before anyone logs in, not just
  # after someone without admin logs in.
  observe({
    admin_now <- isTRUE(rv$logged_in) && is_admin(rv$logged_in_email)
    if (admin_now) bslib::nav_show("main_tab", "User Management", session = session)
    else bslib::nav_hide("main_tab", "User Management", session = session)
  })

  user_mgmt_refresh <- reactiveVal(0)

  output$user_mgmt_body <- renderUI({
    user_mgmt_refresh()
    # Server-side check, not just the hidden tab - see the comment on the
    # nav_panel definition above.
    req(isTRUE(rv$logged_in), is_admin(rv$logged_in_email))

    users_df <- list_users()
    other_emails <- setdiff(users_df$email, tolower(trimws(rv$logged_in_email)))

    role_row <- function(icon, name, access, cant) {
      div(class = "assay-row",
          div(class = "d-flex align-items-center gap-2", bsicons::bs_icon(icon), strong(name)),
          tags$ul(style = "margin: 6px 0 0 0;",
                  tags$li(tags$b("Can: "), access),
                  if (!is.null(cant)) tags$li(tags$b("Can't: "), cant)))
    }

    tagList(
      card(
        card_header(bsicons::bs_icon("info-circle", class = "section-icon"), "What each role can do"),
        card_body(
          role_row("person", "Normal user (default for every new signup)",
                    "Go through the whole process (Steps 1-7), download the archive, and save it to Draft.",
                    "Move a draft into Publish, or publish directly from Step 7."),
          role_row("send", "Publisher",
                    "Everything a normal user can, plus: choose “Publish” in Step 7, and move a draft into Publish from the Draft tab.",
                    "Manage accounts or change anyone's role."),
          role_row("shield-lock", "Admin",
                    "Everything a publisher can, plus: see this tab and change anyone else's role (except their own, and except a seed admin's - both are blocked here on purpose to prevent lockouts).",
                    NULL),
          div(class = "alert alert-info py-2 mt-2",
              bsicons::bs_icon("info-circle-fill"), " ",
              "Someone outside ", tags$b(paste0("@", allowed_email_domain())), " can't sign up at all - if they need access, point them to ",
              tags$a(href = paste0("mailto:", contact_email()), contact_email()), ".")
        )
      ),
      card(
        card_header(bsicons::bs_icon("people", class = "section-icon"), "Accounts"),
        card_body(
          if (nrow(users_df) == 0) p(class = "muted", "No accounts yet.")
          else DTOutput("user_mgmt_table"),
          div(style = "text-align: right; margin-top: 10px;",
              actionButton("user_mgmt_refresh_btn", tagList(bsicons::bs_icon("arrow-repeat"), " Refresh"), class = "btn-outline-secondary btn-sm"))
        )
      ),
      card(
        card_header(bsicons::bs_icon("person-gear", class = "section-icon"), "Change a role"),
        card_body(
          if (length(other_emails) == 0) {
            p(class = "muted", "No other accounts to manage yet.")
          } else {
            tagList(
              selectInput("user_mgmt_target_email", "Account", choices = other_emails),
              radioButtons("user_mgmt_new_role", "New role",
                           choices = c("Normal user" = "user", "Publisher" = "publisher", "Admin" = "admin"), inline = TRUE),
              actionButton("user_mgmt_update_role", "Update role", class = "btn-primary")
            )
          }
        )
      )
    )
  })

  output$user_mgmt_table <- renderDT({
    user_mgmt_refresh()
    req(isTRUE(rv$logged_in), is_admin(rv$logged_in_email))
    datatable(list_users(), rownames = FALSE, options = list(pageLength = 15, dom = "tp"))
  })

  observeEvent(input$user_mgmt_update_role, {
    req(isTRUE(rv$logged_in), is_admin(rv$logged_in_email))  # belt-and-braces: the tab is already hidden otherwise
    res <- set_user_role(rv$logged_in_email, input$user_mgmt_target_email, input$user_mgmt_new_role)
    showNotification(res$message, type = if (isTRUE(res$ok)) "message" else "error", duration = 6)
    if (isTRUE(res$ok)) user_mgmt_refresh(user_mgmt_refresh() + 1)
  })

  observeEvent(input$user_mgmt_refresh_btn, {
    user_mgmt_refresh(user_mgmt_refresh() + 1)
  })

  # Bumped after any save/publish/move (or a Refresh click) to re-list the
  # Draft and Publish tables without requiring a manual page refresh.
  history_refresh <- reactiveVal(0)

  output$current_step_num <- renderText({ as.character(rv$current_step) })
  outputOptions(output, "current_step_num", suspendWhenHidden = FALSE)

  # ---- Stepper --------------------------------------------------------
  output$stepper_ui <- renderUI({
    items <- list()
    for (i in seq_along(STEPS)) {
      state <- if (i %in% ACTIVE_STEPS && i < rv$current_step) "done"
                else if (i == rv$current_step) "active"
                else if (!(i %in% ACTIVE_STEPS)) "disabled"
                else "todo"
      circle_content <- if (state == "done") bsicons::bs_icon("check-lg") else as.character(i)
      items[[length(items) + 1]] <- div(
        class = "step-item",
        div(class = paste("step-circle", state), circle_content),
        div(class = paste("step-label", if (state == "active") "active" else ""), STEPS[i])
      )
      if (i < length(STEPS)) {
        line_state <- if (i %in% ACTIVE_STEPS && i < rv$current_step) "done" else "todo"
        items[[length(items) + 1]] <- div(class = paste("step-line", line_state))
      }
    }
    div(class = "stepper-flex", items)
  })

  # =====================================================================
  # STEP 1 - Upload & Configure (assay row management)
  # =====================================================================
  setup_remove_handler <- function(id) {
    local({
      this_id <- id
      observeEvent(input[[paste0("remove_assay_", this_id)]], {
        if (length(rv$assay_ids) <= 1) {
          showNotification("At least one assay is required.", type = "warning")
          return()
        }
        removeUI(selector = paste0("#assay_row_", this_id))
        rv$assay_ids <- setdiff(rv$assay_ids, this_id)
      }, ignoreInit = TRUE, once = TRUE)
    })
  }
  # Auto-detect the assay name from experimentRunMetadata$assay_name the
  # moment a file is uploaded into a row - pre-fills the name field but
  # leaves it fully editable, and only overwrites in response to a file
  # actually being (re-)chosen, never on its own.
  setup_assay_name_autodetect <- function(id) {
    local({
      this_id <- id
      observeEvent(input[[paste0("assay_file_", this_id)]], {
        file_val <- input[[paste0("assay_file_", this_id)]]
        req(file_val)
        file_db <- detect_reference_db_from_filename(file_val$name)
        if (!is.na(file_db) && !is.null(input$reference_db) && file_db != input$reference_db) {
          showNotification(
            paste0("This looks like a \"", names(REFERENCE_DB_CHOICES)[REFERENCE_DB_CHOICES == file_db],
                   "\" file, but the selected type is \"",
                   names(REFERENCE_DB_CHOICES)[REFERENCE_DB_CHOICES == input$reference_db],
                   "\". Change the selection above or upload a matching file."),
            type = "warning", duration = 8
          )
        }
        detected <- tryCatch(detect_assay_name_combined(file_val$datapath, file_val$name), error = function(e) NA_character_)
        if (!is.na(detected)) {
          updateTextInput(session, paste0("assay_name_", this_id), value = detected)
        } else {
          showNotification(
            paste0("Couldn't auto-detect an assay name for this file - enter one manually."),
            type = "warning", duration = 4
          )
        }
      }, ignoreInit = TRUE)
    })
  }

  setup_remove_handler(1)          # the row already in the static UI
  setup_assay_name_autodetect(1)

  observeEvent(input$add_assay, {
    new_id <- rv$next_assay_id
    insertUI(selector = "#assay_rows_container", where = "beforeEnd", ui = assay_row_ui(new_id))
    setup_remove_handler(new_id)
    setup_assay_name_autodetect(new_id)
    rv$assay_ids <- c(rv$assay_ids, new_id)
    rv$next_assay_id <- new_id + 1
  })

  # ---- Step 6 creator row management (same insertUI/removeUI pattern) ----
  setup_creator_remove_handler <- function(id) {
    local({
      this_id <- id
      observeEvent(input[[paste0("remove_creator_", this_id)]], {
        if (length(rv$creator_ids) <= 1) {
          showNotification("At least one creator is required.", type = "warning")
          return()
        }
        removeUI(selector = paste0("#creator_row_", this_id))
        rv$creator_ids <- setdiff(rv$creator_ids, this_id)
      }, ignoreInit = TRUE, once = TRUE)
    })
  }
  setup_creator_remove_handler(1)  # the row already in the static UI

  observeEvent(input$add_creator, {
    new_id <- rv$next_creator_id
    insertUI(selector = "#creator_rows_container", where = "beforeEnd", ui = creator_row_ui(new_id))
    setup_creator_remove_handler(new_id)
    rv$creator_ids <- c(rv$creator_ids, new_id)
    rv$next_creator_id <- new_id + 1
  })

  # Rebuilds the creator rows to match a freshly-uploaded metadata Excel's
  # "creators" sheet: clears every row but the first (kept and reused so
  # there's always at least one), adds however many more are needed, then
  # returns the id list so the caller can updateTextInput() each one.
  set_creator_rows <- function(n) {
    for (id in setdiff(rv$creator_ids, 1)) removeUI(selector = paste0("#creator_row_", id))
    rv$creator_ids <- c(1)
    n <- max(n, 1)
    while (length(rv$creator_ids) < n) {
      new_id <- rv$next_creator_id
      insertUI(selector = "#creator_rows_container", where = "beforeEnd", ui = creator_row_ui(new_id))
      setup_creator_remove_handler(new_id)
      rv$creator_ids <- c(rv$creator_ids, new_id)
      rv$next_creator_id <- new_id + 1
    }
    rv$creator_ids
  }

  # ---- Step 6 project-personnel row management (same pattern again) -----
  setup_personnel_remove_handler <- function(id) {
    local({
      this_id <- id
      observeEvent(input[[paste0("remove_personnel_", this_id)]], {
        if (length(rv$personnel_ids) <= 1) {
          showNotification("At least one person is required (or remove the Project ID to omit personnel entirely).", type = "warning")
          return()
        }
        removeUI(selector = paste0("#personnel_row_", this_id))
        rv$personnel_ids <- setdiff(rv$personnel_ids, this_id)
      }, ignoreInit = TRUE, once = TRUE)
    })
  }
  setup_personnel_remove_handler(1)

  observeEvent(input$add_personnel, {
    new_id <- rv$next_personnel_id
    insertUI(selector = "#personnel_rows_container", where = "beforeEnd", ui = personnel_row_ui(new_id))
    setup_personnel_remove_handler(new_id)
    rv$personnel_ids <- c(rv$personnel_ids, new_id)
    rv$next_personnel_id <- new_id + 1
  })

  set_personnel_rows <- function(n) {
    for (id in setdiff(rv$personnel_ids, 1)) removeUI(selector = paste0("#personnel_row_", id))
    rv$personnel_ids <- c(1)
    n <- max(n, 1)
    while (length(rv$personnel_ids) < n) {
      new_id <- rv$next_personnel_id
      insertUI(selector = "#personnel_rows_container", where = "beforeEnd", ui = personnel_row_ui(new_id))
      setup_personnel_remove_handler(new_id)
      rv$personnel_ids <- c(rv$personnel_ids, new_id)
      rv$next_personnel_id <- new_id + 1
    }
    rv$personnel_ids
  }

  # ---- Step 6 associated-party row management (same pattern, but this
  # one is allowed to go all the way to ZERO rows - associated parties
  # are fully optional, unlike creators/personnel) ----------------------
  setup_ap_remove_handler <- function(id) {
    local({
      this_id <- id
      observeEvent(input[[paste0("remove_ap_", this_id)]], {
        removeUI(selector = paste0("#ap_row_", this_id))
        rv$ap_ids <- setdiff(rv$ap_ids, this_id)
      }, ignoreInit = TRUE, once = TRUE)
    })
  }
  setup_ap_remove_handler(1)

  observeEvent(input$add_ap, {
    new_id <- rv$next_ap_id
    insertUI(selector = "#ap_rows_container", where = "beforeEnd", ui = ap_row_ui(new_id))
    setup_ap_remove_handler(new_id)
    rv$ap_ids <- c(rv$ap_ids, new_id)
    rv$next_ap_id <- new_id + 1
  })

  # Removes every currently-visible row (whatever ids they happen to be -
  # row 1 might already be gone, since ap rows can be removed down to
  # zero) and inserts exactly `n` fresh ones with new ids. Simpler and
  # more robust than reusing id 1 specially, which only worked for
  # creators/personnel because those can never be removed below one.
  set_ap_rows <- function(n) {
    for (id in rv$ap_ids) removeUI(selector = paste0("#ap_row_", id))
    rv$ap_ids <- integer(0)
    while (length(rv$ap_ids) < n) {
      new_id <- rv$next_ap_id
      insertUI(selector = "#ap_rows_container", where = "beforeEnd", ui = ap_row_ui(new_id))
      setup_ap_remove_handler(new_id)
      rv$ap_ids <- c(rv$ap_ids, new_id)
      rv$next_ap_id <- new_id + 1
    }
    rv$ap_ids
  }

  get_input_files <- reactive({
    files <- list()
    for (i in rv$assay_ids) {
      name_val <- input[[paste0("assay_name_", i)]]
      file_val <- input[[paste0("assay_file_", i)]]
      if (!is.null(name_val) && nzchar(name_val) && !is.null(file_val)) {
        files[[name_val]] <- file_val$datapath
      }
    }
    files
  })

  # Step 1 "Sample handling": what MAT will publish vs remove, from the
  # first uploaded FAIRe file (all assays share the same samples).
  sample_split <- reactive({
    path <- NULL
    for (i in rv$assay_ids) {
      file_val <- input[[paste0("assay_file_", i)]]
      if (!is.null(file_val)) { path <- file_val$datapath; break }
    }
    if (is.null(path)) return(NULL)
    tryCatch(summarise_sample_split(path, SAMPLE_CATEGORY_KEEP),
             error = function(e) list(error = conditionMessage(e)))
  })

  # Do all uploaded files list the same samples? (Step 2 blocks if not.)
  sample_match <- reactive({
    paths <- list()
    for (i in rv$assay_ids) {
      file_val <- input[[paste0("assay_file_", i)]]
      if (!is.null(file_val)) paths[[file_val$name]] <- file_val$datapath
    }
    if (length(paths) < 2) return(NULL)
    tryCatch(compare_sample_files(paths), error = function(e) NULL)
  })

  sample_match_ui <- function() {
    m <- sample_match()
    if (is.null(m)) return(NULL)
    pa <- m$per_assay
    if (m$same) {
      return(div(class = "split-match ok", bsicons::bs_icon("check-circle-fill"),
                 paste0(" All ", nrow(pa), " files list the same ", m$n_union, " samples (controls included).")))
    }
    div(class = "alert alert-warning",
        tags$b(bsicons::bs_icon("exclamation-triangle"), " Your files don't list the same samples."),
        tags$ul(class = "mb-1 mt-1",
          lapply(seq_len(nrow(pa)), function(k) {
            bits <- c(
              if (pa$n_missing[k] > 0) paste0("missing ", pa$n_missing[k], ": ", paste(head(pa$missing[[k]], 10), collapse = ", "),
                                              if (pa$n_missing[k] > 10) " ..."),
              if (pa$n_duplicated[k] > 0) paste0("listed twice: ", paste(head(pa$duplicated[[k]], 10), collapse = ", "))
            )
            tags$li(tags$b(pa$assay[k]), " - ", pa$n_samples[k], " samples",
                    if (length(bits)) paste0(" (", paste(bits, collapse = "; "), ")"))
          })),
        "This is usually a mistake (a typo in samp_name, or an older or wrong file) - if so, fix the sampleMetadata sheets and upload again.",
        " If some samples really weren't analysed with an assay (for example discarded), confirm that in the box below and MAT will",
        " still publish all assays together. A sample listed twice must always be fixed.")
  }

  # Collapsed by default - read it only if you want the detail.
  how_split_works <- tags$details(class = "split-details",
    tags$summary("How MAT tells samples and controls apart"),
    tags$ol(class = "split-steps mt-2",
      tags$li("It opens the ", tags$b("sampleMetadata"), " sheet of your FAIRe file (one row per sample, named in ",
              tags$code("samp_name"), ")."),
      tags$li("It reads the ", tags$code("samp_category"), " column on each row."),
      tags$li("Rows marked exactly ", tags$code(SAMPLE_CATEGORY_KEEP), " are ", tags$b("published"),
              ". Every other value (", tags$code("negative control"), ", ", tags$code("positive control"),
              ", anything else, or a blank cell) is ", tags$b("removed"),
              ", along with all of that row's detections in every assay.")),
    p(class = "muted mb-2", "All your assay files must describe the same samples, so MAT counts them from the first file",
      " you upload and checks that every other file lists exactly the same samp_names. Step 2 also compares their sample details",
      " (dates, coordinates and so on) and stops you if anything differs."))

  output$sample_split_ui <- renderUI({
    s <- sample_split()
    if (is.null(s)) {
      return(tagList(
        div(class = "split-empty", bsicons::bs_icon("upload"),
            " Go back to Step 1 and upload a FAIRe file to see which samples MAT will publish and which controls it will remove."),
        how_split_works))
    }
    if (!is.null(s$error)) {
      return(div(class = "alert alert-warning mb-0", "Couldn't read sampleMetadata from this file: ", s$error))
    }
    n_kept <- length(s$kept); n_removed <- nrow(s$removed)
    name_list <- function(x) div(class = "split-names", paste(x, collapse = ", "))

    tagList(
      tags$h6(class = "split-heading", "What MAT found in your file"),
      div(class = "split-tiles",
          div(class = "split-tile kept", div(class = "num", n_kept), div("samples will be published")),
          div(class = "split-tile removed", div(class = "num", n_removed), div("controls / other rows will be removed"))),
      tags$table(class = "table table-sm split-table",
        tags$thead(tags$tr(tags$th("samp_category value"), tags$th("Rows"), tags$th("What MAT does"))),
        tags$tbody(lapply(seq_len(nrow(s$categories)), function(k) {
          r <- s$categories[k, ]
          tags$tr(tags$td(tags$code(r$samp_category)), tags$td(r$n),
                  tags$td(if (r$kept) span(class = "badge text-bg-success", "Published")
                          else span(class = "badge text-bg-secondary", "Removed")))
        }))),
      sample_match_ui(),
      if (length(s$near_misses) > 0)
        div(class = "alert alert-warning",
            bsicons::bs_icon("exclamation-triangle"), " ",
            "Some rows say ", tags$b(paste0("\"", s$near_misses, "\"", collapse = ", ")),
            ", which looks like a sample but isn't exactly ", tags$code(SAMPLE_CATEGORY_KEEP),
            ", so MAT will remove them. If they are real samples, change the value to ", tags$code(SAMPLE_CATEGORY_KEEP),
            " in your FAIRe file and upload it again."),
      how_split_works,
      tags$details(class = "split-details",
        tags$summary(paste0("Show the ", n_kept, " published sample names")), name_list(s$kept)),
      if (n_removed > 0)
        tags$details(class = "split-details",
          tags$summary(paste0("Show the ", n_removed, " removed rows")),
          {
            groups <- split(s$removed$samp_name, s$removed$samp_category)
            lapply(names(groups), function(g)
              div(tags$b(g, paste0(" (", length(groups[[g]]), ")")), name_list(groups[[g]])))
          })
    )
  })

  # Start reading the files for Step 2's Sample handling card as soon as
  # they're uploaded on Step 1, so it's usually ready when Step 2 opens.
  outputOptions(output, "sample_split_ui", suspendWhenHidden = FALSE)

  get_input_filenames <- reactive({
    names_list <- list()
    for (i in rv$assay_ids) {
      name_val <- input[[paste0("assay_name_", i)]]
      file_val <- input[[paste0("assay_file_", i)]]
      if (!is.null(name_val) && nzchar(name_val) && !is.null(file_val)) {
        names_list[[name_val]] <- file_val$name
      }
    }
    names_list
  })

  observeEvent(input$to_step2, {
    input_files <- get_input_files()

    if (length(input_files) == 0) {
      showNotification("Upload at least one assay file with a name before continuing.", type = "error")
      return()
    }

    rv$allow_uneven <- FALSE   # new validation from Step 1: confirm again if needed
    # Optional Step 1 fields: blank = not supplied (NA), never a placeholder.
    opt <- function(x) { x <- trimws(x %||% ""); if (nzchar(x)) x else NA_character_ }
    rv$answers <- list(georeference_sources     = opt(input$georeference_sources),
                       associated_sequences_uri = opt(input$associated_sequences_uri),
                       vessel_name              = opt(input$vessel_name),
                       vessel_code_uri          = opt(input$vessel_code_uri),
                       voyage_id                = opt(input$voyage_id))
    withProgress(message = "Validating uploaded files...", value = 0.3, {
      rv$validation <- tryCatch(
        validate_faire_files(
          input_files               = input_files,
          sample_category_keep      = SAMPLE_CATEGORY_KEEP,
          georeference_sources      = rv$answers$georeference_sources,
          associated_sequences_uri  = rv$answers$associated_sequences_uri,
          reference_db              = input$reference_db,
          input_filenames           = get_input_filenames(),
          allow_uneven_samples      = rv$allow_uneven
        ),
        error = function(e) {
          showNotification(paste("Validation error:", conditionMessage(e)), type = "error", duration = NULL)
          NULL
        }
      )
      incProgress(0.7)
    })

    if (!is.null(rv$validation)) {
      rv$validated_answers <- rv$answers
      rv$current_step <- 2
    }
  })

  # =====================================================================
  # STEP 2 - Validate & Fix
  # =====================================================================
  output$step2_body <- renderUI({
    if (is.null(rv$validation)) return(p("Run validation from Step 1 first."))

    issues <- rv$validation$issues
    if (length(issues) == 0) {
      return(tagList(
        div(class = "alert alert-success d-flex align-items-center gap-2",
            bsicons::bs_icon("check-circle-fill"), "All checks passed - no missing fields or inconsistencies found."),
        div(class = "nav-row",
            actionButton("back_to_1_from_2", tagList(bsicons::bs_icon("arrow-left"), " Back"), class = "btn-outline-secondary"),
            actionButton("to_step3", tagList("Continue ", bsicons::bs_icon("arrow-right")), class = "btn-primary"))
      ))
    }

    tagList(
      lapply(issues, render_issue_card),
      div(class = "nav-row",
          actionButton("back_to_1_from_2", tagList(bsicons::bs_icon("arrow-left"), " Back"), class = "btn-outline-secondary"),
          div(
            actionButton("revalidate", tagList(bsicons::bs_icon("arrow-repeat"), " Re-check"), class = "btn-outline-secondary"),
            actionButton("to_step3", tagList("Continue ", bsicons::bs_icon("arrow-right")), class = "btn-primary",
                         disabled = isTRUE(rv$validation$any_blocking))
          )
      ),
      if (isTRUE(rv$validation$any_blocking))
        p(class = "muted", style = "text-align: right;", "Resolve the error(s) above before continuing."),
      # Shown/hidden by the step2_pending observer below (not re-rendered
      # here, so typing in a box never redraws the boxes and steals focus).
      p(id = "step2_pending_hint", class = "muted", style = "text-align: right; display: none;",
        "You've entered something that hasn't been checked - click Re-check to continue.")
    )
  })

  # Empty or normalised for comparing what's typed in an answer box against
  # the value the last validation ran with.
  norm_answer <- function(x) if (is.null(x) || length(x) == 0 || is.na(x[1])) "" else trimws(x[1])

  # TRUE while any answer box holds text that differs from what the last
  # Re-check/validation used. An untouched (or emptied back to blank) box
  # is not pending, so users with nothing to enter can continue straight away.
  step2_pending <- reactive({
    req(rv$validation)
    any(vapply(rv$validation$issues, function(issue) {
      if (issue$fix_type != "config_question") return(FALSE)
      typed <- input[[paste0("answer_", issue$id)]]
      if (is.null(typed)) return(FALSE)
      norm_answer(typed) != norm_answer(rv$validated_answers[[issue$field]])
    }, logical(1)))
  })

  observe({
    req(rv$validation, rv$current_step == 2)
    blocked <- isTRUE(rv$validation$any_blocking) || isTRUE(step2_pending())
    session$sendCustomMessage("faire_step2_lock", list(
      blocked = blocked,
      pending = !isTRUE(rv$validation$any_blocking) && isTRUE(step2_pending())
    ))
  })

  render_issue_card <- function(issue) {
    card(
      class = paste("issue-card", if (issue$severity == "warning") "warning" else ""),
      card_body(
        span(class = if (issue$severity == "error") "badge bg-danger" else "badge bg-warning text-dark",
             toupper(issue$severity)),
        h5(issue$title, style = "margin-top: 8px;"),
        p(issue$message),
        if (issue$fix_type == "config_question") {
          tagList(
            textInput(paste0("answer_", issue$id), NULL,
                      value = rv$answers[[issue$field]] %||% "",
                      placeholder = "Type your answer here", width = "100%"),
            actionButton(paste0("save_", issue$id), "Save answer", class = "btn-sm btn-primary")
          )
        } else if (issue$fix_type == "confirm_uneven_samples") {
          div(class = "uneven-confirm",
            checkboxInput("confirm_uneven", width = "100%", value = isTRUE(rv$allow_uneven),
              label = "Yes - these samples were not analysed with that assay (for example discarded). Publish all assays together."),
            p(class = "muted mb-0", "MAT will then publish every sample once and record on each affected sample which assays",
              " were not run. Leave this unticked if the difference is a mistake, and fix the files instead."))
        } else if (issue$fix_type == "data_table") {
          tagList(
            DTOutput(paste0("table_", issue$id)),
            p(class = "muted", "Double-click a cell to edit it, then click Re-check above.")
          )
        } else {
          NULL
        }
      )
    )
  }

  observe({
    req(rv$validation)
    for (issue in rv$validation$issues) {
      if (issue$fix_type == "data_table") {
        local({
          this_issue <- issue
          tbl_id <- paste0("table_", this_issue$id)
          output[[tbl_id]] <- renderDT({
            datatable(this_issue$data, editable = TRUE, rownames = FALSE,
                      options = list(pageLength = 10, dom = "tp"))
          })
          proxy <- dataTableProxy(tbl_id)
          observeEvent(input[[paste0(tbl_id, "_cell_edit")]], {
            edit <- input[[paste0(tbl_id, "_cell_edit")]]
            new_data <- this_issue$data
            new_data[edit$row, edit$col + 1] <- edit$value
            replaceData(proxy, new_data, resetPaging = FALSE, rownames = FALSE)
          })
        })
      }
    }
  })

  observe({
    req(rv$validation)
    for (issue in rv$validation$issues) {
      if (issue$fix_type == "config_question") {
        local({
          this_issue <- issue
          observeEvent(input[[paste0("save_", this_issue$id)]], {
            val <- input[[paste0("answer_", this_issue$id)]]
            rv$answers[[this_issue$field]] <- val
            showNotification(paste0("Saved: ", this_issue$field, " - click Re-check to confirm"), type = "message", duration = 3)
          }, ignoreInit = TRUE)
        })
      }
    }
  })

  observeEvent(input$revalidate, {
    # Re-check re-draws the answer boxes from rv$answers, so anything typed
    # but not yet saved would vanish (and the warning would stay). Save what
    # is typed first so Re-check does what it looks like it does.
    for (issue in rv$validation$issues) {
      if (issue$fix_type == "config_question") {
        typed <- input[[paste0("answer_", issue$id)]]
        if (!is.null(typed)) rv$answers[[issue$field]] <- trimws(typed)
      }
    }
    withProgress(message = "Re-checking...", value = 0.3, {
      rv$validation <- validate_faire_files(
        input_files               = get_input_files(),
        sample_category_keep      = SAMPLE_CATEGORY_KEEP,
        georeference_sources      = rv$answers$georeference_sources,
        associated_sequences_uri  = rv$answers$associated_sequences_uri,
        reference_db              = input$reference_db,
        input_filenames           = get_input_filenames(),
        allow_uneven_samples      = rv$allow_uneven
      )
      rv$validated_answers <- rv$answers
      incProgress(0.7)
    })
  })

  observeEvent(input$confirm_uneven, {
    if (identical(isTRUE(input$confirm_uneven), isTRUE(rv$allow_uneven))) return()
    rv$allow_uneven <- isTRUE(input$confirm_uneven)
    rv$validation <- validate_faire_files(
      input_files               = get_input_files(),
      sample_category_keep      = SAMPLE_CATEGORY_KEEP,
      georeference_sources      = rv$validated_answers$georeference_sources,
      associated_sequences_uri  = rv$validated_answers$associated_sequences_uri,
      reference_db              = input$reference_db,
      input_filenames           = get_input_filenames(),
      allow_uneven_samples      = rv$allow_uneven
    )
  }, ignoreInit = TRUE)

  observeEvent(input$back_to_1_from_2, { rv$current_step <- 1 })
  observeEvent(input$to_step3, {
    # Server-side guard as well as the disabled button (which a stale page or
    # a quick click could get past): unchecked text isn't what was validated.
    if (isTRUE(rv$validation$any_blocking)) return()
    if (isTRUE(step2_pending())) {
      showNotification("You've entered something that hasn't been checked - click Re-check first.", type = "warning", duration = 8)
      return()
    }
    rv$current_step <- 3
  })

  # =====================================================================
  # STEP 3 - Build Core Files
  # =====================================================================
  project_meta_raw <- reactive({
    input_files <- get_input_files()
    req(length(input_files) > 0)
    tryCatch(readxl::read_excel(input_files[[1]], sheet = "projectMetadata", col_names = TRUE), error = function(e) NULL)
  })

  project_meta_preview <- reactive({
    pm <- project_meta_raw()
    req(!is.null(pm))

    assay_cols <- grep("^assay[0-9]+$", names(pm), value = TRUE)
    preview_fields <- c(assay_name = "assay_name", target_gene = "target_gene",
                        targetTaxonomicScope = "targetTaxonomicScope",
                        forward = "pcr_primer_name_forward", reverse = "pcr_primer_name_reverse")

    preview <- lapply(assay_cols, function(col) {
      vapply(preview_fields, function(f) {
        v <- project_meta_value(pm, f, col)
        if (is.na(v)) "-" else v
      }, character(1))
    })
    names(preview) <- assay_cols
    preview
  })

  # One readable checkbox label per projectMetadata assay column.
  assay_column_label <- function(col, vals) {
    div(class = "assay-col-choice",
        div(span(class = "assay-col-id", col), strong(vals[["assay_name"]])),
        div(class = "assay-col-meta",
            span(span(class = "muted", "Target gene: "), vals[["target_gene"]]),
            span(span(class = "muted", "Taxa: "), vals[["targetTaxonomicScope"]]),
            span(span(class = "muted", "Primers: "), vals[["forward"]], " / ", vals[["reverse"]])))
  }

  output$step3_body <- renderUI({
    input_files <- get_input_files()
    if (length(input_files) == 0) return(p("Go back to Step 1 and upload files first."))

    preview <- tryCatch(project_meta_preview(), error = function(e) NULL)
    pm <- tryCatch(project_meta_raw(), error = function(e) NULL)

    tagList(
      card(
        card_header(bsicons::bs_icon("link-45deg", class = "section-icon"), "Map each assay to its projectMetadata column(s)"),
        card_body(
          p(class = "muted", "projectMetadata stores PCR/primer/sequencing details per assay column (assay1, assay2, ...). Matches are suggested below by comparing each assay's name against the primer/gene names shown - review and adjust, don't just accept blindly."),
          if (is.null(preview)) {
            div(class = "alert alert-warning", "Could not read projectMetadata from the first uploaded file - check it has that sheet.")
          } else {
            tagList(lapply(names(input_files), function(assay) {
              suggested <- if (!is.null(pm)) suggest_assay_columns(assay, pm) else character()
              div(class = "assay-row",
                  div(class = "d-flex align-items-center gap-2",
                      strong(assay),
                      if (length(suggested) > 0) {
                        span(class = "badge bg-primary-subtle text-primary-emphasis",
                             bsicons::bs_icon("stars"), " Suggested - please verify")
                      }
                  ),
                  div(class = "assay-col-choices",
                      checkboxGroupInput(paste0("map_", assay), NULL, width = "100%",
                                         choiceNames = unname(Map(assay_column_label, names(preview), preview)),
                                         choiceValues = names(preview),
                                         selected = suggested))
              )
            }))
          }
        )
      ),
      div(class = "nav-row", style = "margin-bottom: 18px;",
          actionButton("back_to_2_from_3", tagList(bsicons::bs_icon("arrow-left"), " Back"), class = "btn-outline-secondary"),
          actionButton("do_build", tagList(bsicons::bs_icon("gear-fill"), " Build Event core, Occurrence & DNA extensions"), class = "btn-primary")
      ),
      uiOutput("build_results")
    )
  })

  observeEvent(input$back_to_2_from_3, { rv$current_step <- 2 })

  observeEvent(input$do_build, {
    input_files <- get_input_files()
    assay_project_column <- setNames(
      lapply(names(input_files), function(a) input[[paste0("map_", a)]]),
      names(input_files)
    )
    if (any(vapply(assay_project_column, length, integer(1)) == 0)) {
      showNotification("Map every assay to at least one projectMetadata column before building.", type = "error")
      return()
    }

    rv$build_error <- NULL
    withProgress(message = "Building archive files...", value = 0.1, {
      result <- tryCatch({
        incProgress(0.1, detail = "Event core")
        ec <- build_event_core(
          input_files             = input_files,
          # Any file works: Step 2 already confirmed that samples shared by the files have identical details.
          event_core_source_assay = names(input_files)[1],
          sample_category_keep    = SAMPLE_CATEGORY_KEEP,
          georeference_sources    = rv$answers$georeference_sources,
          allow_uneven_samples    = rv$allow_uneven
        )

        incProgress(0.3, detail = "Occurrence extensions")
        real_event_ids <- ec$event_core$eventID
        occ <- build_occurrence(
          input_files              = input_files,
          real_event_ids           = real_event_ids,
          associated_sequences_uri = rv$answers$associated_sequences_uri
        )

        incProgress(0.4, detail = "DNA Derived Data extensions")
        dna <- build_dna_extension(
          input_files              = input_files,
          occurrence_tables        = occ$occurrence,
          assay_project_column     = assay_project_column
        )
        emof <- build_emof(
          event_ids   = ec$event_core$eventID,
          vessel_name = rv$answers$vessel_name,
          vessel_uri  = rv$answers$vessel_code_uri,
          voyage_id   = rv$answers$voyage_id
        )
        incProgress(0.2)

        list(event_core = ec$event_core, controls = ec$controls, occurrence = occ$occurrence, dna_extension = dna$dna_extension,
             emof = emof, assay_coverage = ec$assay_coverage)
      }, error = function(e) {
        rv$build_error <- conditionMessage(e)
        NULL
      })
    })

    if (!is.null(result)) {
      rv$build_result <- result
      # A rebuild invalidates any taxonomy matching / QC results
      # computed against the old data - clear them so Step 4/5 show a
      # fresh "run" prompt instead of a stale result from before.
      rv$worms_result <- NULL
      rv$taxonomy_rechecks <- 0
      rv$excluded_taxa <- character()
      rv$use_accepted_names <- character()
      rv$auto_id_off <- character()
      rv$unknown_names <- character()
      rv$keep_names <- character()
      rv$qc_result     <- NULL
      showNotification("Build complete.", type = "message")
    }
  })

  output$build_results <- renderUI({
    if (!is.null(rv$build_error)) {
      return(div(class = "alert alert-danger", strong("Build failed: "), rv$build_error))
    }
    req(rv$build_result)
    r <- rv$build_result

    stat_box <- function(value, label) div(class = "stat-box", div(class = "stat-value", format(value, big.mark = ",")), div(class = "stat-label", label))

    tagList(
      card(
        card_header(bsicons::bs_icon("check-circle", class = "section-icon"), "Build results"),
        card_body(
          div(class = "d-flex flex-wrap gap-3",
              stat_box(nrow(r$event_core), "Event core rows"),
              stat_box(nrow(r$controls), "Controls excluded"),
              lapply(names(r$occurrence), function(a) stat_box(nrow(r$occurrence[[a]]), paste0(a, " detections")))
          )
        )
      ),
      render_mapping_card(r),
      div(style = "text-align: right;",
          actionButton("to_step4", tagList("Continue to Taxonomy Review ", bsicons::bs_icon("arrow-right")), class = "btn-primary")
      )
    )
  })

  render_mapping_card <- function(r) {
    m <- tryCatch(check_dwc_mapping(r$event_core, r$occurrence, r$dna_extension, r$emof), error = function(e) NULL)
    if (is.null(m)) return(NULL)

    n_total <- nrow(m)
    problems <- m[m$status %in% c("not_a_term", "wrong_type"), , drop = FALSE]
    notes <- m[m$status == "expected", , drop = FALSE]

    badge <- function(status) {
      switch(status,
        not_a_term = span(class = "badge bg-warning text-dark", "Not a Darwin Core term"),
        wrong_type = span(class = "badge bg-danger", "Wrong data type"),
        expected   = span(class = "badge bg-secondary", "Expected")
      )
    }
    rows_ui <- function(df) tags$table(class = "table table-sm align-middle",
      tags$thead(tags$tr(tags$th("Table"), tags$th("Column"), tags$th("Status"), tags$th("What it means"))),
      tags$tbody(lapply(seq_len(nrow(df)), function(i) {
        tags$tr(tags$td(df$table[i]), tags$td(tags$code(df$column[i])), tags$td(badge(df$status[i])), tags$td(class = "muted", df$detail[i]))
      }))
    )

    card(
      card_header(bsicons::bs_icon("signpost-split", class = "section-icon"), "Darwin Core mapping check"),
      card_body(
        p(class = "muted", "This is what the IPT mapping screen will complain about. It compares every column against the official GBIF Darwin Core definitions."),
        if (nrow(problems) == 0) {
          div(class = "alert alert-success d-flex align-items-center gap-2",
              bsicons::bs_icon("check-circle-fill"),
              paste0("All ", n_total - nrow(notes), " columns map to a Darwin Core term with a valid data type."))
        } else {
          tagList(
            div(class = "alert alert-warning",
                bsicons::bs_icon("exclamation-triangle-fill"), " ",
                paste0(nrow(problems), " column(s) will not map cleanly. Fix these in your FAIRe file or pipeline before uploading to the IPT.")),
            rows_ui(problems)
          )
        },
        if (nrow(notes) > 0) {
          tags$details(tags$summary(class = "muted", paste0(nrow(notes), " expected note(s)")), rows_ui(notes))
        }
      )
    )
  }

  observeEvent(input$to_step4, { rv$current_step <- 4 })

  # =====================================================================
  # STEP 4 - Taxonomy Review (interactive WoRMS matching)
  # =====================================================================
  run_worms_matching <- function() {
    req(rv$build_result)
    withProgress(message = "Matching scientific names against WoRMS...", value = 0.2, {
      result <- tryCatch(
        match_worms(
          occurrence_tables      = rv$build_result$occurrence,
          name_corrections       = rv$name_corrections,
          manual_aphia_overrides = rv$manual_aphia_overrides
        ),
        error = function(e) {
          showNotification(paste("WoRMS matching failed:", conditionMessage(e)), type = "error", duration = NULL)
          NULL
        }
      )
      incProgress(0.8)
    })
    if (!is.null(result)) {
      # Spelling variants and newer names both start on WoRMS' form (the
      # user's choice, 2026-11-04); the reviewer can switch any row back to
      # the original in Step 4. Only names new to this run get the default,
      # so a re-run doesn't undo a row the reviewer switched back.
      prev_syn <- rv$worms_result$synonym_df$queriedName %||% character()
      new_syn  <- setdiff(result$synonym_df$queriedName %||% character(), prev_syn)
      rv$use_accepted_names <- union(rv$use_accepted_names, new_syn)
      rv$worms_result <- result
      # Any change to taxonomy matching invalidates a previously-run QC
      # result - clear it so Step 5 re-checks fresh instead of silently
      # showing a stale pass/fail from before this change.
      rv$qc_result <- NULL
    }
  }

  observeEvent(input$run_worms, { run_worms_matching() })
  observeEvent(input$retry_worms, { run_worms_matching() })

  # Names the user excluded in Step 4 (likely contamination) are removed
  # here, by WoRMS ID, from the Occurrence tables and - via occurrenceID -
  # the DNA Derived Data tables. Everything downstream (QC, mapping check,
  # report, archive zip) reads these, never the unfiltered tables. Nothing
  # is excluded unless the user clicks Exclude.
  # Names WoRMS couldn't match, split into the Step 4 groups:
  #  misspelt_open - WoRMS's fuzzy search offered a spelling; still waiting for a choice
  #  keep          - published as identified, no WoRMS ID (names with no suggestion
  #                  land here automatically: likely taxa not yet in WoRMS)
  #  unknown       - recorded as Biota incertae sedis
  name_groups <- reactive({
    r <- rv$worms_result
    req(r)
    suggested <- names(r$fuzzy_candidates %||% as.list(r$suggested_corrections))
    misspelt  <- intersect(r$unmatched_names, suggested)
    no_match  <- setdiff(r$unmatched_names, suggested)
    unknown   <- intersect(rv$unknown_names, r$unmatched_names)
    keep      <- union(intersect(rv$keep_names, misspelt), setdiff(no_match, unknown))
    keep      <- setdiff(keep, unknown)
    list(misspelt_open = setdiff(misspelt, c(keep, unknown)), keep = keep, unknown = unknown,
         not_in_worms = c(keep, unknown), suggested = misspelt)
  })
  # The reviewer's name choices from Step 4 (switch a synonym to WoRMS'
  # accepted name; keep or record as unknown a name WoRMS doesn't have).
  named_occurrence <- reactive({
    req(rv$worms_result)
    g <- name_groups()
    apply_name_choices(rv$worms_result, rv$use_accepted_names, g$unknown, g$keep, rv$auto_id_off)
  })
  excluded_name_ids <- reactive({
    r <- rv$worms_result
    if (is.null(r) || length(rv$excluded_taxa) == 0 || nrow(r$matched_df) == 0) return(character())
    hit <- r$matched_df$queriedName %in% rv$excluded_taxa
    ids <- r$matched_df$scientificNameID[hit]
    # A name switched to its accepted name now carries the accepted ID.
    switched <- hit & r$matched_df$queriedName %in% rv$use_accepted_names & !is.na(r$matched_df$worms_valid_AphiaID)
    ids <- c(ids, paste0("urn:lsid:marinespecies.org:taxname:", r$matched_df$worms_valid_AphiaID[switched]))
    unique(ids[!is.na(ids)])
  })
  final_occurrence <- reactive({
    req(rv$worms_result)
    ids <- excluded_name_ids()
    occ <- named_occurrence()
    if (length(ids) == 0) return(occ)
    # By name too: an excluded name whose auto-chosen ID was turned off has no ID.
    no_id_names <- intersect(rv$excluded_taxa, rv$auto_id_off)
    lapply(occ, function(o) o[!(o$scientificNameID %in% ids | o$scientificName %in% no_id_names), , drop = FALSE])
  })
  final_dna <- reactive({
    req(rv$build_result)
    dna <- rv$build_result$dna_extension
    if (length(excluded_name_ids()) == 0) return(dna)
    occ <- final_occurrence()
    setNames(lapply(names(dna), function(a) {
      d <- dna[[a]]
      if (is.null(occ[[a]])) d else d[d$occurrenceID %in% occ[[a]]$occurrenceID, , drop = FALSE]
    }), names(dna))
  })
  # One sentence for the EML methods so data users know what was removed.
  exclusion_method_note <- reactive({
    if (length(rv$excluded_taxa) == 0 || is.null(rv$worms_result)) return(NULL)
    n_removed <- sum(vapply(rv$worms_result$occurrence_tables, nrow, integer(1))) -
                 sum(vapply(final_occurrence(), nrow, integer(1)))
    n_taxa <- length(rv$excluded_taxa)
    paste0("After manual review of taxa that WoRMS does not record as marine, ",
           if (n_taxa == 1) "1 taxon was" else paste(n_taxa, "taxa were"), " excluded from the published occurrences as likely contamination or misidentification (",
           format(n_removed, big.mark = ","), " occurrence records removed): ",
           paste(sort(rv$excluded_taxa), collapse = ", "), ".")
  })
  # Uneven sample lists (confirmed in Step 2): say how many samples each assay covered.
  uneven_samples_method_note <- reactive({
    cov <- rv$build_result$assay_coverage
    if (is.null(cov)) return(NULL)
    paste0("Not every sample was analysed with every assay: ",
           paste0(cov$assay, " ", cov$samples_analysed, " of ", cov$samples_total, " samples", collapse = "; "),
           ". Each sample is listed once in the Event core; samples not analysed with an assay say so in eventRemarks,",
           " and the absence of records from that assay for those samples does not mean the taxa were absent.")
  })
  # Same idea for the Step 4 name choices: what was renamed or kept as unknown.
  name_choice_method_note <- reactive({
    r <- rv$worms_result
    if (is.null(r)) return(NULL)
    notes <- character()
    syn <- r$synonym_df
    if (length(rv$use_accepted_names) > 0 && !is.null(syn) && nrow(syn) > 0) {
      s <- syn[syn$queriedName %in% rv$use_accepted_names, ]
      typo <- grepl("^misspelling", s$worms_status, ignore.case = TRUE)
      if (any(typo)) notes <- c(notes, paste0(
        "Names that WoRMS spells differently (usually a Latin ending changed to agree with the genus) were updated to the WoRMS spelling: ",
        paste0(s$queriedName[typo], " -> ", s$worms_valid_name[typo], collapse = "; "),
        ". The original identification is kept in verbatimIdentification."))
      if (any(!typo)) notes <- c(notes, paste0(
        "Names listed by WoRMS under a newer accepted name were updated to that name: ",
        paste0(s$queriedName[!typo], " -> ", s$worms_valid_name[!typo], collapse = "; "),
        ". The original identification is kept in verbatimIdentification."))
    }
    g <- name_groups()
    kept <- g$keep
    if (length(kept) > 0) notes <- c(notes, paste0(
      "Names not yet registered in WoRMS are published as identified, with scientificNameID left empty and the reference-database ",
      "taxon ID in taxonConceptID, following the OBIS guidance for DNA-derived data: ",
      paste(sort(kept), collapse = ", "), "."))
    unknown <- g$unknown
    if (length(unknown) > 0) notes <- c(notes, paste0(
      "Identifications judged unreliable and not in WoRMS are recorded as 'Biota incertae sedis' (urn:lsid:marinespecies.org:taxname:12), ",
      "with the original name kept in verbatimIdentification and identificationRemarks: ",
      paste(sort(unknown), collapse = ", "), "."))
    auto_off <- intersect(rv$auto_id_off, r$resolved_ambiguous_df$queriedName)
    if (length(auto_off) > 0) notes <- c(notes, paste0(
      "For names with several WoRMS records, the WoRMS ID was left for OBIS to assign during ingestion: ",
      paste(sort(auto_off), collapse = ", "), "."))
    if (length(notes) == 0) NULL else notes
  })

  exclude_button <- function(nm, size = "") {
    tags$button(type = "button", class = paste("btn btn-outline-danger btn-white", size), `data-taxon` = nm,
                title = "Click to exclude this taxon from the archive",
                onclick = "Shiny.setInputValue('exclude_taxon', this.getAttribute('data-taxon'), {priority: 'event'})",
                bsicons::bs_icon("x-circle"), " Exclude")
  }
  undo_exclude_button <- function(nm, size = "") {
    tags$button(type = "button", class = paste("btn btn-outline-secondary btn-white", size), `data-taxon` = nm,
                title = "Click to keep this taxon in the archive",
                onclick = "Shiny.setInputValue('include_taxon', this.getAttribute('data-taxon'), {priority: 'event'})",
                bsicons::bs_icon("arrow-counterclockwise"), " Undo")
  }

  # exclude_name: when set (a flagged non-marine name), the popup footer gets
  # Exclude / Undo so the decision can be made right after reviewing it.
  # extended: TRUE for the Check buttons (WoRMS, OBIS, FishBase/SeaLifeBase,
  # GBIF, Wikipedia tabs). The Search tab uses TRUE as well; FALSE (WoRMS + FishBase only) is unused.
  show_taxon_modal <- function(query, exclude_name = NULL, extended = TRUE) {
    query <- trimws(query %||% "")
    if (!nzchar(query)) {
      showNotification("Type a scientific name or an AphiaID first.", type = "warning")
      return()
    }
    res <- withProgress(message = "Looking up", value = 0, {
      lookup_taxon_all(query, extended = extended, progress = function(detail) incProgress(0.25, detail = detail))
    })
    footer <- if (is.null(exclude_name)) {
      modalButton("Close")
    } else if (exclude_name %in% rv$excluded_taxa) {
      tagList(span(class = "text-danger me-auto", bsicons::bs_icon("x-circle-fill"), " Excluded from the archive"),
              undo_exclude_button(exclude_name), modalButton("Close"))
    } else {
      tagList(
        span(class = "muted me-auto", "Kept unless you exclude it. Exclude only if it's likely contamination or a wrong match."),
        exclude_button(exclude_name),
        modalButton("Close")
      )
    }
    showModal(modalDialog(
      title = tagList(bsicons::bs_icon("search"), " ", query),
      size = "xl", easyClose = TRUE, footer = footer,
      taxon_lookup_body(res, query)
    ))
  }
  observeEvent(input$taxon_check, { show_taxon_modal(input$taxon_check) })
  observeEvent(input$taxon_lookup_btn, { show_taxon_modal(input$taxon_query, extended = TRUE) })
  observeEvent(input$non_marine_check, { show_taxon_modal(input$non_marine_check, exclude_name = input$non_marine_check) })
  # Changing what's excluded changes the data, so a QC result or eml.xml
  # generated before it is stale - clear them, like a WoRMS re-run does.
  set_excluded_taxa <- function(new) {
    rv$excluded_taxa <- new
    rv$qc_result <- NULL
    rv$eml_xml <- NULL
    removeModal()
  }
  observeEvent(input$exclude_taxon, { set_excluded_taxa(union(rv$excluded_taxa, input$exclude_taxon)) })
  observeEvent(input$include_taxon, { set_excluded_taxa(setdiff(rv$excluded_taxa, input$include_taxon)) })

  output$step4_body <- renderUI({
    req(rv$build_result)

    if (is.null(rv$worms_result)) {
      all_names <- unique(unlist(lapply(rv$build_result$occurrence, function(x) x$scientificName)))
      n_names <- length(all_names[!is.na(all_names)])
      return(tagList(
        card(
          card_header(bsicons::bs_icon("search", class = "section-icon"), "Match scientific names against WoRMS"),
          card_body(
            p(class = "muted", paste0(n_names, " unique scientific name(s) will be matched against the World Register of Marine Species (WoRMS).")),
            div(class = "alert alert-info small",
                strong("Why this step? "), "OBIS uses WoRMS as its main taxonomic reference, and runs its own name checks when your dataset is published. ",
                "This step shows you, before publishing, what those checks would flag:",
                tags$ul(class = "mb-2 mt-1",
                  tags$li("spelling variants and outdated names (synonyms)"),
                  tags$li("names WoRMS doesn't have yet"),
                  tags$li("names shared by different organisms, such as a fish and a plant"),
                  tags$li("taxa WoRMS doesn't record as marine")),
                "For each one you decide: fix it here, or keep the original and let OBIS handle it. Some things only you can decide - OBIS can't tell which organism a shared name means. ",
                "Nothing changes without your choice, and the original name always stays in ", tags$code("verbatimIdentification"), "."),
            actionButton("run_worms", tagList(bsicons::bs_icon("play-fill"), " Run WoRMS matching"), class = "btn-primary")
          )
        ),
        div(class = "nav-row", actionButton("back_to_3_from_4", tagList(bsicons::bs_icon("arrow-left"), " Back"), class = "btn-outline-secondary"), div())
      ))
    }

    r <- rv$worms_result
    ambiguous_names <- unique(r$ambiguous_df$queriedName)
    # Only open misspellings are issues; names not in WoRMS are listed in
    # their own section. Isolated so switching keep/unknown there doesn't
    # redraw (and reset) the choices above - step4_refresh() redraws on purpose.
    step4_refresh()
    g <- isolate(name_groups())
    unmatched_names <- g$misspelt_open
    n_not_in_worms <- length(g$not_in_worms)
    syn_status <- if (is.null(r$synonym_df) || nrow(r$synonym_df) == 0) character() else r$synonym_df$worms_status
    n_known_misspellings <- sum(is_misspelling(syn_status))
    n_synonyms <- length(syn_status) - n_known_misspellings
    n_matched <- length(unique(r$matched_df$queriedName))
    n_auto <- length(unique(r$resolved_ambiguous_df$queriedName))
    n_issues <- length(ambiguous_names) + length(unmatched_names)
    has_issues <- n_issues > 0

    stat_box <- function(value, label) div(class = "stat-box", div(class = "stat-value", value), div(class = "stat-label", label))

    failed <- r$failed_names %||% character()

    tagList(
      # WoRMS didn't answer for some names (busy/slow server). Those names
      # have no ID yet - not because they aren't in WoRMS - so Continue is
      # blocked until a re-run gets an answer for them.
      if (length(failed) > 0) {
        card(
          class = "issue-card warning",
          card_header(bsicons::bs_icon("wifi-off", class = "section-icon"), "WoRMS didn't respond for some names"),
          card_body(
            p(strong(length(failed), if (length(failed) == 1) " name was" else " names were", " not checked"),
              ", because WoRMS didn't answer in time (it is sometimes slow or busy). They don't have a WoRMS ID yet - this is not a problem with your data. ",
              "Run the matching again; your choices so far are kept."),
            tags$details(class = "muted small mb-3", tags$summary("Show the names"), paste(failed, collapse = ", ")),
            actionButton("retry_worms", tagList(bsicons::bs_icon("arrow-clockwise"), " Run WoRMS matching again"), class = "btn-primary")
          )
        )
      },
      card(
        card_header(bsicons::bs_icon("bar-chart-fill", class = "section-icon"), "Matching summary"),
        card_body(
          div(class = "d-flex flex-wrap gap-3",
              stat_box(n_matched, "Matched"),
              stat_box(n_auto, "Auto-selected IDs"),
              stat_box(length(ambiguous_names), "Need your review"),
              stat_box(n_known_misspellings + length(unmatched_names), "Spelling variants"),
              stat_box(n_synonyms, "Newer name in WoRMS"),
              stat_box(n_not_in_worms, "Not yet in WoRMS"),
              stat_box(nrow(r$non_marine_df), "Not marine in WoRMS")
          )
        )
      ),

      # Results are split into tabs so what MUST be decided ("To review") is
      # never mixed with optional or automatic items. Orange, not red: none of
      # this blocks publishing once reviewed.
      local({
        auto <- r$resolved_ambiguous_df
        auto <- if (!is.null(auto) && nrow(auto) > 0) auto[auto$resolution == "single_accepted", , drop = FALSE] else NULL
        n_auto_ids <- if (is.null(auto)) 0 else length(unique(auto$queriedName))
        n_review <- length(ambiguous_names) + length(unmatched_names)
        n_updates <- n_known_misspellings + n_synonyms
        n_non_marine <- nrow(r$non_marine_df)
        tab_label <- function(text, n, cls = "bg-secondary-subtle text-secondary-emphasis") {
          tagList(text, " ", span(class = paste("badge rounded-pill", cls), n))
        }
        nothing <- function(text) div(class = "muted py-3", bsicons::bs_icon("check-circle-fill", class = "text-success"), " ", text)
        tab_intro <- function(...) p(class = "muted mt-3 mb-3", ...)

        navset_underline(
          id = "step4_tabs",

          # ---- 1. Decisions needed ------------------------------------------
          nav_panel(
            tab_label("To review", n_review, if (n_review > 0) "bg-warning text-dark" else "bg-success"), value = "review",
            tab_intro(strong("These need your decision before you continue."), " Choose an option for each name, then click ", strong("Apply & re-check"), "."),
            if (n_review == 0) nothing("Nothing needs your review. The other tabs are for information and optional changes."),
            if (length(ambiguous_names) > 0) {
              card(
                class = "issue-card warning",
                card_header(bsicons::bs_icon("question-circle", class = "section-icon"), "Ambiguous names - pick the correct WoRMS record"),
                card_body(
                  p(class = "muted", "WoRMS has more than one accepted record with this name - usually different organisms that share a name, such as a fish and a plant. ",
                    "Only you can tell which one your data means, so pick the correct record below."),
                  tagList(lapply(ambiguous_names, function(nm) {
                    cand <- r$ambiguous_df %>% filter(queriedName == nm)
                    choices <- setNames(
                      as.character(cand$AphiaID),
                      paste0(cand$AphiaID, " — ", cand$scientificname, " (", cand$status, ", ", cand$rank, ", marine=", cand$isMarine, ")")
                    )
                    div(class = "assay-row",
                        div(class = "d-flex align-items-center gap-2", strong(nm), taxon_check_button(nm)),
                        selectInput(paste0("choose_aphia_", safe_id(nm)), NULL, choices = c("Leave unresolved" = "", choices)),
                        div(class = "muted", "Open a candidate in WoRMS: ",
                            lapply(cand$AphiaID, function(id) tagList(
                              tags$a(href = worms_taxon_url(id), target = "_blank", rel = "noopener noreferrer", paste0(id, " ↗")), " ")))
                    )
                  }))
                )
              )
            },
            if (length(unmatched_names) > 0) {
              card(
                class = "issue-card warning",
                card_header(bsicons::bs_icon("spellcheck", class = "section-icon"), "Close spellings found by WoRMS's search"),
                card_body(
                  p(class = "muted", "WoRMS has no record with these names as written, but its typo-tolerant search found one or more close names. Compare them and choose an option."),
                  tags$ul(class = "muted small mb-3",
                    tags$li(strong("Use WoRMS's spelling"), " - the name is corrected and matched. Your original stays in ", tags$code("verbatimIdentification"), "."),
                    tags$li(strong("Keep original name"), " - if the original spelling is right and the taxon just isn't in WoRMS yet. It moves to the ", em("Not yet in WoRMS"), " tab."),
                    tags$li(strong("Try another name"), " - type a different name to look up."),
                    tags$li(strong("Record as unknown"), " - published as ", tags$em("Biota incertae sedis"), ", for a name you doubt.")),
                  tagList(lapply(unmatched_names, function(nm) {
                    candidates <- r$fuzzy_candidates[[nm]] %||% unname(r$suggested_corrections[nm])
                    sid <- safe_id(nm)
                    choice_names <- c(
                      lapply(candidates, function(cand) tagList("Use WoRMS's spelling: ", tags$em(strong(cand)))),
                      list("Keep original name", "Try another name",
                           tagList("Record as unknown (", tags$em("Biota incertae sedis"), ")")))
                    choice_values <- c(paste0("s", seq_along(candidates)), "keep", "other", "unknown")
                    div(class = "assay-row",
                        div(class = "d-flex align-items-center gap-2 mb-2 flex-wrap",
                            span(class = "muted", "Original name:"), strong(tags$em(nm)),
                            bsicons::bs_icon("arrow-right", class = "muted"),
                            span(class = "muted", if (length(candidates) == 1) "WoRMS:" else paste0("WoRMS (", length(candidates), " close names):")),
                            strong(tags$em(paste(candidates, collapse = " / "))),
                            taxon_check_button(nm, paste0("correct_name_", sid))),
                        radioButtons(paste0("unmatched_choice_", sid), NULL,
                                     choiceNames = choice_names, choiceValues = choice_values,
                                     selected = if (length(candidates) == 1) "s1" else character(0)),
                        conditionalPanel(
                          condition = sprintf("input['unmatched_choice_%s'] == 'other'", sid),
                          textInput(paste0("correct_name_", sid), NULL, placeholder = "Corrected name to look up in WoRMS")
                        )
                    )
                  }))
                )
              )
            }
          ),

          # ---- 2. Optional name updates --------------------------------------
          nav_panel(
            tab_label("Name updates", n_updates), value = "updates",
            tab_intro(strong("Optional."), " WoRMS knows these names in a newer form. They are set to the ", strong("WoRMS form"), " by default. ",
                      "Keeping the original is also fine - OBIS links it to the current name when it publishes your data - so switch a row back only if you prefer your own name."),
            if (n_updates == 0) nothing("No names need updating - every matched name is already in its current WoRMS form."),
            if (n_known_misspellings > 0 && n_synonyms > 0) {
              div(class = "d-flex gap-2 align-items-center flex-wrap mb-3",
                  span(class = "muted small", strong("Change every name in this tab at once:")),
                  syn_all_btn("all", TRUE, "check2-all", "Use WoRMS names for all", class = "btn btn-sm btn-primary"),
                  syn_all_btn("all", FALSE, "arrow-counterclockwise", "Keep all original names", class = "btn btn-sm btn-outline-primary"))
            },
            if (n_known_misspellings > 0) {
              card(
                card_header(bsicons::bs_icon("spellcheck", class = "section-icon"), "Spelling variants"),
                card_body(
                  p(class = "muted", "Same species, same name - WoRMS uses a different spelling. Usually it's the Latin ending: a species name must agree in gender with its genus, ",
                    "so the ending changes when the genus is re-assessed (", tags$em("heptacanthus"), " / ", tags$em("heptacantha"), "). ",
                    "WoRMS files the older form under the technical status \"misspelling\"; it isn't a mistake in your data, just an older spelling."),
                  p(class = "muted", "These use the ", strong("WoRMS spelling"), " by default. ",
                    "Switch a row to ", strong("Original spelling"), " to keep the form in your data - it stays linked to its WoRMS record. ",
                    "Your original always stays in ", tags$code("verbatimIdentification"), ", and a change is noted in ", tags$code("identificationRemarks"), "."),
                  uiOutput("misspelling_table")
                )
              )
            },
            if (n_synonyms > 0) {
              card(
                card_header(bsicons::bs_icon("arrow-left-right", class = "section-icon"), "Names WoRMS lists under a newer name"),
                card_body(
                  p(class = "muted", "WoRMS recognises these names but now uses a different accepted name for the same taxon (for example after a genus change). Each is set to the ", strong("WoRMS accepted name"), " by default; switch a row to keep the original name instead."),
                  p(class = "muted small", strong("Keeping the original name is valid: "), "it stays linked to its WoRMS record, and OBIS shows the records under the accepted name. Switching makes the archive use the current name directly. Either way your original name is kept in ",
                    tags$code("verbatimIdentification"), ", and a switch is noted in ", tags$code("identificationRemarks"), "."),
                  uiOutput("synonym_table")
                )
              )
            }
          ),

          # ---- 3. Automatically selected WoRMS IDs ---------------------------
          nav_panel(
            tab_label("Auto-selected IDs", n_auto_ids), value = "auto",
            tab_intro(strong("Optional - nothing to fix here."), " This only affects which WoRMS ID is attached. ", strong("Your taxonomic names are not changed.")),
            if (n_auto_ids == 0) nothing("No names needed an automatic ID choice."),
            if (n_auto_ids > 0) {
              card(
                card_header(bsicons::bs_icon("check2-all", class = "section-icon"), "WoRMS IDs selected automatically"),
                card_body(
                  p(class = "muted", "WoRMS has more than one record for each of these names: one ", strong("accepted"),
                    " record, plus older entries for the same name (unaccepted, junior homonym, unassessed). MAT attached the accepted record's ID - WoRMS's own choice, not a guess."),
                  p(class = "muted", strong("Why it's worth a look: "), "it shows exactly which ID each record will carry. ",
                    "If you'd rather not choose here, switch a name to ", strong("Leave to OBIS"), ": the ID is left empty and OBIS identifies the record when it publishes your data from the IPT. ",
                    "Either way the name stays exactly as it is."),
                  uiOutput("auto_id_table")
                )
              )
            }
          ),

          # ---- 4. Names WoRMS doesn't have -----------------------------------
          nav_panel(
            tab_label("Not yet in WoRMS", n_not_in_worms), value = "not_in_worms",
            tab_intro(strong("Nothing to do here"), " unless a name looks wrong."),
            if (n_not_in_worms == 0) nothing("Every name was found in WoRMS."),
            if (n_not_in_worms > 0) {
              card(
                card_header(bsicons::bs_icon("stars", class = "section-icon"), "Not yet in WoRMS - possibly newly detected taxa"),
                card_body(
                  p(class = "muted", "WoRMS has no record of these names and no close spelling. eDNA often detects taxa that WoRMS hasn't registered yet, including newly described species."),
                  div(class = "alert alert-info py-2 small",
                      "They are published as identified: ", tags$code("scientificNameID"), " left empty, the NCBI ID in ", tags$code("taxonConceptID"),
                      ", and the original name in ", tags$code("verbatimIdentification"), ". ",
                      strong("OBIS may treat them as unknown"), " until WoRMS registers the name - at most it places them under a higher rank it can find. Rebuild the archive once WoRMS has them."),
                  uiOutput("not_in_worms_table")
                )
              )
            }
          ),

          # ---- 5. Not marine -------------------------------------------------
          nav_panel(
            tab_label("Not marine", n_non_marine), value = "not_marine",
            tab_intro(strong("Optional review."), " Everything is published unless you exclude it."),
            if (n_non_marine == 0) nothing("WoRMS records every matched name as marine."),
            if (n_non_marine > 0) {
              card(
                card_header(bsicons::bs_icon("flag", class = "section-icon"), "Not marine in WoRMS - review before publishing"),
                card_body(
                  div(class = "muted mb-3",
                      p(class = "mb-1", "These names are flagged because WoRMS doesn't record them as marine. ", strong("You can still publish them."), " ",
                        tags$a(href = "#non_marine_more", class = "read-more-link", `data-bs-toggle` = "collapse", role = "button",
                               `aria-expanded` = "false", `aria-controls` = "non_marine_more",
                               span(class = "when-collapsed", "Read more"), span(class = "when-expanded", "Show less"),
                               bsicons::bs_icon("chevron-down", class = "read-more-chevron"))),
                      div(id = "non_marine_more", class = "collapse",
                        p(class = "mb-2 mt-1", "They may live in freshwater or on land, or WoRMS may not have a habitat recorded for them yet. They go to the IPT with the rest of the data, and OBIS decides what to do with each one. OBIS reports any quality issues back to the data provider if something needs clarifying."),
                        p(class = "mb-2", strong("Birds, reptiles and land animals are normal in eDNA. "),
                          "Seawater carries DNA from seabirds, sea snakes and marine mammals, and from land animals whose DNA washes in from rivers, runoff or the shore. ",
                          "These are real detections of DNA in the water, so keep them unless you have a reason to think otherwise."),
                        p(class = "mb-1", strong("OBIS outcome"), " predicts what OBIS will do:"),
                        tags$ul(class = "mb-2",
                          tags$li(strong("Kept"), " - WoRMS records it as marine or brackish. Published normally."),
                          tags$li(strong("Kept - marked unsure"), " - WoRMS has no marine or brackish record. Published, with a \"marine unsure\" flag."),
                          tags$li(strong("Will be dropped"), " - WoRMS says it is neither marine nor brackish. Hidden from OBIS searches."))),
                      p(class = "mb-0 mt-2", strong("What to do:"), " click ", strong("Check"), " to see the taxon in WoRMS, OBIS, FishBase/SeaLifeBase, GBIF and Wikipedia. Everything is kept unless you click ",
                        strong("Exclude"), ". A bird, snake or land animal is not a reason to exclude on its own - eDNA genuinely detects them. ",
                        "Exclude only likely lab or reagent contamination (for example human or common food species such as pig, cow or chicken) or a wrong match. Exclusions are listed automatically in the eml.xml methods.")),
                  uiOutput("non_marine_table")
                )
              )
            }
          )
        )
      }),

      if (has_issues) {
        div(class = "alert alert-warning",
            bsicons::bs_icon("exclamation-triangle-fill"), " ",
            paste0(n_issues, " name(s) in the To review tab still need a decision. Choose an option for each, then click "),
            tags$b("Apply & re-check"),
            ". The other tabs are optional. Continue unlocks once nothing is left to review - choices that aren't applied are not used.",
            if (rv$taxonomy_rechecks >= 1) {
              tagList(
                hr(class = "my-2"),
                checkboxInput("allow_unresolved",
                              "I've checked the remaining name(s) and they can't be resolved - continue and publish them without a scientificNameID (clear any fix boxes first).",
                              value = FALSE),
                tags$script(HTML("$(document).off('change.taxunlock', '#allow_unresolved').on('change.taxunlock', '#allow_unresolved', function() { $('#to_step5').prop('disabled', !this.checked); });"))
              )
            }
        )
      },

      div(class = "nav-row",
          actionButton("back_to_3_from_4", tagList(bsicons::bs_icon("arrow-left"), " Back"), class = "btn-outline-secondary"),
          div(
            if (has_issues) {
              actionButton("apply_corrections", tagList(bsicons::bs_icon("arrow-repeat"), " Apply & re-check"), class = "btn-primary")
            },
            actionButton("to_step5", tagList("Continue to QC Checks ", bsicons::bs_icon("arrow-right")),
                         class = if (has_issues) "btn-outline-secondary" else "btn-primary",
                         disabled = has_issues)
          )
      )
    )
  })

  # Its own output (not part of step4_body) so excluding a name doesn't
  # re-render the whole step and wipe the ambiguous/unmatched inputs.
  output$non_marine_table <- renderUI({
    req(rv$worms_result)
    nm_df <- rv$worms_result$non_marine_df
    req(nrow(nm_df) > 0)
    excluded <- rv$excluded_taxa

    # Mirrors OBIS's own QC rule (iobis/obis-qc, obisqc/taxonomy.py): dropped
    # only when WoRMS says marine = 0 AND brackish = 0; when neither is 1
    # (e.g. both blank) it's kept with a MARINE_UNSURE flag.
    obis_outcome <- function(row) {
      m <- row$worms_isMarine; b <- row$worms_isBrackish
      if (identical(as.numeric(m), 0) && identical(as.numeric(b), 0)) {
        span(class = "badge bg-danger", "Will be dropped")
      } else if (!(m %in% 1) && !(b %in% 1)) {
        span(class = "badge bg-warning text-dark", title = "Published, with OBIS's MARINE_UNSURE flag", "Kept - marked unsure")
      } else {
        span(class = "badge bg-success-subtle text-success-emphasis", "Kept")
      }
    }
    all_occ <- dplyr::bind_rows(lapply(rv$worms_result$occurrence_tables, function(o) o[, "scientificNameID", drop = FALSE]))
    n_records <- function(id) sum(all_occ$scientificNameID %in% id)

    habitat <- function(row) {
      flags <- c(Marine = row$worms_isMarine, Brackish = row$worms_isBrackish,
                 Freshwater = row$worms_isFreshwater, Terrestrial = row$worms_isTerrestrial)
      on <- names(flags)[!is.na(flags) & flags == 1]
      if (length(on) == 0) span(class = "muted", "not recorded")
      else lapply(on, function(x) span(class = "badge bg-info-subtle text-info-emphasis me-1", x))
    }
    n_excluded <- sum(nm_df$queriedName %in% excluded)

    tagList(
      tags$table(class = "table align-middle",
        tags$thead(tags$tr(tags$th("Name"), tags$th("Rank"), tags$th("WoRMS habitat"), tags$th("OBIS outcome"),
                           tags$th(class = "text-end", "Records"), tags$th("In archive"), tags$th())),
        tags$tbody(lapply(seq_len(nrow(nm_df)), function(i) {
          row <- nm_df[i, ]
          nm <- row$queriedName
          is_excluded <- nm %in% excluded
          tags$tr(class = if (is_excluded) "text-decoration-line-through text-muted",
            tags$td(tags$em(nm),
                    if (!identical(row$worms_valid_name, nm)) div(class = "muted small", "WoRMS: ", tags$em(row$worms_valid_name))),
            tags$td(row$worms_rank),
            tags$td(habitat(row)),
            tags$td(obis_outcome(row)),
            tags$td(class = "text-end", format(n_records(row$scientificNameID), big.mark = ",")),
            tags$td(if (is_excluded) span(class = "badge bg-danger", "Excluded") else span(class = "badge bg-secondary-subtle text-secondary-emphasis", "Kept")),
            tags$td(class = "text-end text-nowrap",
              tags$button(type = "button", class = "btn btn-sm btn-outline-primary me-1", `data-taxon` = nm,
                          title = "Open the WoRMS record and FishBase page",
                          onclick = "Shiny.setInputValue('non_marine_check', this.getAttribute('data-taxon'), {priority: 'event'})",
                          bsicons::bs_icon("search"), " Check"),
              if (is_excluded) undo_exclude_button(nm, "btn-sm") else exclude_button(nm, "btn-sm"))
          )
        }))
      ),
      div(class = "muted",
          if (n_excluded == 0) "Nothing excluded - all of these will be in the DwC archive."
          else paste0(n_excluded, " of ", nrow(nm_df), " excluded. Their records are removed from the Occurrence and DNA Derived Data files, and listed in the eml.xml methods."))
    )
  })

  # Names WoRMS lists under a newer accepted name: one row per name, with a
  # "WoRMS name / Original name" switch. Its own output so a choice only
  # redraws this table, not the inputs elsewhere in Step 4.
  # WoRMS-registered misspellings (status "misspelling - ...") get their own
  # table in the Misspellings section; every other synonym is a newer name.
  is_misspelling <- function(status) grepl("^misspelling", status %||% "", ignore.case = TRUE)
  syn_rows <- function(misspellings) {
    syn <- rv$worms_result$synonym_df
    if (is.null(syn) || nrow(syn) == 0) return(syn)
    syn[is_misspelling(syn$worms_status) == misspellings, , drop = FALSE]
  }
  # "Change all at once" buttons. scope: "misspellings", "synonyms" or "all"
  # (both tables). Rows can still be switched one by one afterwards.
  syn_all_btn <- function(scope, accept, icon, label, class = "btn btn-sm btn-outline-secondary btn-white") {
    tags$button(type = "button", class = class,
                onclick = sprintf("Shiny.setInputValue('syn_all', {scope: '%s', accept: %s}, {priority: 'event'})",
                                  scope, if (accept) "true" else "false"),
                bsicons::bs_icon(icon), " ", label)
  }
  observeEvent(input$syn_all, {
    ch <- input$syn_all
    nms <- switch(ch$scope,
                  misspellings = syn_rows(TRUE)$queriedName,
                  synonyms     = syn_rows(FALSE)$queriedName,
                  rv$worms_result$synonym_df$queriedName) %||% character()
    if (length(nms) == 0) return()
    rv$use_accepted_names <- if (isTRUE(ch$accept)) union(rv$use_accepted_names, nms) else setdiff(rv$use_accepted_names, nms)
    rv$qc_result <- NULL
    rv$eml_xml <- NULL
  })
  output$synonym_table <- renderUI(syn_table_ui(syn_rows(FALSE), misspellings = FALSE))
  output$misspelling_table <- renderUI(syn_table_ui(syn_rows(TRUE), misspellings = TRUE))
  syn_table_ui <- function(syn, misspellings) {
    req(rv$worms_result, !is.null(syn), nrow(syn) > 0)
    chosen <- rv$use_accepted_names
    js <- "Shiny.setInputValue('syn_choice', {name: this.getAttribute('data-taxon'), accept: this.value === 'accepted'}, {priority: 'event'})"
    option <- function(nm, value, label, checked) {
      tags$label(class = "radio-inline",
        tags$input(type = "radio", name = paste0("syn_", safe_id(nm)), value = value, `data-taxon` = nm,
                   checked = if (checked) NA, onchange = js),
        span(label))
    }
    scope <- if (misspellings) "misspellings" else "synonyms"
    tagList(
      div(class = "d-flex gap-2 justify-content-end mb-2 flex-wrap",
          syn_all_btn(scope, TRUE, "check2-all", if (misspellings) "Use all WoRMS spellings" else "Use all WoRMS accepted names"),
          syn_all_btn(scope, FALSE, "arrow-counterclockwise", if (misspellings) "Keep all original spellings" else "Keep all original names")),
      tags$table(class = "table align-middle",
        tags$thead(tags$tr(tags$th("Original name"), tags$th(if (misspellings) "WoRMS spelling" else "WoRMS accepted name"),
                           tags$th("WoRMS status"), tags$th("Publish as"), tags$th())),
        tags$tbody(lapply(seq_len(nrow(syn)), function(i) {
          row <- syn[i, ]
          nm <- row$queriedName
          use_new <- nm %in% chosen
          tags$tr(
            tags$td(tags$em(nm)),
            tags$td(tags$em(row$worms_valid_name), if (!is.na(row$worms_valid_authority)) span(class = "muted small", " ", row$worms_valid_authority)),
            tags$td(span(class = "badge bg-secondary-subtle text-secondary-emphasis", row$worms_status)),
            tags$td(div(class = "choice-switch shiny-options-group",
                        option(nm, "accepted", if (misspellings) "WoRMS spelling" else "WoRMS accepted name", use_new),
                        option(nm, "keep", if (misspellings) "Original spelling" else "Original name", !use_new))),
            tags$td(class = "text-end", taxon_check_button(row$worms_valid_name))
          )
        }))
      ),
      div(class = "muted small", {
        n_switched <- length(intersect(chosen, syn$queriedName))
        if (misspellings && n_switched == 0) "All original spellings are kept. Switch any row to publish the WoRMS spelling instead."
        else if (misspellings) paste0(n_switched, " of ", nrow(syn), " set to the WoRMS spelling.")
        else if (n_switched == 0) "All original names are kept. Switch any row to publish the WoRMS accepted name instead."
        else paste0(n_switched, " of ", nrow(syn), " switched to the WoRMS accepted name.")
      })
    )
  }
  # Auto-selected IDs: one row per name, switch between the accepted ID MAT
  # chose and "Leave to OBIS" (ID left empty). The name never changes.
  output$auto_id_table <- renderUI({
    req(rv$worms_result)
    auto <- rv$worms_result$resolved_ambiguous_df
    req(!is.null(auto), nrow(auto) > 0)
    auto <- auto[auto$resolution == "single_accepted", , drop = FALSE]
    req(nrow(auto) > 0)
    off <- rv$auto_id_off
    js <- "Shiny.setInputValue('auto_id_choice', {name: this.getAttribute('data-taxon'), off: this.value === 'off'}, {priority: 'event'})"
    option <- function(nm, value, label, checked) {
      tags$label(class = "radio-inline",
        tags$input(type = "radio", name = paste0("autoid_", safe_id(nm)), value = value, `data-taxon` = nm,
                   checked = if (checked) NA, onchange = js),
        span(label))
    }
    all_btn <- function(off_all, icon, label) {
      tags$button(type = "button", class = "btn btn-sm btn-outline-secondary btn-white",
                  onclick = sprintf("Shiny.setInputValue('auto_id_all', {off: %s}, {priority: 'event'})", if (off_all) "true" else "false"),
                  bsicons::bs_icon(icon), " ", label)
    }
    names_auto <- unique(auto$queriedName)
    tagList(
      div(class = "d-flex gap-2 justify-content-end mb-2",
          all_btn(FALSE, "check2-all", "Use all accepted IDs"),
          all_btn(TRUE, "cloud-arrow-up", "Leave all to OBIS")),
      tags$table(class = "table align-middle",
        tags$thead(tags$tr(tags$th("Name"), tags$th("Accepted ID (selected)"), tags$th("Other WoRMS records"), tags$th("WoRMS ID in archive"), tags$th())),
        tags$tbody(lapply(names_auto, function(nm) {
          recs <- auto[auto$queriedName == nm, , drop = FALSE]
          chosen <- recs$chosenAphiaID[1]
          used <- recs[recs$AphiaID == chosen, , drop = FALSE]
          others <- recs[recs$AphiaID != chosen, , drop = FALSE]
          is_off <- nm %in% off
          tags$tr(
            tags$td(tags$em(nm)),
            tags$td(tags$a(href = worms_taxon_url(chosen), target = "_blank", rel = "noopener noreferrer", paste0(chosen, " ↗")),
                    span(class = "muted small", paste0(" ", used$rank[1]))),
            tags$td(class = "small", lapply(seq_len(nrow(others)), function(i) div(
              tags$a(href = worms_taxon_url(others$AphiaID[i]), target = "_blank", rel = "noopener noreferrer", others$AphiaID[i]),
              span(class = "muted", paste0(" ", others$status[i]))))),
            tags$td(div(class = "choice-switch shiny-options-group",
                        option(nm, "on", "Accepted ID", !is_off),
                        option(nm, "off", "Leave to OBIS", is_off))),
            tags$td(class = "text-end", taxon_check_button(nm))
          )
        }))
      ),
      div(class = "muted small",
          if (length(intersect(off, names_auto)) == 0) "Every name carries its accepted WoRMS ID."
          else paste0(length(intersect(off, names_auto)), " of ", length(names_auto),
                      " left to OBIS. Step 5 will show these as records without a WoRMS ID - that's expected, and they can still be published."))
    )
  })
  set_auto_id_off <- function(new) {
    rv$auto_id_off <- new
    rv$qc_result <- NULL
    rv$eml_xml <- NULL
  }
  observeEvent(input$auto_id_choice, {
    ch <- input$auto_id_choice
    set_auto_id_off(if (isTRUE(ch$off)) union(rv$auto_id_off, ch$name) else setdiff(rv$auto_id_off, ch$name))
  })
  observeEvent(input$auto_id_all, {
    auto <- rv$worms_result$resolved_ambiguous_df
    names_auto <- unique(auto$queriedName[auto$resolution == "single_accepted"])
    set_auto_id_off(if (isTRUE(input$auto_id_all$off)) names_auto else character())
  })

  # Switching a name changes the data, so stale QC results and eml.xml are cleared.
  observeEvent(input$syn_choice, {
    ch <- input$syn_choice
    rv$use_accepted_names <- if (isTRUE(ch$accept)) union(rv$use_accepted_names, ch$name) else setdiff(rv$use_accepted_names, ch$name)
    rv$qc_result <- NULL
    rv$eml_xml <- NULL
  })
  # "Not yet in WoRMS" list: each name is kept as identified unless recorded
  # as unknown; names that had a WoRMS spelling can go back to that section.
  output$not_in_worms_table <- renderUI({
    g <- name_groups()
    req(length(g$not_in_worms) > 0)
    act <- function(nm, mode, icon, label, title) {
      tags$button(type = "button", class = "btn btn-sm btn-outline-secondary btn-white ms-1", `data-taxon` = nm, title = title,
                  onclick = sprintf("Shiny.setInputValue('not_in_worms_set', {name: this.getAttribute('data-taxon'), mode: '%s'}, {priority: 'event'})", mode),
                  bsicons::bs_icon(icon), " ", label)
    }
    tags$table(class = "table align-middle mb-0",
      tags$thead(tags$tr(tags$th("Name"), tags$th("Published as"), tags$th())),
      tags$tbody(lapply(g$not_in_worms, function(nm) {
        is_unknown <- nm %in% g$unknown
        tags$tr(
          tags$td(tags$em(nm)),
          tags$td(if (is_unknown) span(class = "badge bg-secondary-subtle text-secondary-emphasis", "Biota incertae sedis")
                  else span(class = "badge bg-success-subtle text-success-emphasis", "As identified, no WoRMS ID")),
          tags$td(class = "text-end text-nowrap",
            taxon_check_button(nm),
            if (is_unknown) act(nm, "keep", "arrow-counterclockwise", "Keep as identified", "Publish this name as identified")
            else act(nm, "unknown", "question-circle", "Record as unknown", "Publish as Biota incertae sedis instead"),
            if (nm %in% g$suggested) act(nm, "reopen", "spellcheck", "Back to spellings", "Choose a WoRMS spelling instead"))
        )
      }))
    )
  })
  step4_refresh <- reactiveVal(0)
  observeEvent(input$not_in_worms_set, {
    nm <- input$not_in_worms_set$name
    mode <- input$not_in_worms_set$mode
    rv$unknown_names <- if (mode == "unknown") union(rv$unknown_names, nm) else setdiff(rv$unknown_names, nm)
    if (mode == "keep") rv$keep_names <- union(rv$keep_names, nm)
    if (mode %in% c("unknown", "reopen")) rv$keep_names <- setdiff(rv$keep_names, nm)
    if (mode == "reopen") step4_refresh(step4_refresh() + 1)
    rv$qc_result <- NULL
    rv$eml_xml <- NULL
  })

  observeEvent(input$apply_corrections, {
    req(rv$worms_result)
    r <- rv$worms_result
    ambiguous_names <- unique(r$ambiguous_df$queriedName)
    unmatched_names <- name_groups()$misspelt_open

    needs_rerun <- FALSE
    for (nm in ambiguous_names) {
      val <- input[[paste0("choose_aphia_", safe_id(nm))]]
      if (!is.null(val) && nzchar(val)) { rv$manual_aphia_overrides[nm] <- as.integer(val); needs_rerun <- TRUE }
    }
    for (nm in unmatched_names) {
      choice <- input[[paste0("unmatched_choice_", safe_id(nm))]] %||% ""
      candidates <- r$fuzzy_candidates[[nm]] %||% unname(r$suggested_corrections[nm])
      if (grepl("^s[0-9]+$", choice)) {
        rv$name_corrections[nm] <- candidates[as.integer(sub("^s", "", choice))]; needs_rerun <- TRUE
      } else if (choice == "other") {
        val <- trimws(input[[paste0("correct_name_", safe_id(nm))]] %||% "")
        if (nzchar(val)) { rv$name_corrections[nm] <- val; needs_rerun <- TRUE }
      } else if (choice == "keep") {
        rv$keep_names <- union(rv$keep_names, nm)
      } else if (choice == "unknown") {
        rv$unknown_names <- union(rv$unknown_names, nm)
      }
    }
    rv$taxonomy_rechecks <- rv$taxonomy_rechecks + 1
    # Only a new spelling or AphiaID needs WoRMS again; "keep" and
    # "unknown" are applied locally.
    if (needs_rerun) run_worms_matching() else { rv$qc_result <- NULL; rv$eml_xml <- NULL }
  })

  observeEvent(input$back_to_3_from_4, { rv$current_step <- 3 })
  observeEvent(input$to_step5, {
    req(rv$worms_result)
    r <- rv$worms_result
    ambiguous_names <- unique(r$ambiguous_df$queriedName)
    unmatched_names <- name_groups()$misspelt_open

    if (length(r$failed_names) > 0) {
      showNotification("WoRMS didn't respond for some names. Click \"Run WoRMS matching again\" at the top of this step first.", type = "warning", duration = 8)
      return()
    }

    if (length(ambiguous_names) > 0 || length(unmatched_names) > 0) {
      if (!isTRUE(input$allow_unresolved)) {
        showNotification("Apply your fixes and re-check first - some names still need attention.", type = "warning")
        return()
      }
      pending <- vapply(ambiguous_names, function(nm) nzchar(input[[paste0("choose_aphia_", safe_id(nm))]] %||% ""), logical(1))
      if (any(pending)) {
        showNotification("Some fixes are selected but not applied. Click Apply & re-check, or clear them to leave those names unresolved.", type = "warning", duration = 8)
        return()
      }
    }
    rv$current_step <- 5
  })

  # =====================================================================
  # STEP 5 - QC Checks
  # =====================================================================
  run_qc <- function() {
    req(rv$build_result, rv$worms_result)
    withProgress(message = "Running QC checks...", value = 0.3, {
      result <- tryCatch(
        run_qc_checks(rv$build_result$event_core, final_occurrence(), final_dna(), rv$build_result$emof),
        error = function(e) {
          showNotification(paste("QC checks failed:", conditionMessage(e)), type = "error", duration = NULL)
          NULL
        }
      )
      incProgress(0.7)
    })
    if (!is.null(result)) rv$qc_result <- result
  }

  observeEvent(input$run_qc, { run_qc() })

  output$step5_body <- renderUI({
    req(rv$build_result, rv$worms_result)

    if (is.null(rv$qc_result)) {
      return(tagList(
        card(
          card_header(bsicons::bs_icon("clipboard-check", class = "section-icon"), "Run QC checks"),
          card_body(
            p(class = "muted", "Runs obistools' validation checks against the finished archive - required fields, date/coordinate sanity, and cross-file ID consistency - before anything is downloaded."),
            actionButton("run_qc", tagList(bsicons::bs_icon("play-fill"), " Run QC checks"), class = "btn-primary")
          )
        ),
        div(class = "nav-row", actionButton("back_to_4_from_5", tagList(bsicons::bs_icon("arrow-left"), " Back"), class = "btn-outline-secondary"), div())
      ))
    }

    qc <- rv$qc_result
    r <- qc$results

    fields_ok <- all(vapply(names(r)[grepl("^check_fields_", names(r))], function(k) nrow(r[[k]]) == 0, logical(1)))
    ext_ok <- all(vapply(names(r)[grepl("^check_extension_eventids_", names(r))], function(k) {
      x <- r[[k]]; length(x) == 0 || (is.data.frame(x) && nrow(x) == 0) || all(is.na(x))
    }, logical(1)))
    crosscheck_ok <- all(vapply(r$occ_dna_crosscheck, function(x) length(x$missing_in_dna) == 0 && length(x$missing_in_occ) == 0, logical(1)))
    n_total   <- sum(vapply(r$scientificname_coverage, function(x) x$total, numeric(1)))
    n_missing <- sum(vapply(r$scientificname_coverage, function(x) x$missing, numeric(1)))

    checklist <- list(
      list(label = "Required Darwin Core fields present (Event + Occurrence)", passed = fields_ok),
      list(label = "eventID unique in Event core; parentEventID valid", passed = nrow(r$check_eventids) == 0),
      list(label = "eventDate format valid", passed = nrow(r$check_eventdate) == 0),
      list(label = "No coordinates on land", passed = nrow(r$check_onland) == 0),
      list(label = "No zero, missing, or out-of-range coordinates", passed = nrow(r$coord_out_of_range) == 0),
      list(label = "Every Occurrence/DNA row's eventID exists in the Event core", passed = ext_ok),
      list(label = "Occurrence ↔ DNA Derived Data occurrenceID consistency", passed = crosscheck_ok),
      list(label = "occurrenceID unique across all assays (archive-wide)", passed = length(r$duplicate_occurrence_ids) == 0),
      # Not a failure: names not yet in WoRMS (or IDs left to OBIS) are publishable.
      if (n_missing == 0) list(label = "Every record has a WoRMS ID (scientificNameID)", passed = TRUE)
      else list(label = paste0(format(n_total - n_missing, big.mark = ","), " of ", format(n_total, big.mark = ","),
                               " records have a WoRMS ID (scientificNameID). The rest are names not yet in WoRMS, or IDs left to OBIS - still publishable."),
                passed = TRUE, warning = TRUE)
    )

    check_row <- function(item) {
      icon <- if (isTRUE(item$warning)) bsicons::bs_icon("exclamation-circle-fill", class = "text-warning")
              else if (item$passed) bsicons::bs_icon("check-circle-fill", class = "text-success")
              else bsicons::bs_icon("x-circle-fill", class = "text-danger")
      div(class = "d-flex align-items-center gap-2", style = "padding: 7px 0;", icon, item$label)
    }

    tagList(
      card(
        card_header(bsicons::bs_icon("clipboard-check", class = "section-icon"), "QC results"),
        card_body(
          lapply(checklist, check_row),
          if (length(qc$any_skipped) > 0) {
            div(class = "alert alert-warning", style = "margin-top: 12px;",
                strong("Not verified: "), paste(qc$any_skipped, collapse = "; "))
          }
        )
      ),
      if (!qc$any_failures && isTRUE(qc$any_warnings)) {
        div(class = "alert alert-warning d-flex align-items-center gap-2",
            bsicons::bs_icon("exclamation-circle-fill"), "All required checks passed. Some records have no WoRMS ID - that's expected for names not yet in WoRMS, and the data can still be published.")
      } else if (!qc$any_failures) {
        div(class = "alert alert-success d-flex align-items-center gap-2",
            bsicons::bs_icon("check-circle-fill"), "All runnable checks passed. Still worth spot-checking a sample of rows before publishing.")
      } else {
        div(class = "alert alert-danger d-flex align-items-center gap-2",
            bsicons::bs_icon("exclamation-triangle-fill"), "Some checks failed - review above. You can still continue to see results, but resolve these before publishing.")
      },
      div(class = "nav-row",
          actionButton("back_to_4_from_5", tagList(bsicons::bs_icon("arrow-left"), " Back"), class = "btn-outline-secondary"),
          div(
            actionButton("rerun_qc", tagList(bsicons::bs_icon("arrow-repeat"), " Re-check"), class = "btn-outline-secondary"),
            actionButton("to_step6", tagList("Continue to Results ", bsicons::bs_icon("arrow-right")), class = "btn-primary")
          )
      )
    )
  })

  observeEvent(input$rerun_qc, { run_qc() })

  observeEvent(input$back_to_4_from_5, { rv$current_step <- 4 })
  observeEvent(input$to_step6, { rv$current_step <- 6 })

  # =====================================================================
  # STEP 6 - Metadata (generates eml.xml - the dataset-level metadata
  # file IPT can import directly instead of retyping everything into
  # its own metadata screens). Person fields share one organization/
  # address block since in practice everyone listed is usually at the
  # same institution - "same as creator" checkboxes cover the common
  # case where the metadata provider/contact IS the creator, without
  # forcing re-entry.
  # =====================================================================
  output$step6_body <- renderUI({
    tagList(
      card(
        card_header(bsicons::bs_icon("file-earmark-arrow-up", class = "section-icon"), "Fill from Excel (optional)"),
        card_body(
          p(class = "muted", "Prepare this metadata offline in a spreadsheet instead of the form below - download a blank template, fill it in, then upload it here to auto-fill every field. You can still edit anything by hand afterward."),
          fluidRow(
            column(6, downloadButton("download_eml_template", "Download blank template (.xlsx)", class = "btn-outline-secondary")),
            column(6, fileInput("eml_upload", "Upload a filled-in copy", accept = ".xlsx"))
          )
        )
      ),
      card(
        card_header(bsicons::bs_icon("card-text", class = "section-icon"), "Dataset"),
        card_body(
          textInput("eml_title", "Dataset title", placeholder = "e.g. OcOm_2408_16SFishD"),
          textAreaInput("eml_abstract", "Abstract (one paragraph per line)", rows = 4,
                        placeholder = "This dataset contains environmental DNA (eDNA) metabarcoding data..."),
          textInput("eml_keywords", "Keywords (comma-separated)", placeholder = "metabarcoding, DNA, eDNA, OceanOmics"),
          fluidRow(
            column(6, selectInput("eml_license", "License", choices = EML_LICENSE_CHOICES)),
            column(6, dateInput("eml_pub_date", "Publication date", value = Sys.Date()))
          ),
          textInput("eml_distribution_url", "Project/organization website (optional)", placeholder = "https://...")
        )
      ),
      card(
        card_header(bsicons::bs_icon("people-fill", class = "section-icon"), "People"),
        card_body(
          strong("Organization / address"), p(class = "muted", "Shared by everyone listed below, unless a person needs a different one."),
          fluidRow(
            column(6, textInput("eml_org_name", "Organization name")),
            column(6, textInput("eml_org_address", "Street address"))
          ),
          fluidRow(
            column(4, textInput("eml_org_city", "City")),
            column(4, textInput("eml_org_admin_area", "State/region")),
            column(4, textInput("eml_org_country", "Country code (e.g. AU)"))
          ),
          textInput("eml_org_phone", "Phone number (optional)"),
          tags$hr(),
          strong("Creator(s)"), p(class = "muted", "The person/people who generated this dataset - add one row per creator."),
          div(id = "creator_rows_container", creator_row_ui(1)),
          actionButton("add_creator", tagList(bsicons::bs_icon("plus-lg"), " Add another creator"), class = "btn-outline-secondary btn-sm"),
          tags$hr(),
          checkboxInput("eml_mp_same", "Metadata provider is the same person as the creator", value = TRUE),
          conditionalPanel(condition = "input.eml_mp_same == false",
            strong("Metadata provider"),
            fluidRow(
              column(3, textInput("eml_mp_given", "Given name")),
              column(3, textInput("eml_mp_sur", "Surname")),
              column(3, textInput("eml_mp_position", "Position")),
              column(3, textInput("eml_mp_email", "Email"))
            )
          ),
          tags$hr(),
          checkboxInput("eml_contact_same", "Contact is the same person as the creator", value = TRUE),
          conditionalPanel(condition = "input.eml_contact_same == false",
            strong("Contact"),
            fluidRow(
              column(3, textInput("eml_contact_given", "Given name")),
              column(3, textInput("eml_contact_sur", "Surname")),
              column(3, textInput("eml_contact_position", "Position")),
              column(3, textInput("eml_contact_email", "Email"))
            )
          ),
          tags$hr(),
          strong("Associated parties (optional)"), p(class = "muted", "Other people credited on this dataset, each with their own role - author, editor, reviewer, processor, curator, programmer, content provider, publisher, and more. Shares the Organization/address block above. Remove all rows to omit entirely."),
          div(id = "ap_rows_container", ap_row_ui(1)),
          actionButton("add_ap", tagList(bsicons::bs_icon("plus-lg"), " Add another associated party"), class = "btn-outline-secondary btn-sm")
        )
      ),
      card(
        card_header(bsicons::bs_icon("list-check", class = "section-icon"), "Methods"),
        card_body(
          textAreaInput("eml_methods", "Method steps (one per line, in order)", rows = 5,
                        placeholder = "Environmental sample collection from designated marine sampling locations.\nDNA extraction from collected environmental samples.\nPCR amplification of targeted genetic markers.\n..."),
          textAreaInput("eml_study_extent", "Study extent description", rows = 2),
          textAreaInput("eml_sampling_desc", "Sampling description", rows = 2)
        )
      ),
      card(
        card_header(bsicons::bs_icon("diagram-3", class = "section-icon"), "Project (optional)"),
        card_body(
          p(class = "muted", "Leave the Project ID blank to omit this section entirely."),
          textInput("eml_project_id", "Project ID"),
          textInput("eml_project_title", "Project title"),
          textAreaInput("eml_project_abstract", "Project abstract", rows = 2),
          textInput("eml_funding", "Funding"),
          textAreaInput("eml_study_area_desc", "Study area description", rows = 2),
          textAreaInput("eml_design_desc", "Design description", rows = 2),
          tags$hr(),
          strong("Project personnel"), p(class = "muted", "People with a role on this project specifically (e.g. point of contact, curator) - shares the Organization/address block above. Only used if Project ID is filled in."),
          div(id = "personnel_rows_container", personnel_row_ui(1)),
          actionButton("add_personnel", tagList(bsicons::bs_icon("plus-lg"), " Add another person"), class = "btn-outline-secondary btn-sm")
        )
      ),
      div(class = "nav-row",
          actionButton("back_to_5_from_6", tagList(bsicons::bs_icon("arrow-left"), " Back"), class = "btn-outline-secondary"),
          actionButton("generate_eml", tagList(bsicons::bs_icon("file-earmark-code"), " Generate metadata & continue"), class = "btn-primary")
      )
    )
  })

  observeEvent(input$back_to_5_from_6, { rv$current_step <- 5 })

  output$download_eml_template <- downloadHandler(
    filename = function() "mat_metadata_template.xlsx",
    content = function(file) write_eml_metadata_excel(file)
  )

  observeEvent(input$eml_upload, {
    req(input$eml_upload)
    parsed <- tryCatch(
      read_eml_metadata_excel(input$eml_upload$datapath),
      error = function(e) {
        showNotification(paste("Could not read that file:", conditionMessage(e)), type = "error", duration = NULL)
        NULL
      }
    )
    if (is.null(parsed)) return()

    for (def in EML_FIELD_DEFS) {
      if (!def$key %in% names(parsed)) next
      val <- parsed[[def$key]]
      input_id <- paste0("eml_", def$key)
      switch(def$type,
        text     = updateTextInput(session, input_id, value = val),
        textarea = updateTextAreaInput(session, input_id, value = val),
        date     = updateDateInput(session, input_id, value = as.Date(val)),
        logical  = updateCheckboxInput(session, input_id, value = isTRUE(val)),
        select   = updateSelectInput(session, input_id, selected = val)
      )
    }

    creators <- parsed$creators
    if (!is.null(creators) && length(creators) > 0) {
      ids <- set_creator_rows(length(creators))
      for (i in seq_along(ids)) {
        c <- creators[[i]]
        updateTextInput(session, paste0("eml_creator_given_", ids[i]), value = c$given_name %||% "")
        updateTextInput(session, paste0("eml_creator_sur_", ids[i]), value = c$sur_name %||% "")
        updateTextInput(session, paste0("eml_creator_position_", ids[i]), value = c$position %||% "")
        updateTextInput(session, paste0("eml_creator_email_", ids[i]), value = c$email %||% "")
        updateTextInput(session, paste0("eml_creator_org_", ids[i]), value = c$organization %||% "")
        updateTextInput(session, paste0("eml_creator_address_", ids[i]), value = c$delivery_point %||% "")
        updateTextInput(session, paste0("eml_creator_city_", ids[i]), value = c$city %||% "")
        updateTextInput(session, paste0("eml_creator_admin_area_", ids[i]), value = c$admin_area %||% "")
        updateTextInput(session, paste0("eml_creator_country_", ids[i]), value = c$country %||% "")
        updateTextInput(session, paste0("eml_creator_phone_", ids[i]), value = c$phone %||% "")
      }
    }

    personnel <- parsed$personnel
    if (!is.null(personnel) && length(personnel) > 0) {
      ids <- set_personnel_rows(length(personnel))
      for (i in seq_along(ids)) {
        p <- personnel[[i]]
        updateTextInput(session, paste0("eml_personnel_given_", ids[i]), value = p$given_name %||% "")
        updateTextInput(session, paste0("eml_personnel_sur_", ids[i]), value = p$sur_name %||% "")
        updateTextInput(session, paste0("eml_personnel_position_", ids[i]), value = p$position %||% "")
        updateTextInput(session, paste0("eml_personnel_email_", ids[i]), value = p$email %||% "")
        updateTextInput(session, paste0("eml_personnel_role_", ids[i]), value = p$role %||% "POINT_OF_CONTACT")
      }
    }

    aps <- parsed$associated_parties
    if (!is.null(aps)) {
      ids <- set_ap_rows(length(aps))
      for (i in seq_along(ids)) {
        a <- aps[[i]]
        updateTextInput(session, paste0("eml_ap_given_", ids[i]), value = a$given_name %||% "")
        updateTextInput(session, paste0("eml_ap_sur_", ids[i]), value = a$sur_name %||% "")
        updateTextInput(session, paste0("eml_ap_position_", ids[i]), value = a$position %||% "")
        updateTextInput(session, paste0("eml_ap_email_", ids[i]), value = a$email %||% "")
        updateSelectInput(session, paste0("eml_ap_role_", ids[i]), selected = a$role %||% "PUBLISHER")
      }
    }

    showNotification(paste0("Filled ", length(parsed), " field(s) from the uploaded file - review before continuing."),
                      type = "message", duration = 6)
  })

  observeEvent(input$generate_eml, {
    org <- list(
      organization   = input$eml_org_name,
      delivery_point = input$eml_org_address,
      city           = input$eml_org_city,
      admin_area     = input$eml_org_admin_area,
      country        = input$eml_org_country,
      phone          = input$eml_org_phone
    )
    creators <- lapply(rv$creator_ids, function(id) {
      list(given_name     = input[[paste0("eml_creator_given_", id)]],
           sur_name       = input[[paste0("eml_creator_sur_", id)]],
           position       = input[[paste0("eml_creator_position_", id)]],
           email          = input[[paste0("eml_creator_email_", id)]],
           organization   = input[[paste0("eml_creator_org_", id)]],
           delivery_point = input[[paste0("eml_creator_address_", id)]],
           city           = input[[paste0("eml_creator_city_", id)]],
           admin_area     = input[[paste0("eml_creator_admin_area_", id)]],
           country        = input[[paste0("eml_creator_country_", id)]],
           phone          = input[[paste0("eml_creator_phone_", id)]])
    })
    # "Same as creator" (metadata provider / contact) means the FIRST
    # listed creator - EML only ever has one metadataProvider/contact,
    # unlike creator which can repeat.
    first_creator <- creators[[1]]

    metadata_provider <- if (isTRUE(input$eml_mp_same)) {
      first_creator
    } else {
      c(list(given_name = input$eml_mp_given, sur_name = input$eml_mp_sur,
             position = input$eml_mp_position, email = input$eml_mp_email), org)
    }

    contact <- if (isTRUE(input$eml_contact_same)) {
      first_creator
    } else {
      c(list(given_name = input$eml_contact_given, sur_name = input$eml_contact_sur,
             position = input$eml_contact_position, email = input$eml_contact_email), org)
    }

    associated_party <- lapply(rv$ap_ids, function(id) {
      c(list(given_name = input[[paste0("eml_ap_given_", id)]],
             sur_name    = input[[paste0("eml_ap_sur_", id)]],
             position    = input[[paste0("eml_ap_position_", id)]],
             email       = input[[paste0("eml_ap_email_", id)]],
             role        = input[[paste0("eml_ap_role_", id)]]), org)
    })

    project_personnel <- lapply(rv$personnel_ids, function(id) {
      c(list(given_name = input[[paste0("eml_personnel_given_", id)]],
             sur_name    = input[[paste0("eml_personnel_sur_", id)]],
             position    = input[[paste0("eml_personnel_position_", id)]],
             email       = input[[paste0("eml_personnel_email_", id)]],
             role        = input[[paste0("eml_personnel_role_", id)]]), org)
    })

    keywords <- trimws(strsplit(input$eml_keywords %||% "", ",")[[1]])
    abstract_paragraphs <- strsplit(input$eml_abstract %||% "", "\n")[[1]]
    method_steps <- c(strsplit(input$eml_methods %||% "", "\n")[[1]], uneven_samples_method_note(), name_choice_method_note(), exclusion_method_note(),
                      platform_method_note(rv$answers$vessel_name, rv$answers$voyage_id))
    # Raw-read link (Step 1): OBIS's dataset page shows the abstract but not
    # the methods, so the link goes in both (unless already typed there).
    reads_note <- raw_reads_sentence(rv$answers$associated_sequences_uri)
    if (!is.null(reads_note)) {
      uri <- rv$answers$associated_sequences_uri
      if (!any(grepl(uri, abstract_paragraphs, fixed = TRUE))) abstract_paragraphs <- c(abstract_paragraphs, reads_note)
      if (!any(grepl(uri, method_steps, fixed = TRUE)))        method_steps <- c(method_steps, reads_note)
    }

    eml <- tryCatch(
      build_eml_xml(
        title                     = input$eml_title,
        creator                   = creators,
        metadata_provider         = metadata_provider,
        contact                   = contact,
        associated_party          = associated_party,
        pub_date                  = input$eml_pub_date,
        abstract_paragraphs       = abstract_paragraphs,
        keywords                  = keywords,
        license_url               = input$eml_license,
        license_title             = names(EML_LICENSE_CHOICES)[EML_LICENSE_CHOICES == input$eml_license],
        distribution_url          = input$eml_distribution_url,
        method_steps              = method_steps,
        study_extent_description  = input$eml_study_extent,
        sampling_description      = input$eml_sampling_desc,
        project_id                = input$eml_project_id,
        project_title             = input$eml_project_title,
        project_abstract          = input$eml_project_abstract,
        funding                   = input$eml_funding,
        study_area_description    = input$eml_study_area_desc,
        design_description        = input$eml_design_desc,
        project_personnel         = project_personnel,
        coverage                  = tryCatch(
          eml_coverage_from_data(rv$build_result$event_core, final_occurrence(), rv$worms_result$matched_df),
          error = function(e) NULL)
      ),
      error = function(e) {
        showNotification(paste("Could not generate metadata:", conditionMessage(e)), type = "error", duration = NULL)
        NULL
      }
    )

    if (!is.null(eml)) {
      rv$eml_xml <- eml
      rv$current_step <- 7
    }
  })

  # =====================================================================
  # STEP 7 - Results & Download
  # =====================================================================
  output$step7_body <- renderUI({
    req(rv$build_result, rv$worms_result)
    tagList(
      card(
        card_header(bsicons::bs_icon("file-earmark-bar-graph", class = "section-icon"), "Analysis report"),
        card_body(
          p(class = "muted", "A one-page summary of this archive. It is saved with the archive (in the Report folder, under the same name) when you choose Save as draft or Publish below."),
          uiOutput("report_preview"),
          div(class = "d-flex gap-2 mt-3",
              actionButton("view_report_full", tagList(bsicons::bs_icon("arrows-fullscreen"), " View full size"), class = "btn-outline-primary btn-sm"),
              downloadButton("download_report_png", "Download report (.png)", class = "btn-outline-primary btn-sm"))
        )
      ),
      card(
        card_header(bsicons::bs_icon("download", class = "section-icon"), "Download"),
        card_body(
          p(class = "muted", paste0(
            "A zip with the Event core, one Occurrence extension per assay, one DNA Derived Data extension per assay",
            if (!is.null(rv$eml_xml)) ", and eml.xml" else " (no eml.xml - go back to Step 6 to generate one)",
            " - ready to upload to an IPT."
          )),
          # Open to everyone, account or not: the archive is built from the
          # user's own files in this session, so nothing shared is exposed.
          downloadButton("download_archive", "Download archive (.zip)", class = "btn-primary")
        )
      ),
      card(
        card_header(bsicons::bs_icon("cloud-upload", class = "section-icon"), "Save this archive"),
        card_body(
          if (!archive_history_enabled()) {
            p(class = "muted", "Draft and Publish storage isn't connected. Download the archive above and keep it in a folder on your computer, ",
              "or connect an AWS S3 bucket (see the README) to keep a shared record of drafts and published archives.")
          } else if (!isTRUE(rv$logged_in)) {
            tagList(
              p(class = "muted", "Log in to save this archive to Draft (or Publish, if you're a publisher)."),
              actionButton("save_archive_locked", tagList(bsicons::bs_icon("lock-fill"), " Save"), class = "btn-primary")
            )
          } else if (can_publish(rv$logged_in_email)) {
            tagList(
              p(class = "muted", "Choose where this archive goes before you finish. You can review drafts later and publish them from the Draft tab."),
              radioButtons("archive_destination", NULL, selected = "draft",
                choiceValues = c("draft", "publish"),
                choiceNames = list(
                  tagList(tags$b("Save as draft"), tags$br(), span(class = "muted", "Keep it as a draft, named with a timestamp. Nothing is sent to OBIS.")),
                  tagList(tags$b("Publish"), tags$br(), span(class = "muted", "Put it in the Publish folder as PROJECT_MAT_DwCA.zip, where it will be sent to OBIS."))
                )),
              actionButton("save_archive_btn", tagList(bsicons::bs_icon("cloud-upload"), " Save"), class = "btn-primary"),
              uiOutput("save_archive_status")
            )
          } else {
            # Normal user: no "Publish" choice at all (not even disabled) -
            # input$archive_destination simply won't exist, which the save
            # observer below already treats as "draft" (its default branch).
            tagList(
              p(class = "muted", "Keep it as a draft, named with a timestamp. Nothing is sent to OBIS. Ask a publisher or admin to move it to Publish once it's ready."),
              actionButton("save_archive_btn", tagList(bsicons::bs_icon("cloud-upload"), " Save as draft"), class = "btn-primary"),
              uiOutput("save_archive_status")
            )
          }
        )
      ),
      div(class = "nav-row", actionButton("back_to_6_from_7", tagList(bsicons::bs_icon("arrow-left"), " Back"), class = "btn-outline-secondary"), div())
    )
  })

  # ---- Analysis report (drawn once per set of results, shown here and saved with the archive) ----
  current_report_info <- function() {
    r <- rv$worms_result
    n_manual <- n_auto <- 0
    if (nrow(r$resolved_ambiguous_df) > 0) {
      n_manual <- length(unique(r$resolved_ambiguous_df$queriedName[r$resolved_ambiguous_df$resolution == "manual_override"]))
      n_auto   <- length(unique(r$resolved_ambiguous_df$queriedName[r$resolved_ambiguous_df$resolution == "single_accepted"]))
    }
    n_matched <- length(unique(r$matched_df$queriedName))
    taxonomy_df <- tibble(
      Resolution = c("Matched directly", "Corrected by you, then matched", "Auto-resolved (single accepted WoRMS record)",
                     "Manually resolved by you", "Updated to WoRMS spelling or accepted name", "Not yet in WoRMS (kept as identified)",
                     "Recorded as Biota incertae sedis", "Still ambiguous", "Close spellings left open"),
      Names = c(n_matched - n_auto - n_manual, length(rv$name_corrections), n_auto, n_manual,
                length(intersect(rv$use_accepted_names, r$synonym_df$queriedName)),
                length(name_groups()$keep),
                length(name_groups()$unknown),
                length(unique(r$ambiguous_df$queriedName)), length(name_groups()$misspelt_open))
    )
    # The name-choice rows only appear when used, so the one-page report keeps its size.
    taxonomy_df <- taxonomy_df[!(taxonomy_df$Resolution %in% c("Updated to WoRMS spelling or accepted name", "Not yet in WoRMS (kept as identified)",
                                                               "Recorded as Biota incertae sedis") & taxonomy_df$Names == 0), ]

    m <- tryCatch(check_dwc_mapping(rv$build_result$event_core, final_occurrence(), final_dna(), rv$build_result$emof), error = function(e) NULL)
    mapping <- if (is.null(m)) NULL else list(ok = sum(m$status %in% c("ok", "expected")), total = nrow(m),
                                              issues = sum(m$status %in% c("not_a_term", "wrong_type")))
    qc <- if (is.null(rv$qc_result)) NULL else list(
      passed = sum(vapply(rv$qc_result$results, function(x) !is.data.frame(x) || nrow(x) == 0, logical(1))),
      total = length(rv$qc_result$results), skipped = length(rv$qc_result$any_skipped))

    first_text <- function(...) { v <- c(...); v <- trimws(v[!is.na(v)]); v <- v[nzchar(v)]; if (length(v)) v[1] else NULL }
    first_para <- function(x) strsplit(x %||% "", "\n")[[1]][1]

    list(
      project_id = trimws(input$project_id %||% ""),
      title = first_text(input$eml_title, input$eml_project_title),
      description = first_text(first_para(input$eml_abstract), first_para(input$eml_project_abstract)),
      generated = Sys.time(),
      reference_db_label = names(REFERENCE_DB_CHOICES)[REFERENCE_DB_CHOICES == (input$reference_db %||% "curated")],
      event_core = rv$build_result$event_core, n_controls = nrow(rv$build_result$controls),
      occurrence_tables = final_occurrence(), taxonomy_df = taxonomy_df, qc = qc, mapping = mapping
    )
  }

  report_file <- reactive({
    req(rv$build_result, rv$worms_result)
    path <- tempfile(fileext = ".png")
    build_report_png(path, current_report_info())
    path
  })

  report_data_uri <- function(path) base64enc::dataURI(file = path, mime = "image/png")

  output$report_preview <- renderUI({
    img(src = report_data_uri(report_file()), alt = "Analysis report",
        style = "width: 100%; max-width: 820px; display: block; margin: 0 auto; border: 1px solid #eceae4; border-radius: 10px;")
  })

  output$download_report_png <- downloadHandler(
    filename = function() paste0(safe_project_id(input$project_id), "_report_", format(Sys.Date(), "%Y%m%d"), ".png"),
    content = function(file) file.copy(report_file(), file, overwrite = TRUE)
  )

  # One popup used for the report in Step 7 and for saved reports in the Draft/Publish tabs.
  viewed_report <- reactiveVal(NULL)
  show_report_modal <- function(path, title) {
    viewed_report(list(path = path, name = title))
    showModal(modalDialog(
      title = tagList(bsicons::bs_icon("file-earmark-bar-graph"), " ", title),
      div(style = "max-height: 75vh; overflow-y: auto;",
          img(src = report_data_uri(path), alt = title, style = "width: 100%; display: block;")),
      footer = tagList(downloadButton("download_viewed_report", "Download", class = "btn-outline-primary"), modalButton("Close")),
      size = "l", easyClose = TRUE
    ))
  }
  output$download_viewed_report <- downloadHandler(
    filename = function() { v <- viewed_report(); req(v); v$name },
    content = function(file) { v <- viewed_report(); req(v); file.copy(v$path, file, overwrite = TRUE) }
  )
  observeEvent(input$view_report_full, {
    show_report_modal(report_file(), paste0(safe_project_id(input$project_id), "_report.png"))
  })

  # Builds the archive zip at `file`. Shared by the Download button and the
  # Draft / Publish save, so all three always produce the identical archive.
  build_archive_zip <- function(file) {
      tmpdir <- tempfile("mat_")
      dir.create(tmpdir, recursive = TRUE)

      # All data files sit flat in one folder, matching standard Darwin
      # Core Archive layout (core + every extension side-by-side; it's
      # meta.xml, not folder structure, that records which file is the
      # core and how each extension links back to it - no meta.xml yet,
      # since these files are meant to be uploaded individually into an
      # institutional IPT, which builds the real archive itself).
      write.csv(rv$build_result$event_core, file.path(tmpdir, "Event.csv"), row.names = FALSE, na = "")
      occ_final <- final_occurrence()
      dna_final <- final_dna()
      for (a in names(occ_final)) {
        write.csv(occ_final[[a]], file.path(tmpdir, paste0("Occurrence_", a, ".csv")), row.names = FALSE, na = "")
      }
      for (a in names(dna_final)) {
        write.csv(dna_final[[a]], file.path(tmpdir, paste0("DNADerivedData_", a, ".csv")), row.names = FALSE, na = "")
      }
      # Optional: only when a vessel or voyage was given in Step 1 (see R/build_emof.R).
      if (!is.null(rv$build_result$emof)) {
        write.csv(rv$build_result$emof, file.path(tmpdir, "ExtendedMeasurementOrFact.csv"), row.names = FALSE, na = "")
      }
      if (!is.null(rv$eml_xml)) {
        writeLines(rv$eml_xml, file.path(tmpdir, "eml.xml"))
      }

      old_wd <- setwd(tmpdir)
      on.exit(setwd(old_wd))
      zip::zip(file, files = list.files(".", recursive = TRUE))
      setwd(old_wd)
  }

  observeEvent(input$download_archive_locked, { showModal(login_required_modal("download this archive")) })
  observeEvent(input$save_archive_locked, { showModal(login_required_modal("save this archive")) })

  output$download_archive <- downloadHandler(
    filename = function() paste0("mat_archive_", format(Sys.Date(), "%Y%m%d"), ".zip"),
    content = function(file) {
      req(rv$build_result, rv$worms_result)
      build_archive_zip(file)
    }
  )

  # ---- Step 7: save as Draft or Publish ----------------------------------
  save_status <- reactiveVal(NULL)
  output$save_archive_status <- renderUI(save_status())

  save_current_archive <- function(saver, label) {
    req(rv$logged_in)  # belt-and-braces: the UI already hides this behind login
    zip_path <- tempfile(fileext = ".zip")
    on.exit(unlink(zip_path), add = TRUE)
    res <- withProgress(message = paste0(label, "..."), value = 0.4, {
      tryCatch({
        build_archive_zip(zip_path)
        incProgress(0.2)
        # The report is saved with the zip under the same name; if it can't be drawn,
        # the archive is still saved (and the user is told).
        report_path <- tryCatch(report_file(), error = function(e) {
          showNotification(paste0("The analysis report couldn't be generated (", conditionMessage(e), ") - saving the archive without it."),
                           type = "warning", duration = 10)
          NULL
        })
        incProgress(0.2)
        saver(zip_path, trimws(input$project_id), report_path)
      }, error = function(e) e)
    })
    if (inherits(res, "error")) {
      showNotification(paste0(label, " failed: ", conditionMessage(res)), type = "error", duration = NULL)
      return(NULL)
    }
    history_refresh(isolate(history_refresh()) + 1)
    res
  }

  observeEvent(input$save_archive_btn, {
    req(rv$build_result, rv$worms_result)
    project <- trimws(input$project_id %||% "")
    if (!nzchar(project)) {
      showNotification("Enter a Project ID in Step 1 first - it names the saved file.", type = "error")
      return()
    }

    if (identical(input$archive_destination, "publish")) {
      # Belt-and-braces: the UI doesn't even offer this choice to a
      # non-publisher, but check again here in case of a forged input.
      if (!isTRUE(rv$logged_in) || !can_publish(rv$logged_in_email)) {
        showNotification("Publishing requires the publisher (or admin) role. Ask an admin in User Management.", type = "error", duration = 8)
        return()
      }
      already <- tryCatch(publish_exists(project), error = function(e) FALSE)
      showModal(modalDialog(
        title = "Publish this archive?",
        p("This will put ", tags$b(publish_file_name(project)),
          " in the Publish folder, where it will be sent to OBIS."),
        if (already) div(class = "alert alert-warning",
                          "A published version of this project already exists and will be replaced."),
        footer = tagList(modalButton("Cancel"), actionButton("confirm_publish", "Yes, publish", class = "btn-primary")),
        easyClose = TRUE
      ))
    } else {
      res <- save_current_archive(save_archive_as_draft, "Saving draft")
      if (!is.null(res)) {
        save_status(div(class = "alert alert-success mt-3",
                        bsicons::bs_icon("check-circle-fill"), " Saved as draft: ", tags$b(res$file_name),
                        ". Find it in the Draft tab - you can publish it from there."))
        popup_tab("Draft")
        showModal(success_modal(
          "Draft saved successfully",
          tagList("Your archive was saved as ", tags$b(res$file_name), ".", tags$br(),
                  report_line(res$report_key), tags$br(),
                  "It stays a draft until you move it to Publish from the Draft tab."),
          "Draft", "View in Draft tab"))
      }
    }
  })

  observeEvent(input$confirm_publish, {
    removeModal()
    req(rv$logged_in, can_publish(rv$logged_in_email))  # belt-and-braces: see the same check above where this modal is opened
    res <- save_current_archive(publish_archive, "Publishing")
    if (!is.null(res)) {
      save_status(div(class = "alert alert-success mt-3",
                      bsicons::bs_icon("check-circle-fill"), " Published: ", tags$b(res$file_name),
                      ". It's in the Publish tab, ready to be sent to OBIS."))
      popup_tab("Publish")
      showModal(success_modal(
        "Published successfully",
        tagList("Your archive was published as ", tags$b(res$file_name), ".", tags$br(),
                report_line(res$report_key), tags$br(),
                "It's in the Publish folder, ready to be sent to OBIS."),
        "Publish", "View in Publish tab"))
    }
  })

  report_line <- function(report_key) {
    if (is.null(report_key)) span(class = "text-warning", "No analysis report was saved with it.")
    else tagList("Report saved as ", tags$b(basename(report_key)), " in the Report folder.")
  }

  # Which tab the success popup's button jumps to.
  popup_tab <- reactiveVal("Draft")
  observeEvent(input$go_to_saved_tab, {
    removeModal()
    nav_select("main_tab", popup_tab())
  })

  # ---- Draft and Publish tabs (only functional when AWS credentials are set) ----
  list_or_notify <- function(lister, what) {
    history_refresh()
    req(archive_history_enabled())
    tryCatch(lister(), error = function(e) {
      showNotification(paste0("Couldn't load ", what, ": ", conditionMessage(e)), type = "error")
      NULL
    })
  }
  drafts_df    <- reactive(list_or_notify(list_drafts, "drafts"))
  published_df <- reactive(list_or_notify(list_published, "published files"))

  not_configured_card <- function(title) card(
    card_header(bsicons::bs_icon("cloud-slash", class = "section-icon"), title),
    card_body(
      p(class = "muted", "Not connected. This is optional: without it, download each archive from Step 7 and keep it in a folder on your computer."),
      p(class = "muted", "To keep a shared record of drafts and published archives, add an AWS S3 bucket and credentials to .Renviron (see the README).")
    )
  )

  render_archive_table <- function(df_reactive) DT::renderDT({
    df <- df_reactive()
    req(df)
    DT::datatable(
      df[, c("last_modified", "project", "path", "size_mb")],
      selection = "single", rownames = TRUE,
      # Positional, not named - with rownames = TRUE the row-number
      # column comes first with no real underlying column to name-map to.
      colnames = c("No.", "Last modified (UTC)", "Project", "File", "Size"),
      options = list(pageLength = 15, dom = "tip")
    )
  })
  output$draft_table   <- render_archive_table(drafts_df)

  download_selected_handler <- function(get_df, rows_id) downloadHandler(
    filename = function() {
      sel <- input[[rows_id]]
      req(sel)
      df <- get_df()
      req(df)
      basename(df$s3_key[sel])
    },
    content = function(file) {
      req(rv$logged_in)  # belt-and-braces: the UI already hides this download link behind login
      sel <- input[[rows_id]]
      req(sel)
      df <- get_df()
      req(df)
      fetch_archive_from_s3(df$s3_key[sel], file)
    }
  )
  output$download_draft_item   <- download_selected_handler(drafts_df, "draft_table_rows_selected")

  # Shared by both tabs: a real downloadButton when logged in, or a locked
  # placeholder (opens login_required_modal()) for a guest.
  download_or_locked_btn <- function(real_id, locked_id, label) {
    if (isTRUE(rv$logged_in)) downloadButton(real_id, label, class = "btn-outline-primary btn-sm")
    else actionButton(locked_id, tagList(bsicons::bs_icon("lock-fill"), " ", label), class = "btn-outline-primary btn-sm")
  }

  output$draft_tab_body <- renderUI({
    if (!archive_history_enabled()) return(not_configured_card("Draft"))
    card(
      card_header(bsicons::bs_icon("pencil-square", class = "section-icon"), "Draft projects"),
      card_body(
        p(class = "muted", "Archives saved as drafts, newest first. Select one to download it, or move it to Publish once you're happy with it."),
        DT::DTOutput("draft_table"),
        div(class = "d-flex gap-2 mt-3 flex-wrap",
            download_or_locked_btn("download_draft_item", "download_draft_item_locked", "Download selected"),
            actionButton("view_draft_report", tagList(bsicons::bs_icon("file-earmark-bar-graph"), " View report"), class = "btn-outline-primary btn-sm"),
            if (isTRUE(rv$logged_in) && can_publish(rv$logged_in_email)) {
              actionButton("move_to_publish_btn", tagList(bsicons::bs_icon("send"), " Move to Publish"), class = "btn-primary btn-sm")
            } else if (isTRUE(rv$logged_in)) {
              tagList(actionButton("move_to_publish_btn_disabled", tagList(bsicons::bs_icon("send"), " Move to Publish"),
                                    class = "btn-outline-secondary btn-sm", disabled = TRUE),
                      span(class = "muted", style = "align-self: center;", "Publisher role required"))
            } else {
              actionButton("move_to_publish_locked", tagList(bsicons::bs_icon("lock-fill"), " Move to Publish"), class = "btn-primary btn-sm")
            },
            actionButton("refresh_drafts", tagList(bsicons::bs_icon("arrow-repeat"), " Refresh"), class = "btn-outline-secondary btn-sm"))
      )
    )
  })

  output$publish_tab_body <- renderUI({
    if (!archive_history_enabled()) return(not_configured_card("Publish"))
    card(
      card_header(class = "d-flex align-items-center",
        bsicons::bs_icon("send-check", class = "section-icon"), "Ready to publish",
        actionButton("refresh_publish", tagList(bsicons::bs_icon("arrow-repeat"), " Refresh"),
                     class = "btn-outline-secondary btn-sm ms-auto")),
      card_body(
        p(class = "muted", "Archives in the Publish folder, ready for OBIS. Every action is on the archive's own row: download it, view its report, ",
          "copy the S3 link the OBIS node harvests from, or email the node that it's ready."),
        uiOutput("publish_list")
      )
    )
  })

  # ---- Publish tab: one row per archive, its buttons on the same row ----
  # Row buttons send input$publish_action = {action, key}; the S3 key (not a
  # row number) says which archive, so a refresh can't point at the wrong one.
  pub_action_js <- function(action, key) sprintf(
    "Shiny.setInputValue('publish_action', {action: '%s', key: %s, t: Date.now()}, {priority: 'event'})",
    action, jsonlite::toJSON(key, auto_unbox = TRUE))
  fmt_s3_time <- function(x) {
    t <- as.POSIXct(sub("\\.[0-9]+Z?$", "", sub("T", " ", as.character(x))), tz = "UTC")
    if (is.na(t)) return(as.character(x))
    paste0(sub("^0", "", format(t, "%d %b %Y, %H:%M", tz = "UTC")), " UTC")
  }
  published_row <- function(key) {
    df <- published_df()
    if (is.null(df) || !(key %in% df$s3_key)) return(NULL)
    df[df$s3_key == key, , drop = FALSE][1, ]
  }

  # One download link per row. They're registered whenever the listing changes.
  observe({
    df <- published_df()
    req(df, nrow(df) > 0)
    for (i in seq_len(nrow(df))) local({
      key <- df$s3_key[i]
      output[[paste0("pub_dl_", i)]] <- downloadHandler(
        filename = function() basename(key),
        content = function(file) {
          req(rv$logged_in)  # the UI only shows this link to logged-in users
          fetch_archive_from_s3(key, file)
        })
    })
  })

  output$publish_list <- renderUI({
    df <- published_df()
    req(!is.null(df))
    if (nrow(df) == 0) {
      return(div(class = "pub-empty", bsicons::bs_icon("inbox"),
                 strong("Nothing published yet."), br(), "Move a draft here from the Draft tab, or publish from Step 7."))
    }
    logged_in <- isTRUE(rv$logged_in)
    publisher <- logged_in && can_publish(rv$logged_in_email)
    lock <- function(label, icon, cls = "btn-outline-primary") {
      tags$button(type = "button", class = paste("btn btn-sm", cls), onclick = pub_action_js("locked", ""),
                  title = "Log in to use this", bsicons::bs_icon("lock-fill"), " ", label)
    }
    div(class = "pub-list", lapply(seq_len(nrow(df)), function(i) {
      row <- df[i, ]
      url <- s3_public_url(row$s3_key)
      copy_js <- paste0(
        "var b=this,u=b.getAttribute('data-url'),ok=function(){var h=b.innerHTML;b.innerHTML='Copied!';",
        "b.classList.add('btn-success');setTimeout(function(){b.innerHTML=h;b.classList.remove('btn-success');},1800);};",
        "if(navigator.clipboard&&window.isSecureContext){navigator.clipboard.writeText(u).then(ok);}",
        "else{var t=document.createElement('textarea');t.value=u;document.body.appendChild(t);t.select();",
        "document.execCommand('copy');document.body.removeChild(t);ok();}")
      div(class = "pub-item",
        div(class = "pub-icon", bsicons::bs_icon("file-earmark-zip")),
        div(class = "pub-main",
          div(class = "pub-title", row$project, span(class = "pub-badge", "Ready for OBIS")),
          div(class = "pub-meta",
              span(bsicons::bs_icon("file-earmark"), row$path),
              span(bsicons::bs_icon("hdd"), row$size_mb),
              span(bsicons::bs_icon("clock"), fmt_s3_time(row$last_modified)))
        ),
        div(class = "pub-actions",
          if (logged_in) downloadButton(paste0("pub_dl_", i), "Download", class = "btn-outline-primary btn-sm")
          else lock("Download", "download"),
          tags$button(type = "button", class = "btn btn-sm btn-outline-primary", onclick = pub_action_js("report", row$s3_key),
                      bsicons::bs_icon("file-earmark-bar-graph"), " Report"),
          if (logged_in) tags$button(type = "button", class = "btn btn-sm btn-outline-primary", `data-url` = url, onclick = copy_js,
                                     title = "Copy the S3 link the OBIS node harvests from", bsicons::bs_icon("link-45deg"), " Copy link")
          else lock("Copy link", "clipboard"),
          if (publisher) tags$button(type = "button", class = "btn btn-sm btn-primary", onclick = pub_action_js("notify", row$s3_key),
                                     bsicons::bs_icon("envelope-paper"), " Notify OBIS node")
          else if (logged_in) tags$button(type = "button", class = "btn btn-sm btn-outline-secondary", disabled = NA,
                                          title = "Publisher role required", bsicons::bs_icon("envelope-paper"), " Notify OBIS node")
          else lock("Notify OBIS node", "envelope-paper", "btn-primary")
        )
      )
    }))
  })

  observeEvent(input$publish_action, {
    a <- input$publish_action
    if (identical(a$action, "locked")) return(showModal(login_required_modal("use a published archive")))
    row <- published_row(a$key)
    if (is.null(row)) {
      showNotification("That archive is no longer in the Publish folder - press Refresh.", type = "warning")
      return()
    }
    if (identical(a$action, "report")) view_report_for_key(row$s3_key)
    if (identical(a$action, "notify")) open_notify(row)
  })

  notify_target <- reactiveVal(NULL)  # the row (archive) the open popup is about
  notify_report_url <- reactiveVal(NULL)
  open_notify <- function(row) {
    req(rv$logged_in, can_publish(rv$logged_in_email))
    if (!smtp_configured()) {
      showNotification("Email isn't set up on this server (SMTP_USER / SMTP_PASSWORD in .Renviron).", type = "error", duration = 8)
      return()
    }
    notify_target(row)
    rep_key <- report_key_for_zip(row$s3_key)
    notify_report_url(if (isTRUE(tryCatch(.s3_exists(rep_key), error = function(e) FALSE))) s3_public_url(rep_key) else NULL)
    showModal(modalDialog(
      title = tagList(bsicons::bs_icon("envelope-paper"), " Notify the OBIS node"),
      size = "l", easyClose = FALSE,
      p(class = "muted", "Emails the OBIS node that ", tags$b(row$path), " is ready, with its S3 link so they can harvest it. ",
        "Replies come back to you (", rv$logged_in_email, ")."),
      layout_columns(col_widths = c(6, 6),
        textInput("notify_to", "To (OBIS node email)", value = obis_node_email(), placeholder = "node@example.org", width = "100%"),
        textInput("notify_sender_name", "Your name", width = "100%")),
      textInput("notify_title", optional_label("Dataset title", "Shown in the subject line and the email."), width = "100%"),
      textAreaInput("notify_note", optional_label("Message", "A personal note added to the email, e.g. what changed since the last version."),
                    rows = 3, width = "100%"),
      tags$div(class = "small fw-semibold mb-1", "Preview"),
      uiOutput("notify_preview"),
      footer = tagList(modalButton("Cancel"),
                       actionButton("notify_send", tagList(bsicons::bs_icon("send"), " Send email"), class = "btn-primary"))
    ))
  }
  notify_email <- reactive({
    row <- notify_target()
    req(row)
    obis_notify_email(
      project = row$project, archive_url = s3_public_url(row$s3_key), file_name = row$path, size = row$size_mb,
      last_modified = sub(" UTC$", "", fmt_s3_time(row$last_modified)),
      report_url = notify_report_url(), title = input$notify_title, sender_name = input$notify_sender_name,
      sender_email = rv$logged_in_email, note = input$notify_note)
  })
  output$notify_preview <- renderUI({
    em <- notify_email()
    tagList(div(class = "small mb-1", strong("Subject: "), em$subject),
            tags$iframe(srcdoc = em$html, style = "width:100%;height:420px;border:1px solid #dde3ee;border-radius:4px;background:#fff;"))
  })
  # "Send email" first asks who to copy in (a second popup), then sends.
  # The email is frozen here, because the first popup's inputs go away.
  notify_pending <- reactiveVal(NULL)
  observeEvent(input$notify_send, {
    req(rv$logged_in, can_publish(rv$logged_in_email), notify_target())
    to <- split_emails(input$notify_to)
    bad <- to[!vapply(to, email_looks_valid, logical(1))]
    if (length(to) == 0 || length(bad) > 0) {
      showNotification(if (length(to) == 0) "Enter the OBIS node's email address." else paste0("Not a valid email: ", paste(bad, collapse = ", ")),
                       type = "error")
      return()
    }
    notify_pending(list(to = to, email = notify_email()))
    showModal(modalDialog(
      title = tagList(bsicons::bs_icon("people"), " Copy anyone in on this request?"),
      easyClose = FALSE,
      p("The email goes to ", tags$b(paste(to, collapse = ", ")), ". Do you want to CC it to yourself or anyone else?"),
      checkboxInput("notify_cc_me", paste0("CC me (", rv$logged_in_email, ")"), value = TRUE),
      textInput("notify_cc_others", optional_label("Also CC", "Separate several addresses with commas."),
                placeholder = "colleague@uwa.edu.au, manager@uwa.edu.au", width = "100%"),
      footer = tagList(modalButton("Cancel"),
                       actionButton("notify_confirm_send", tagList(bsicons::bs_icon("send"), " Send"), class = "btn-primary"))
    ))
  })
  observeEvent(input$notify_confirm_send, {
    req(rv$logged_in, can_publish(rv$logged_in_email))
    pend <- notify_pending()
    req(pend)
    others <- split_emails(input$notify_cc_others)
    bad <- others[!vapply(others, email_looks_valid, logical(1))]
    if (length(bad) > 0) {
      showNotification(paste0("Not a valid email: ", paste(bad, collapse = ", ")), type = "error")
      return()
    }
    cc <- unique(tolower(c(if (isTRUE(input$notify_cc_me)) rv$logged_in_email, others)))
    cc <- setdiff(cc, tolower(pend$to))
    em <- pend$email
    sent <- withProgress(message = "Sending email...", value = 0.5, tryCatch(
      send_obis_notify_email(pend$to, em$subject, em$html, reply_to = rv$logged_in_email, cc = cc),
      error = function(e) e))
    if (inherits(sent, "error")) {
      showNotification(paste0("Couldn't send the email: ", conditionMessage(sent)), type = "error", duration = 10)
      return()
    }
    notify_pending(NULL)
    removeModal()
    showNotification(paste0("Email sent to ", paste(pend$to, collapse = ", "),
                            if (length(cc) > 0) paste0(" (CC: ", paste(cc, collapse = ", "), ")"), "."),
                     type = "message", duration = 8)
  })

  observeEvent(input$download_draft_item_locked,   { showModal(login_required_modal("download a saved archive")) })
  observeEvent(input$move_to_publish_locked,       { showModal(login_required_modal("move a draft to Publish")) })

  # "View report": fetch the report that shares the selected zip's name and show it in a popup.
  view_saved_report <- function(get_df, rows_id) {
    sel <- input[[rows_id]]
    df <- get_df()
    if (length(sel) != 1 || is.null(df)) {
      showNotification("Select a row in the table first.", type = "warning")
      return()
    }
    view_report_for_key(df$s3_key[sel])
  }
  view_report_for_key <- function(key) {
    dest <- tempfile(fileext = ".png")
    found <- withProgress(message = "Loading report...", value = 0.5,
                          tryCatch(fetch_report_for_zip(key, dest), error = function(e) e))
    if (inherits(found, "error")) {
      showNotification(paste0("Couldn't load the report: ", conditionMessage(found)), type = "error")
    } else if (!isTRUE(found)) {
      showNotification("No report was saved for this file (archives saved before reports existed don't have one).", type = "warning", duration = 8)
    } else {
      show_report_modal(dest, sub("\\.zip$", ".png", basename(key)))
    }
  }
  observeEvent(input$view_draft_report,   { view_saved_report(drafts_df, "draft_table_rows_selected") })

  observeEvent(input$refresh_drafts,  { history_refresh(isolate(history_refresh()) + 1) })
  observeEvent(input$refresh_publish, { history_refresh(isolate(history_refresh()) + 1) })

  # Moving a draft to Publish always asks first, and remembers WHICH draft
  # was chosen when the dialog opened (the table can refresh underneath it).
  pending_move_key <- reactiveVal(NULL)

  observeEvent(input$move_to_publish_btn, {
    req(rv$logged_in, can_publish(rv$logged_in_email))  # belt-and-braces: the UI already hides this button otherwise
    sel <- input$draft_table_rows_selected
    df <- drafts_df()
    if (length(sel) != 1 || is.null(df)) {
      showNotification("Select a draft in the table first.", type = "warning")
      return()
    }
    pending_move_key(df$s3_key[sel])
    already <- tryCatch(publish_exists(df$project[sel]), error = function(e) FALSE)
    showModal(modalDialog(
      title = "Move to Publish?",
      p("You're about to move ", tags$b(df$path[sel]), " out of Draft and into Publish as ",
        tags$b(publish_file_name(df$project[sel])), "."),
      p("Once it's moved it no longer appears in Draft, and it will be sent to OBIS."),
      if (already) div(class = "alert alert-warning",
                        "A published version of ", tags$b(df$project[sel]), " already exists and will be replaced."),
      footer = tagList(modalButton("Cancel"), actionButton("confirm_move_publish", "Yes, move to Publish", class = "btn-primary")),
      easyClose = TRUE
    ))
  })

  observeEvent(input$confirm_move_publish, {
    req(rv$logged_in, can_publish(rv$logged_in_email))  # belt-and-braces: the UI already hides this behind login/role
    key <- pending_move_key()
    req(key)
    removeModal()
    pending_move_key(NULL)
    res <- withProgress(message = "Moving to Publish...", value = 0.5,
                        tryCatch(move_draft_to_publish(key), error = function(e) e))
    if (inherits(res, "error")) {
      showNotification(paste0("Move to Publish failed: ", conditionMessage(res)), type = "error", duration = NULL)
      return()
    }
    history_refresh(isolate(history_refresh()) + 1)
    if (isTRUE(res$draft_removed)) {
      popup_tab("Publish")
      showModal(success_modal(
        "Moved to Publish successfully",
        tagList("The draft is now in the Publish folder as ", tags$b(res$file_name), ".", tags$br(),
                switch(res$report_status,
                  moved  = tagList("Its report was renamed to ", tags$b(sub("\\.zip$", ".png", res$file_name)), ".", tags$br()),
                  copied = tagList("Its report was copied to ", tags$b(sub("\\.zip$", ".png", res$file_name)), " (the old draft report couldn't be removed).", tags$br()),
                  failed = tagList(span(class = "text-warning", "Its report could not be moved - it is still under the draft name in the Report folder."), tags$br()),
                  NULL),
                "It no longer appears in Draft and is ready to be sent to OBIS."),
        "Publish", "View in Publish tab"))
    } else {
      showNotification(paste0("Published as ", res$file_name, ", but the draft could not be removed - delete it manually if you don't need it."),
                       type = "warning", duration = NULL)
    }
  })

  observeEvent(input$back_to_6_from_7, { rv$current_step <- 6 })

  # =====================================================================
  # Restart - clears all progress and returns to a genuinely blank Step 1
  # (fresh DOM nodes for the file inputs, not just cleared values, since
  # browsers won't let JS reset a file input's displayed selection).
  # =====================================================================
  observeEvent(input$restart_app, {
    showModal(modalDialog(
      title = "Restart process?",
      "This clears all uploaded files, answers, and progress. This cannot be undone.",
      footer = tagList(modalButton("Cancel"), actionButton("confirm_restart", "Restart", class = "btn-danger")),
      easyClose = TRUE
    ))
  })

  observeEvent(input$confirm_restart, {
    removeModal()

    for (id in rv$assay_ids) removeUI(selector = paste0("#assay_row_", id))

    updateTextInput(session, "project_id", value = "")
    updateRadioButtons(session, "reference_db", selected = "curated")

    rv$validation   <- NULL
    rv$allow_uneven <- FALSE
    rv$validated_answers <- list(georeference_sources = NA_character_, associated_sequences_uri = NA_character_)
    rv$answers      <- list(georeference_sources = NA_character_, associated_sequences_uri = NA_character_)
    updateTextInput(session, "georeference_sources", value = "")
    updateTextInput(session, "associated_sequences_uri", value = "")
    updateTextInput(session, "vessel_name", value = "")
    updateTextInput(session, "vessel_code_uri", value = "")
    updateTextInput(session, "voyage_id", value = "")
    rv$build_result <- NULL
    rv$build_error  <- NULL
    rv$worms_result <- NULL
    rv$taxonomy_rechecks <- 0
    rv$name_corrections <- character()
    rv$manual_aphia_overrides <- c()
    rv$excluded_taxa <- character()
    rv$use_accepted_names <- character()
    rv$auto_id_off <- character()
    rv$unknown_names <- character()
    rv$keep_names <- character()
    rv$qc_result    <- NULL
    rv$eml_xml      <- NULL

    new_id <- rv$next_assay_id
    insertUI(selector = "#assay_rows_container", where = "beforeEnd", ui = assay_row_ui(new_id))
    setup_remove_handler(new_id)
    setup_assay_name_autodetect(new_id)
    rv$assay_ids <- c(new_id)
    rv$next_assay_id <- new_id + 1

    rv$current_step <- 1
    showNotification("Restarted.", type = "message", duration = 2)
  })
}

shinyApp(ui, server)
