### R/about_page.R
#
# The "Home" tab (a short welcome with a button into the Generate tab) and
# the "About" tab (what MAT is, why it exists, how the archive is built, how
# to run and adapt it). Static content only; the two navigation buttons are
# wired up in app.R's server. Written for people meeting the tool for the
# first time (data managers, OBIS/GBIF node staff), so it explains rather
# than assumes. about_css is added once in app.R's <head> and serves both.

MAT_VERSION  <- "1.0.0"   # keep in step with CITATION.cff
MAT_REPO_URL <- "https://github.com/Minderoo-OceanOmics-Centre-UWA/faire2obis"

about_css <- "
  .about-wrap { max-width: 1040px; margin: 0 auto; padding: 28px 24px 70px 24px; }
  .about-hero {
    display: flex; gap: 34px; align-items: center; padding: 34px 38px; border-radius: 18px; color: #fff;
    background: radial-gradient(circle at 18% 30%, #15808f 0%, #0c4766 45%, #061c33 100%);
    box-shadow: 0 16px 40px rgba(6,28,51,0.18);
  }
  .about-hero img { width: 150px; flex-shrink: 0; filter: drop-shadow(0 8px 18px rgba(0,0,0,0.35)); }
  .about-hero h2 { font-size: 1.9rem; font-weight: 800; letter-spacing: -0.02em; margin: 0 0 8px 0; }
  .about-hero .lead { color: #cdeff0; font-size: 1.05rem; margin-bottom: 16px; }
  .about-pill {
    display: inline-block; font-size: 0.74rem; font-weight: 600; letter-spacing: 0.02em; padding: 4px 11px; margin: 0 6px 6px 0;
    border-radius: 999px; background: rgba(255,255,255,0.12); border: 1px solid rgba(255,255,255,0.22); color: #e6fbf8;
  }
  .about-hero .btn { margin: 10px 8px 0 0; }
  .about-section { margin-top: 38px; }
  .about-kicker { font-size: 0.72rem; font-weight: 700; letter-spacing: 0.12em; text-transform: uppercase; color: #14a3a3; margin-bottom: 4px; }
  .about-section h3 { font-size: 1.35rem; font-weight: 800; letter-spacing: -0.01em; margin-bottom: 12px; }
  .about-section p { color: #3d3c38; line-height: 1.65; }
  .about-table { width: 100%; border-collapse: separate; border-spacing: 0; background: #fff; border: 1px solid #eceae4; border-radius: 12px; overflow: hidden; font-size: 0.9rem; }
  .about-table th, .about-table td { padding: 11px 16px; border-bottom: 1px solid #f0efea; vertical-align: top; }
  .about-table tr:last-child td { border-bottom: none; }
  .about-table thead th { background: #f4f7f9; font-weight: 700; color: #0b0b0b; }
  .about-table td:first-child { color: #3d3c38; font-weight: 600; width: 55%; }
  .about-table .mat-col { background: #f1fbfa; font-weight: 600; color: #0a3d62; }

  .arch { background: #fff; border: 1px solid #eceae4; border-radius: 14px; padding: 24px; }
  .arch-core {
    max-width: 420px; margin: 0 auto; text-align: center; color: #fff; border-radius: 12px; padding: 14px 18px;
    background: linear-gradient(160deg, #0a3d62 0%, #14a3a3 100%); box-shadow: 0 6px 16px rgba(10,61,98,0.2);
  }
  .arch-core small, .arch-box small { display: block; opacity: 0.85; font-size: 0.78rem; font-weight: 400; }
  .arch-link { text-align: center; font-size: 0.74rem; color: #898781; font-family: monospace; margin: 8px 0; }
  .arch-cols { display: grid; grid-template-columns: repeat(3, 1fr); gap: 16px; }
  .arch-col { display: flex; flex-direction: column; align-items: stretch; }
  .arch-box { border-radius: 10px; padding: 11px 12px; text-align: center; font-weight: 700; font-size: 0.88rem; }
  .arch-occ { background: #eef7f9; border: 1px solid #cde4ea; color: #0b5672; }
  .arch-dna { background: #fff7e8; border: 1px solid #f3dfb5; color: #8a5a00; }
  .arch-col.more .arch-box { background: #fafaf8; border: 1px dashed #c9c7bf; color: #898781; }
  .arch-assay { text-align: center; font-size: 0.78rem; font-weight: 700; color: #63625c; margin-bottom: 6px; letter-spacing: 0.04em; text-transform: uppercase; }

  .steps-grid { display: grid; grid-template-columns: repeat(auto-fill, minmax(230px, 1fr)); gap: 14px; }
  .step-card { background: #fff; border: 1px solid #eceae4; border-radius: 12px; padding: 16px 16px 14px 16px; }
  .step-card .num {
    width: 30px; height: 30px; border-radius: 50%; display: inline-flex; align-items: center; justify-content: center;
    background: #0e6a8a; color: #fff; font-weight: 700; font-size: 0.82rem; margin-right: 8px;
  }
  .step-card .title { font-weight: 700; }
  .step-card p { font-size: 0.86rem; margin: 8px 0 0 0; color: #55544e; line-height: 1.5; }

  .feature-grid { display: grid; grid-template-columns: repeat(auto-fill, minmax(300px, 1fr)); gap: 14px; }
  .feature { display: flex; gap: 12px; background: #fff; border: 1px solid #eceae4; border-radius: 12px; padding: 14px 16px; }
  .feature-grid.two-col { grid-template-columns: repeat(auto-fill, minmax(min(420px, 100%), 1fr)); }
  .feature .ficon {
    width: 36px; height: 36px; border-radius: 10px; flex-shrink: 0; display: flex; align-items: center; justify-content: center;
    background: #e8f6f5; color: #0f7a8c; font-size: 1.05rem;
  }
  .feature b { display: block; margin-bottom: 2px; }
  .feature span { font-size: 0.86rem; color: #55544e; line-height: 1.5; }
  .checklist { background: #fff; border: 1px solid #eceae4; border-radius: 12px; padding: 16px 20px; }
  .checklist li { margin: 4px 0; color: #3d3c38; }
  .standards a { font-weight: 600; text-decoration: none; }
  .about-code { margin-top: 14px; background: #061c33; color: #bff3ec; border-radius: 10px; padding: 14px 18px; font-size: 0.84rem; white-space: pre-wrap; }
  .cite-box { background: #fff; border-left: 4px solid #14a3a3; border-radius: 8px; padding: 14px 18px; font-size: 0.9rem; color: #3d3c38; }

  /* Home tab */
  .home-wrap { max-width: 1040px; margin: 0 auto; padding: 30px 24px 60px 24px; }
  .home-hero {
    position: relative; overflow: hidden; text-align: center; color: #fff; border-radius: 22px; padding: 54px 32px 50px 32px;
    background: radial-gradient(circle at 50% 22%, #15808f 0%, #0c4766 46%, #061c33 100%);
    box-shadow: 0 20px 50px rgba(6,28,51,0.22);
  }
  .home-hero img { width: 170px; filter: drop-shadow(0 10px 22px rgba(0,0,0,0.4)); margin-bottom: 18px; }
  .home-hero .eyebrow { font-size: 0.78rem; font-weight: 700; letter-spacing: 0.16em; text-transform: uppercase; color: #8fe3dc; }
  .home-hero h2 { font-size: 2.3rem; font-weight: 800; letter-spacing: -0.02em; margin: 6px 0 12px 0; }
  .home-hero .lead { color: #cdeff0; font-size: 1.08rem; max-width: 640px; margin: 0 auto 26px auto; line-height: 1.6; }
  .home-hero .btn-lg { padding: 12px 26px; font-weight: 700; border-radius: 12px; margin: 4px 6px; }
  .home-hero .btn-light, .about-hero .btn-light { background: #ffffff; border-color: #ffffff; color: #071d33; }
  .home-hero .btn-light:hover, .about-hero .btn-light:hover { background: #e5f1f4; border-color: #e5f1f4; color: #071d33; }
  .home-hero .home-note { margin-top: 18px; font-size: 0.82rem; color: #9fd8d6; }
  .home-cards { display: grid; grid-template-columns: repeat(3, 1fr); gap: 16px; margin-top: 26px; }
  .home-card { background: #fff; border: 1px solid #eceae4; border-radius: 14px; padding: 20px; }
  .home-card .ficon {
    width: 40px; height: 40px; border-radius: 11px; display: flex; align-items: center; justify-content: center;
    background: #e8f6f5; color: #0f7a8c; font-size: 1.15rem; margin-bottom: 12px;
  }
  .home-card b { display: block; font-size: 1rem; margin-bottom: 4px; }
  .home-card span { font-size: 0.88rem; color: #55544e; line-height: 1.55; }
  .home-flow { display: flex; align-items: center; justify-content: center; gap: 14px; flex-wrap: wrap; margin-top: 30px; }
  .home-flow .stage { background: #fff; border: 1px solid #eceae4; border-radius: 999px; padding: 9px 18px; font-weight: 600; font-size: 0.9rem; color: #0a3d62; }
  .home-flow .stage .n { display: inline-block; width: 22px; height: 22px; line-height: 22px; border-radius: 50%; background: #14a3a3; color: #fff; font-size: 0.75rem; text-align: center; margin-right: 8px; }
  .home-flow .arrow { color: #a9a89f; }

  @media (max-width: 720px) {
    .about-hero { flex-direction: column; text-align: center; padding: 26px 20px; }
    .arch-cols { grid-template-columns: 1fr; }
    .home-cards { grid-template-columns: 1fr; }
    .home-hero h2 { font-size: 1.7rem; }
  }
"

home_page_ui <- function() {
  home_card <- function(icon, title, text) {
    div(class = "home-card", div(class = "ficon", bsicons::bs_icon(icon)), tags$b(title), span(text))
  }
  stage <- function(n, text) span(class = "stage", span(class = "n", n), text)
  arrow <- span(class = "arrow", bsicons::bs_icon("arrow-right"))

  div(class = "home-wrap",
    div(class = "home-hero",
      tags$img(src = "mat_hex.svg", alt = "Multi-Assay Toolkit (MAT) logo"),
      div(class = "eyebrow", "Welcome to"),
      h2("Multi-Assay Toolkit (MAT)"),
      div(class = "lead",
          "Turn FAIRe eDNA metabarcoding files into one Darwin Core Archive, with every assay in a single dataset, ready for OBIS and GBIF."),
      actionButton("home_go_generate", tagList("Generate an archive ", bsicons::bs_icon("arrow-right")), class = "btn-light btn-lg"),
      actionButton("home_go_about", tagList(bsicons::bs_icon("info-circle"), " How it works"), class = "btn-outline-light btn-lg"),
      div(class = "home-note", "No account needed to build and download an archive. Open source under the MIT licence.")
    ),

    div(class = "home-flow",
      stage(1, "Upload FAIRe files"), arrow,
      stage(2, "Review and check"), arrow,
      stage(3, "Download the Darwin Core Archive")
    ),

    div(class = "home-cards",
      home_card("diagram-3", "Many assays, one dataset",
                "Each sample is written once in an Event core, and every assay links to it, so the whole project is published together."),
      home_card("file-earmark-spreadsheet", "Straight from FAIRe",
                "If your data follows the FAIRe standard, there is nothing to reformat: upload one workbook per assay."),
      home_card("shield-check", "Checked before you publish",
                "Names are matched to WoRMS with a review step, and OBIS quality checks run before you upload to an IPT.")
    )
  )
}

about_page_ui <- function() {
  ext_link <- function(href, text) tags$a(href = href, target = "_blank", rel = "noopener noreferrer", text, .noWS = "outside")
  code <- function(x) tags$code(x, .noWS = "outside")
  section <- function(kicker, title, ...) {
    div(class = "about-section", div(class = "about-kicker", kicker), h3(title), ...)
  }
  step_card <- function(n, title, text) {
    div(class = "step-card", span(class = "num", n), span(class = "title", title), p(text))
  }
  feature <- function(icon, title, text) {
    div(class = "feature", div(class = "ficon", bsicons::bs_icon(icon)), div(tags$b(title), span(text)))
  }
  arch_col <- function(label, more = FALSE) {
    div(class = paste("arch-col", if (more) "more"),
        div(class = "arch-assay", label),
        div(class = "arch-box arch-occ", "Occurrence", tags$small("detections (ASVs)")),
        div(class = "arch-link", "occurrenceID"),
        div(class = "arch-box arch-dna", "DNA Derived Data", tags$small("sequence, primers, PCR")))
  }

  div(class = "about-wrap",

    # ---- Hero -------------------------------------------------------------
    div(class = "about-hero",
      tags$img(src = "mat_hex.svg", alt = "Multi-Assay Toolkit (MAT) logo"),
      div(
        h2("Multi-Assay Toolkit (MAT)"),
        div(class = "lead",
            "Publish eDNA metabarcoding data from any number of assays as one Darwin Core Archive, ready for OBIS and GBIF."),
        span(class = "about-pill", "Open source, MIT licence"),
        span(class = "about-pill", "Event core"),
        span(class = "about-pill", "Any number of assays"),
        span(class = "about-pill", "FAIRe input"),
        span(class = "about-pill", paste0("Version ", MAT_VERSION)),
        div(
          actionButton("about_go_generate", tagList(bsicons::bs_icon("play-fill"), " Generate an archive"), class = "btn-light btn-sm"),
          tags$a(class = "btn btn-outline-light btn-sm", href = MAT_REPO_URL, target = "_blank", rel = "noopener noreferrer",
                 bsicons::bs_icon("github"), " Source code")
        )
      )
    ),

    # ---- Why --------------------------------------------------------------
    section("Why MAT", "One set of samples, several assays, one dataset",
      p("eDNA projects often run the same water or sediment samples through several primer sets (assays), each detecting a different
         group of organisms. The sampling information (where, when, how) is the same for every assay; only the sequencing results differ."),
      p("Established tools such as the ", ext_link("https://mdt.gbif.org/", "GBIF Metabarcoding Data Toolkit (MDT)"),
        " make publishing metabarcoding data straightforward, and they are designed around one assay per dataset. That suits many projects.
         MAT is built for the projects where the same samples go through several assays: it writes each sample once in an Event core and
         links every assay to it, so the whole project can be published as one dataset."),
      tags$table(class = "about-table",
        tags$thead(tags$tr(tags$th("Your project"), tags$th("A good fit"))),
        tags$tbody(
          tags$tr(tags$td("One assay, with an OTU table, taxonomy and sample sheet"),
                  tags$td(ext_link("https://mdt.gbif.org/", "GBIF MDT"), ", which guides you through mapping and publishing")),
          tags$tr(tags$td("Several assays run on the same samples, published as one dataset"), tags$td(class = "mat-col", "MAT")),
          tags$tr(tags$td("Data already in FAIRe workbooks"), tags$td(class = "mat-col", "MAT reads them directly, one workbook per assay")),
          tags$tr(tags$td("Marine data you want matched to WoRMS and checked with OBIS quality checks before upload"), tags$td(class = "mat-col", "MAT"))
        )
      ),
      div(class = "checklist", style = "margin-top: 14px;",
        tags$b("What makes MAT different"),
        tags$ul(style = "margin: 8px 0 0 0;",
          tags$li("One archive for any number of assays, with each sample's information written once."),
          tags$li("Reads FAIRe workbooks directly: if your data follows the FAIRe standard, there is nothing to reformat."),
          tags$li("Marine taxonomy from WoRMS, with a person reviewing anything uncertain before it is published."),
          tags$li("OBIS quality checks run before you upload."),
          tags$li("Open source and runs anywhere R runs, with no account or cloud service required.")
        )
      )
    ),

    # ---- How it works -----------------------------------------------------
    section("How it works", "The archive structure",
      p("MAT follows the ", ext_link("https://manual.obis.org/dna_data.html", "OBIS guidance for DNA-derived data"),
        ". Every physical sample becomes one Event. Each assay adds its own Occurrence and DNA Derived Data extension, linked back to the Event core by ",
        code("eventID"), "."),
      div(class = "arch",
        div(class = "arch-core", tags$b("Event core"), tags$small("one row per sample: date, coordinates, depth, method")),
        div(class = "arch-link", "eventID"),
        div(class = "arch-cols", arch_col("Assay 1"), arch_col("Assay 2"), arch_col("Assay N", more = TRUE))
      ),
      p(style = "margin-top: 14px;",
        "Each detection's ", code("occurrenceID"), " is built from the sample, the assay and the ASV (for example ",
        code("OcOm_2408_1_1_MarVer1_ASV_1079"), "), so it is unique across the whole archive even when ASV numbers repeat between assays.")
    ),

    # ---- How to use -------------------------------------------------------
    section("How to use it", "Seven steps from FAIRe files to an archive",
      div(class = "checklist",
        tags$b("Before you start, have ready:"),
        tags$ul(style = "margin: 8px 0 0 0;",
          tags$li("One FAIRe .xlsx workbook per assay, all describing the same samples."),
          tags$li("Files of one reference type only: all curated-database files, or all NCBI nt files."),
          tags$li("The ", code("samp_category"), " value that marks a real sample (controls are left out)."),
          tags$li("The accession of your raw reads (for example ENA or SRA), and the details for the dataset metadata.")
        )
      ),
      div(style = "height: 14px;"),
      div(class = "steps-grid",
        step_card(1, "Upload", "Enter a project ID and add one FAIRe workbook per assay. Assay names are detected from the file names."),
        step_card(2, "Validate", "Checks every sheet and header, and that all assays describe the same samples with the same metadata."),
        step_card(3, "Build", "Match each assay to its projectMetadata column(s), then build the Event core and the extensions. Every column is checked against the GBIF term definitions."),
        step_card(4, "Taxonomy", "Every name is matched to WoRMS. Review anything ambiguous or not recorded as marine (eDNA genuinely picks up birds, reptiles and land animals too), and exclude lab contamination if needed."),
        step_card(5, "QC checks", "OBIS quality checks: dates, coordinates, links between files, required fields and taxonomy coverage."),
        step_card(6, "Metadata", "Describe the dataset (people, abstract, methods, licence) by form or from the Excel template. MAT writes the eml.xml."),
        step_card(7, "Results", "Review the one-page report, then download the archive or save it as a draft for review.")
      ),
      p(style = "margin-top: 14px;",
        tags$b("Then publish it through an IPT. "),
        "Upload ", code("Event.csv"), " as the Event core, add each Occurrence and DNA Derived Data file as an extension linked by ",
        code("eventID"), ", and register the resource with OBIS or GBIF. The Publish tab in MAT stores the final version for your team;
         it does not send data to OBIS or GBIF by itself.")
    ),

    # ---- Ways to run ------------------------------------------------------
    section("Ways to run MAT", "Start simple, add more only if you need it",
      p("Nothing beyond R is required. If your data is in FAIRe format, you can fork or clone MAT and produce a Darwin Core Archive the same day."),
      div(class = "feature-grid two-col",
        feature("laptop", "1. On your own computer (start here)",
                "Clone the repository, install the R packages and run the app. No account, no cloud and no AWS needed: build the archive,
                 download it, keep it in a local folder and publish it through an IPT or share it with your OBIS node."),
        feature("cloud-check", "2. Add storage to keep a record (optional)",
                "Connect your own AWS S3 bucket and an email account in .Renviron. MAT then adds team accounts, saved drafts, a Publish
                 folder and reports, so every dataset you have produced can be seen in the app."),
        feature("terminal", "3. Scripts only (optional)",
                "Prefer the command line? Five R scripts (01 to 05) and config.R produce the same archive without the app."),
        feature("server", "4. Host it for your team (optional)",
                "Put the app on Shiny Server (for example a Nectar or other cloud VM), shinyapps.io or Posit Connect Cloud, so the team uses one
                 link. Not required: hosting only adds a shared place to work.")
      ),
      tags$pre(class = "about-code", paste0(
        "git clone ", MAT_REPO_URL, ".git\n",
        "# then in R, from the cloned folder:\n",
        "shiny::runApp()"))
    ),

    # ---- Adapt ------------------------------------------------------------
    section("Adapt it to your project", "Fork it and make it yours",
      p("Most projects need no code changes at all: the settings below are chosen in the app or set in one file. Fork the repository
         on GitHub to keep your own copy."),
      tags$table(class = "about-table",
        tags$thead(tags$tr(tags$th("To change"), tags$th("Where"))),
        tags$tbody(
          tags$tr(tags$td("Which assays, and which projectMetadata columns belong to each (including combined assays such as MiFish-U + MiFish-E2)"),
                  tags$td("In the app, Step 3. Any number of assays.")),
          tags$tr(tags$td("Which samples are real and which are controls"),
                  tags$td("In the app, Step 1 (the ", code("samp_category"), " value).")),
          tags$tr(tags$td("Dataset metadata: people, abstract, methods, licence"),
                  tags$td("In the app, Step 6, or fill in the Excel template it provides and upload it.")),
          tags$tr(tags$td("Storage bucket and AWS keys (optional)"),
                  tags$td(code(".Renviron"), " (copy ", code(".Renviron.example"), ").")),
          tags$tr(tags$td("Who can create an account: your organisation's email domain"),
                  tags$td(code("ALLOWED_EMAIL_DOMAIN"), " in ", code(".Renviron"), ". Set this when you fork.")),
          tags$tr(tags$td("Who is an admin, and the contact address for access"),
                  tags$td(code("ADMIN_EMAILS"), " and ", code("SUPPORT_CONTACT_EMAIL"), " in ", code(".Renviron"), ".")),
          tags$tr(tags$td("Settings for a scripted run, without the app"),
                  tags$td(code("config.R"), ", then the five scripts in ", code("scripts/"), ".")),
          tags$tr(tags$td("Darwin Core term definitions used by the mapping check"),
                  tags$td("The CSV files in ", code("reference/"), "; refresh them when GBIF publishes new versions.")),
          tags$tr(tags$td("Name, logo and colours"),
                  tags$td(code("app.R"), " (header and styles) and ", code("www/"), " (logo)."))
        )
      ),
      p(style = "margin-top: 14px;",
        tags$b("Why accounts are limited to one organisation: "),
        "an account can see the team's saved drafts and published archives. Sign-up is therefore open only to one email domain,
         new accounts can save drafts but not publish, and only an admin can give publishing rights. Anyone can still build and download
         archives without an account."),
      p(tags$b("Non-marine data: "),
        "MAT matches names against WoRMS, the register OBIS uses. For freshwater or terrestrial projects, the taxonomy step in ",
        code("R/worms_match.R"), " would need another reference, such as the GBIF Backbone.")
    ),

    # ---- Quality ----------------------------------------------------------
    section("Built-in quality control", "What MAT checks for you",
      div(class = "feature-grid",
        feature("diagram-3", "One sample, written once", "Sample metadata is checked to be identical across all assay files before anything is built."),
        feature("funnel", "Controls excluded", "Negative and positive controls are kept out of the archive automatically."),
        feature("search", "WoRMS matching without guessing", "Ambiguous or unmatched names are shown for review, never filled in. Unidentified sequences become Biota incertae sedis, as OBIS requires."),
        feature("water", "Not-marine review", "Taxa WoRMS doesn't record as marine are listed with the outcome OBIS's own QC will give them, and can be excluded."),
        feature("check2-circle", "OBIS QC and Darwin Core checks", "Runs obistools checks and compares every column with the official GBIF term definitions, as the IPT does."),
        feature("journal-text", "Every decision recorded", "Excluded taxa are written into the eml.xml methods, so data users can see what was removed and why.")
      )
    ),

    # ---- Standards --------------------------------------------------------
    section("Standards", "Built on community standards",
      tags$ul(class = "standards",
        tags$li(ext_link("https://fair-edna.github.io/", "FAIRe"), ": the eDNA data and metadata format MAT reads."),
        tags$li(ext_link("https://dwc.tdwg.org/", "Darwin Core"), " and the ",
                ext_link("https://rs.gbif.org/extension/gbif/1.0/dna_derived_data_2024-07-11.xml", "DNA Derived Data extension"), ": the output format."),
        tags$li(ext_link("https://manual.obis.org/dna_data.html", "OBIS DNA-derived data guidance"), ": the Event core pattern."),
        tags$li(ext_link("https://www.marinespecies.org/", "WoRMS"), ": the taxonomic reference for scientific names."),
        tags$li(ext_link("https://ipt.gbif.org/manual/en/ipt/latest/gbif-metadata-profile", "GBIF metadata profile (EML)"), ": the dataset metadata.")
      )
    ),

    # ---- Open source ------------------------------------------------------
    section("Open source", "Use it, host it, improve it",
      p("MAT is free and open source under the MIT licence. Fork it, adapt it to your own project and run it wherever suits you; see the ",
        ext_link(MAT_REPO_URL, "README on GitHub"), " for installation. Bug reports and suggestions are welcome through ",
        ext_link(paste0(MAT_REPO_URL, "/issues"), "GitHub Issues"), "."),
      div(class = "cite-box",
        tags$b("How to cite"), tags$br(),
        paste0("Dissanayaka, A. (2026). Multi-Assay Toolkit (MAT): convert multi-assay FAIRe eDNA data into an OBIS-ready Darwin Core Archive (v",
        MAT_VERSION, "). Minderoo OceanOmics Centre at UWA. "), ext_link(MAT_REPO_URL, MAT_REPO_URL)),
      p(style = "margin-top: 14px;",
        "Developed by the Minderoo OceanOmics Centre at UWA. With thanks to the ocean experts team at IOC-UNESCO, the OBIS Secretariat and
         node network, and the OBIS Australia node. Questions: ",
        tags$a(href = paste0("mailto:", contact_email()), contact_email(), .noWS = "outside"), ".")
    )
  )
}
