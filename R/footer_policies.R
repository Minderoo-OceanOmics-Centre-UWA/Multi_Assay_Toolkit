### R/footer_policies.R
#
# The site footer (brand, resources, connect, policies) and the three policy
# texts it opens. The policies describe what THIS app actually does with
# data - keep them in step with R/user_auth.R and R/archive_history.R if
# either changes. Anyone hosting their own copy should review them, since
# they are written for the instance run by the Minderoo OceanOmics Centre.

MAT_WEBSITE_URL  <- "https://www.uwa.edu.au/oceans-institute/partnerships/minderoo-oceanomics-centre-at-uwa"
MAT_LINKEDIN_URL <- ""   # paste the centre's LinkedIn page URL here; the link appears once it is set
POLICY_UPDATED   <- "20 October 2026"

footer_css <- "
  .app-footer { display: block; text-align: left; padding: 40px 32px 0 32px; }
  .footer-grid { max-width: 1180px; margin: 0 auto; display: grid; grid-template-columns: 1.6fr 1fr 1fr 1fr; gap: 36px; }
  .footer-brand .footer-partner { display: inline-block; margin-bottom: 14px; }
  .footer-brand p { color: #a9c2d1; font-size: 0.84rem; line-height: 1.6; margin: 0 0 6px 0; }
  .footer-brand .footer-name { color: #ffffff; font-weight: 700; font-size: 0.95rem; }
  .footer-col h6 { color: #ffffff; font-size: 0.78rem; font-weight: 700; letter-spacing: 0.08em; text-transform: uppercase; margin: 0 0 12px 0; }
  .footer-col a { display: block; color: #a9c2d1; font-size: 0.86rem; padding: 3px 0; text-decoration: none; }
  .footer-col a:hover { color: #ffffff; text-decoration: none; }
  .footer-col a .bi { margin-right: 6px; opacity: 0.8; }
  .footer-bottom {
    max-width: 1180px; margin: 32px auto 0 auto; padding: 16px 0 20px 0; border-top: 1px solid rgba(255,255,255,0.10);
    display: flex; justify-content: space-between; flex-wrap: wrap; gap: 8px; color: #7f9fb3; font-size: 0.78rem;
  }
  .policy-body h6 { font-weight: 700; margin-top: 18px; }
  .policy-body p, .policy-body li { color: #3d3c38; font-size: 0.92rem; line-height: 1.6; }
  .policy-updated { color: #898781; font-size: 0.8rem; }
  @media (max-width: 860px) { .footer-grid { grid-template-columns: 1fr 1fr; } }
  @media (max-width: 520px) { .footer-grid { grid-template-columns: 1fr; } }
"

app_footer_ui <- function() {
  ext <- function(href, icon, text) {
    tags$a(href = href, target = "_blank", rel = "noopener noreferrer", bsicons::bs_icon(icon), text)
  }
  # Organisation logo: www/partner_logo.(png|jpg|jpeg|svg) is gitignored, so
  # a fork gets a placeholder instead and can drop in its own file.
  logo <- list.files("www", pattern = "^partner_logo\\.(png|jpe?g|svg)$")
  logo_ui <- if (length(logo) > 0) {
    div(class = "footer-partner", tags$img(src = logo[1], alt = "Partner organisations"))
  } else {
    div(class = "footer-partner placeholder",
        title = "Add your organisation's logo as www/partner_logo.png (or .jpg / .svg)",
        bsicons::bs_icon("image"), " Your organisation's logo")
  }

  div(class = "app-footer",
    div(class = "footer-grid",
      div(class = "footer-brand",
          logo_ui,
          p(class = "footer-name", "Multi-Assay Toolkit (MAT)"),
          p("Publish multi-assay eDNA metabarcoding data as one Darwin Core Archive for OBIS and GBIF."),
          p("Developed by the Minderoo OceanOmics Centre at UWA.")),
      div(class = "footer-col",
          h6("Resources"),
          ext(MAT_REPO_URL, "github", "Source code"),
          ext(paste0(MAT_REPO_URL, "/issues"), "bug", "Report an issue"),
          actionLink("footer_about", tagList(bsicons::bs_icon("book"), "Documentation & citation")),
          ext(paste0(MAT_REPO_URL, "/blob/main/LICENSE"), "file-earmark-text", "MIT licence")),
      div(class = "footer-col",
          h6("Connect"),
          ext(MAT_WEBSITE_URL, "globe2", "Website"),
          if (nzchar(MAT_LINKEDIN_URL)) ext(MAT_LINKEDIN_URL, "linkedin", "LinkedIn"),
          tags$a(href = paste0("mailto:", contact_email()), bsicons::bs_icon("envelope"), "Contact us")),
      div(class = "footer-col",
          h6("Policies"),
          actionLink("footer_privacy", tagList(bsicons::bs_icon("shield-lock"), "Privacy")),
          actionLink("footer_terms", tagList(bsicons::bs_icon("journal-check"), "Terms of use")),
          actionLink("footer_data", tagList(bsicons::bs_icon("database"), "Data & storage")))
    ),
    div(class = "footer-bottom",
        span(paste0("© ", format(Sys.Date(), "%Y"), " Minderoo OceanOmics Centre at UWA")),
        span(paste0("Version ", MAT_VERSION, " · Open source under the MIT licence")))
  )
}

policy_modal <- function(which) {
  updated <- p(class = "policy-updated", paste("Last updated:", POLICY_UPDATED))
  contact <- p("Questions: ", tags$a(href = paste0("mailto:", contact_email()), contact_email(), .noWS = "after"), ".")
  body <- switch(which,
    privacy = list(
      title = "Privacy",
      content = tagList(
        updated,
        p("This notice covers the Multi-Assay Toolkit (MAT) service run by the Minderoo OceanOmics Centre at UWA. Copies hosted by other organisations are run by them."),
        h6("If you use MAT without an account"),
        p("No personal information is collected. Files you upload are processed for your session only and are not kept after it ends, unless you are signed in and choose to save a draft or publish."),
        h6("If you create an account"),
        tags$ul(
          tags$li("We store your email address, your role and the date you signed up."),
          tags$li("Your password is never stored - only a salted, one-way hash of it."),
          tags$li("Verification and password-reset codes are emailed to you, stored only as a hash, and expire after 15 minutes."),
          tags$li("After repeated failed sign-ins, we email you a warning.")),
        p("Account details are kept in the team's secure cloud storage and are used only to run the service."),
        h6("Name lookups"),
        p("WoRMS matching and the Check and Search features send scientific names to public services (WoRMS, OBIS, GBIF, Wikipedia, FishBase and SeaLifeBase). No personal information is sent with them."),
        h6("Cookies and tracking"),
        p("MAT does not use analytics, advertising or tracking cookies."),
        contact)),
    terms = list(
      title = "Terms of use",
      content = tagList(
        updated,
        tags$ul(
          tags$li("MAT is free, open-source software released under the MIT licence and provided \"as is\", without warranty of any kind."),
          tags$li("You are responsible for having the right to publish the data you process, and for checking the archive before you publish it."),
          tags$li("MAT is an independent tool. It is not affiliated with or endorsed by OBIS, GBIF or WoRMS, and results depend on those services being available and current."),
          tags$li("Accounts are for staff of the organisation running this service. Do not share your account; administrators may change or remove access."),
          tags$li("Please use the hosted service fairly. To process large volumes, run your own copy - see the source code.")),
        contact)),
    data = list(
      title = "Data & storage",
      content = tagList(
        updated,
        h6("Your data stays yours"),
        p("MAT does not publish anything to OBIS or GBIF. You publish the archive yourself through an IPT, under the licence you choose in the dataset metadata."),
        h6("What is stored, and where"),
        tags$ul(
          tags$li(strong("Without an account:"), " nothing is stored. Download the archive and keep it on your own computer."),
          tags$li(strong("Saved drafts and published archives:"), " kept in the team's cloud storage, with a one-page report for each. Every account holder of this service can see and download them."),
          tags$li(strong("Your own copy of MAT:"), " archives go to the storage you connect, or only to your computer if you connect none.")),
        h6("Removing data"),
        p("To remove a saved draft or archive, or your account, contact the team below."),
        contact))
  )
  modalDialog(title = body$title, size = "l", easyClose = TRUE, footer = modalButton("Close"),
              div(class = "policy-body", body$content))
}
