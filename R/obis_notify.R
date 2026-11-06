### R/obis_notify.R
#
# "Notify OBIS node" in the Publish tab: emails the OBIS node that a
# published archive is ready, with its S3 link, so the node can harvest it.
# Sent through the same SMTP account as the login codes (R/user_auth.R).
#
#   OBIS_NODE_EMAIL - default recipient(s), comma-separated. Optional: when
#                     unset the "To" box starts empty and the user types it.

obis_node_email <- function() trimws(Sys.getenv("OBIS_NODE_EMAIL"))

# Who the email says the archive comes from. MAT_ORG_NAME lets a fork use its own name.
mat_org_name <- function() {
  v <- trimws(Sys.getenv("MAT_ORG_NAME"))
  if (nzchar(v)) v else "OceanOmics Centre"
}

# "a@x.org, b@y.org" -> c("a@x.org", "b@y.org")
split_emails <- function(x) {
  v <- trimws(unlist(strsplit(x %||% "", "[,;]")))
  v[nzchar(v)]
}

#' Build the notification email.
#' @return list(subject, html)
obis_notify_email <- function(project, archive_url, file_name, size = "", last_modified = "",
                              report_url = NULL, title = "", sender_name = "", sender_email = "",
                              note = "") {
  org <- mat_org_name()
  esc <- function(x) htmltools::htmlEscape(as.character(x %||% ""))
  title <- trimws(title %||% "")
  note <- trimws(note %||% "")
  sender_name <- trimws(sender_name %||% "")
  who <- if (nzchar(sender_name)) paste0(esc(sender_name), " (", esc(sender_email), ")") else esc(sender_email)

  row <- function(label, value) sprintf(
    '<tr><td style="padding:8px 12px;color:#55607a;font-size:13px;white-space:nowrap;vertical-align:top;border-bottom:1px solid #e5ecf7;">%s</td><td style="padding:8px 12px;color:#0a1b2e;font-size:14px;border-bottom:1px solid #e5ecf7;">%s</td></tr>',
    label, value)
  link <- function(url, text = url) sprintf('<a href="%s" style="color:#003087;word-break:break-all;">%s</a>', esc(url), esc(text))

  rows <- paste0(
    row("Project ID", paste0("<strong>", esc(project), "</strong>")),
    if (nzchar(title)) row("Dataset title", esc(title)) else "",
    row("Archive", esc(file_name)),
    row("Download link", link(archive_url)),
    if (!is.null(report_url)) row("Analysis report", link(report_url, "View report (PNG)")) else "",
    if (nzchar(size)) row("Size", esc(size)) else "",
    if (nzchar(last_modified)) row("Last updated", paste0(esc(last_modified), " UTC")) else "",
    row("Submitted by", who)
  )

  note_html <- if (nzchar(note)) sprintf(
    '<div style="margin:20px 0 0;padding:12px 16px;background:#eef3fb;border-left:3px solid #003087;color:#0a1b2e;font-size:14px;white-space:pre-wrap;"><div style="font-size:12px;color:#55607a;margin-bottom:4px;">Message from the submitter</div>%s</div>',
    esc(note)) else ""

  html <- paste0(
    '<!doctype html><html><body style="margin:0;padding:0;background:#f4f6fa;">',
    '<table role="presentation" width="100%" cellpadding="0" cellspacing="0" style="background:#f4f6fa;padding:24px 0;"><tr><td align="center">',
    '<table role="presentation" width="600" cellpadding="0" cellspacing="0" style="max-width:600px;width:100%;background:#ffffff;font-family:Segoe UI,Helvetica,Arial,sans-serif;">',
    '<tr><td style="background:#0a1b2e;padding:18px 24px;border-bottom:3px solid #daaa00;">',
    '<div style="color:#ffffff;font-size:18px;font-weight:600;">Multi-Assay Toolkit (MAT)</div>',
    '<div style="color:#62d5ff;font-size:13px;margin-top:2px;">eDNA dataset ready for OBIS</div></td></tr>',
    '<tr><td style="padding:24px;color:#0a1b2e;font-size:14px;line-height:1.55;">',
    '<p style="margin:0 0 12px;">Hi OBIS node team,</p>',
    '<p style="margin:0 0 12px;">The ', esc(org), ' has recently generated a Darwin Core Archive for its new project, <strong>',
    esc(if (nzchar(title)) title else project), '</strong>, and it is ready to be published to OBIS. ',
    'We have included the S3 link below so you can harvest it directly.</p>',
    '<p style="margin:0 0 12px;color:#55607a;font-size:13px;">The archive holds one Event core (one row per sample), ',
    'an Occurrence and a DNA Derived Data extension for each assay, and eml.xml metadata. It has passed MAT\'s QC checks.</p>',
    sprintf('<p style="margin:20px 0;text-align:center;"><a href="%s" style="display:inline-block;background:#003087;color:#ffffff;text-decoration:none;padding:10px 22px;border-radius:4px;font-weight:600;">Download the archive</a></p>', esc(archive_url)),
    '<table role="presentation" width="100%" cellpadding="0" cellspacing="0" style="border-collapse:collapse;border-top:1px solid #e5ecf7;">', rows, '</table>',
    note_html,
    '<p style="margin:20px 0 0;">If anything needs changing before publication, please reply to this email',
    if (nzchar(sender_email)) paste0(' or contact ', link(paste0("mailto:", sender_email), sender_email)) else "",
    '.</p><p style="margin:12px 0 0;">Many thanks,<br>', if (nzchar(sender_name)) paste0(esc(sender_name), '<br>') else "", esc(org), '</p>',
    '</td></tr>',
    '<tr><td style="background:#eef3fb;padding:12px 24px;color:#55607a;font-size:12px;">Sent from the Multi-Assay Toolkit (MAT), which converts FAIRe eDNA data into Darwin Core Archives for OBIS.</td></tr>',
    '</table></td></tr></table></body></html>'
  )

  subject <- paste0("New Darwin Core Archive ready for OBIS: ", if (nzchar(title)) paste0(title, " (", project, ")") else project)
  list(subject = subject, html = html)
}

#' Send it. `reply_to` is the person who pressed the button, so the node's
#' reply reaches them rather than the no-reply sending address; they get a copy.
send_obis_notify_email <- function(to, subject, html, reply_to = NULL, cc = NULL) {
  msg <- emayili::envelope() |>
    emayili::from(Sys.getenv("SMTP_USER")) |>
    emayili::to(to) |>
    emayili::subject(subject) |>
    emayili::html(html)
  if (length(cc) > 0) msg <- emayili::cc(msg, cc)
  if (!is.null(reply_to) && nzchar(reply_to)) msg <- emayili::reply(msg, reply_to)
  smtp <- .smtp_server()
  smtp(msg)
  invisible(TRUE)
}
