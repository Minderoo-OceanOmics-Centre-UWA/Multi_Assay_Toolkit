"""Send a branded MAT notification email from a GitHub Actions workflow.

Used by .github/workflows/notify-pull-requests.yml (a pull request needs
review) and update-reference-terms.yml (the quarterly reference check found
changes). Everything comes from environment variables, so no text from a pull
request is ever pasted into a shell command:

  SMTP_USER, SMTP_PASSWORD      repository secrets (the app's Gmail account)
  SMTP_HOST, SMTP_PORT          optional, default smtp.gmail.com / 587
  NOTIFY_TO, NOTIFY_CC          repository secrets (comma-separated)
  MAIL_SUBJECT                  subject line
  MAIL_HEADLINE                 big line at the top of the card
  MAIL_INTRO                    first paragraph
  MAIL_FACTS                    "Label: value" lines, shown as a small table
  MAIL_BODY_FILE                optional markdown-ish file shown below (summary)
  MAIL_BODY_TEXT                optional plain text shown below (e.g. PR description)
  MAIL_LINK_URL, MAIL_LINK_LABEL  the button
  MAIL_FILES                    optional newline-separated list of changed files
"""

import html
import os
import re
import smtplib
import sys
from email.message import EmailMessage

NAVY, BLUE, GOLD, TINT, TEXT, MUTED = "#0a1b2e", "#003087", "#daaa00", "#eef3fb", "#0a1b2e", "#55607a"


def env(name, default=""):
    return os.environ.get(name, default).strip()


def esc(s):
    return html.escape(s or "", quote=True)


def inline(s):
    """Escape, then allow `code`, **bold** and [text](https://link)."""
    s = esc(s)
    s = re.sub(r"`([^`]+)`", r'<code style="background:#e5ecf7;padding:1px 4px;border-radius:3px;">\1</code>', s)
    s = re.sub(r"\*\*([^*]+)\*\*", r"<strong>\1</strong>", s)
    s = re.sub(r"\[([^\]]+)\]\((https?://[^)\s]+)\)", rf'<a href="\2" style="color:{BLUE};">\1</a>', s)
    return s


def markdown_lite(text):
    """Headings, bullets, tables and paragraphs - enough for the summary file."""
    out, items, rows = [], [], []

    def flush():
        nonlocal items, rows
        if items:
            out.append('<ul style="margin:0 0 12px;padding-left:20px;">' + "".join(f"<li>{i}</li>" for i in items) + "</ul>")
            items = []
        if rows:
            cells = [r for r in rows if not re.fullmatch(r"\|?[\s:|-]+\|?", r)]
            trs = []
            for n, r in enumerate(cells):
                tag = "th" if n == 0 else "td"
                tds = "".join(f'<{tag} style="border:1px solid #d6deeb;padding:4px 8px;text-align:left;">{inline(c.strip())}</{tag}>'
                              for c in r.strip().strip("|").split("|"))
                trs.append(f"<tr>{tds}</tr>")
            out.append('<table style="border-collapse:collapse;font-size:13px;margin:0 0 12px;">' + "".join(trs) + "</table>")
            rows = []

    for line in text.splitlines():
        if line.startswith("|"):
            rows.append(line); continue
        if re.match(r"^\s*[-*] ", line):
            items.append(inline(re.sub(r"^\s*[-*] ", "", line))); continue
        flush()
        if not line.strip() or line.strip() == "---":
            continue
        h = re.match(r"^(#{1,4})\s+(.*)", line)
        if h:
            out.append(f'<h3 style="margin:16px 0 6px;font-size:15px;color:{NAVY};">{inline(h.group(2))}</h3>')
        else:
            out.append(f'<p style="margin:0 0 10px;">{inline(line)}</p>')
    flush()
    return "".join(out)


def main():
    user, password = env("SMTP_USER"), env("SMTP_PASSWORD")
    to = [a.strip() for a in env("NOTIFY_TO").split(",") if a.strip()]
    cc = [a.strip() for a in env("NOTIFY_CC").split(",") if a.strip()]
    preview = env("MAIL_PREVIEW_FILE")   # set to write the HTML to a file instead of sending
    if not preview and not (user and password and to):
        print("Email not sent: add the SMTP_USER, SMTP_PASSWORD and NOTIFY_TO repository secrets.")
        return 0

    facts = [l.split(":", 1) for l in env("MAIL_FACTS").splitlines() if ":" in l]
    facts_html = "".join(
        f'<tr><td style="padding:3px 12px 3px 0;color:{MUTED};white-space:nowrap;vertical-align:top;">{esc(k.strip())}</td>'
        f'<td style="padding:3px 0;">{inline(v.strip())}</td></tr>' for k, v in facts)

    files = [f for f in env("MAIL_FILES").splitlines() if f.strip()]
    files_html = ""
    if files:
        shown = files[:25]
        more = f'<li style="color:{MUTED};">and {len(files) - 25} more</li>' if len(files) > 25 else ""
        files_html = (f'<h3 style="margin:16px 0 6px;font-size:15px;color:{NAVY};">Changed files ({len(files)})</h3>'
                      '<ul style="margin:0 0 12px;padding-left:20px;font-family:Consolas,monospace;font-size:12px;">'
                      + "".join(f"<li>{esc(f)}</li>" for f in shown) + more + "</ul>")

    body_html = ""
    if env("MAIL_BODY_FILE") and os.path.exists(env("MAIL_BODY_FILE")):
        body_html = markdown_lite(open(env("MAIL_BODY_FILE"), encoding="utf-8").read())
    elif env("MAIL_BODY_TEXT"):
        text = env("MAIL_BODY_TEXT")
        text = text if len(text) <= 1500 else text[:1500] + " …"
        body_html = (f'<h3 style="margin:16px 0 6px;font-size:15px;color:{NAVY};">Description</h3>'
                     f'<div style="background:{TINT};border-left:3px solid {BLUE};padding:10px 12px;white-space:pre-wrap;">{esc(text)}</div>')

    button = ""
    if env("MAIL_LINK_URL"):
        button = (f'<p style="margin:20px 0 4px;"><a href="{esc(env("MAIL_LINK_URL"))}" '
                  f'style="background:{BLUE};color:#ffffff;text-decoration:none;padding:10px 18px;border-radius:6px;font-weight:600;display:inline-block;">'
                  f'{esc(env("MAIL_LINK_LABEL", "Open on GitHub"))}</a></p>')

    html_body = f"""<!doctype html><html><body style="margin:0;background:#f4f6fa;">
<table role="presentation" width="100%" cellpadding="0" cellspacing="0" style="background:#f4f6fa;padding:24px 0;"><tr><td align="center">
<table role="presentation" width="620" cellpadding="0" cellspacing="0" style="max-width:620px;width:100%;background:#ffffff;border-radius:8px;overflow:hidden;font-family:Segoe UI,Arial,sans-serif;">
<tr><td style="background:{NAVY};padding:18px 24px;border-bottom:3px solid {GOLD};">
  <div style="color:#ffffff;font-size:18px;font-weight:600;">Multi-Assay Toolkit (MAT)</div>
  <div style="color:#62d5ff;font-size:13px;margin-top:2px;">{esc(env("MAIL_HEADLINE"))}</div></td></tr>
<tr><td style="padding:24px;color:{TEXT};font-size:14px;line-height:1.55;">
  <p style="margin:0 0 12px;">Hi Anushka,</p>
  <p style="margin:0 0 14px;">{inline(env("MAIL_INTRO"))}</p>
  {f'<table style="font-size:14px;margin:0 0 8px;">{facts_html}</table>' if facts_html else ""}
  {files_html}{body_html}{button}
</td></tr>
<tr><td style="background:{TINT};padding:12px 24px;color:{MUTED};font-size:12px;">
  Sent automatically by GitHub Actions for the Multi-Assay Toolkit repository. Nothing changes on the live app until a pull request is merged and deployed.</td></tr>
</table></td></tr></table></body></html>"""

    if preview:
        with open(preview, "w", encoding="utf-8") as f:
            f.write(html_body)
        print(f"Preview written to {preview} (nothing sent).")
        return 0

    msg = EmailMessage()
    msg["Subject"] = env("MAIL_SUBJECT", "MAT: something needs your review")
    msg["From"] = f"Multi-Assay Toolkit (MAT) <{user}>"
    msg["To"] = ", ".join(to)
    if cc:
        msg["Cc"] = ", ".join(cc)
    plain = "\n".join(filter(None, [
        "Hi Anushka,", "", env("MAIL_INTRO"), "",
        *[f"{k.strip()}: {v.strip()}" for k, v in facts],
        "", env("MAIL_LINK_URL")]))
    msg.set_content(plain)
    msg.add_alternative(html_body, subtype="html")

    host, port = env("SMTP_HOST", "smtp.gmail.com") or "smtp.gmail.com", int(env("SMTP_PORT", "587") or 587)
    with smtplib.SMTP(host, port, timeout=60) as s:
        s.starttls()
        s.login(user, password)
        s.send_message(msg)
    print(f"Email sent to {len(to)} recipient(s), cc {len(cc)}.")
    return 0


if __name__ == "__main__":
    sys.exit(main())
