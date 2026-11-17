"""Email Anushka (cc Emma) once about each open pull request that needs review.

Run every hour by .github/workflows/notify-pull-requests.yml. A pull request
that has been emailed gets the label "review-email-sent", so it's never emailed
twice; drafts are skipped until they're marked ready. Uses the GitHub CLI (gh)
for the pull request list and send_notification.py for the email.

Why a schedule and not the pull_request_target trigger: from December 2026
GitHub restricts pull_request_target on public repositories, and the
organisation's Actions settings can't be changed from this repo.
"""

import json
import os
import subprocess
import sys

sys.path.insert(0, os.path.dirname(__file__))
import send_notification  # noqa: E402

LABEL = "review-email-sent"
SKIP_BRANCHES = {"auto/reference-terms"}   # the quarterly check emails about these itself


def gh(*args):
    return subprocess.run(["gh", *args], check=True, capture_output=True, text=True).stdout


def main():
    if not (os.environ.get("SMTP_USER") and os.environ.get("SMTP_PASSWORD") and os.environ.get("NOTIFY_TO")):
        print("Email not sent: add the SMTP_USER, SMTP_PASSWORD and NOTIFY_TO repository secrets.")
        return 0

    # The label must exist before it can be added (no-op if it already does).
    subprocess.run(["gh", "label", "create", LABEL, "--color", "daaa00",
                    "--description", "Review email sent to the MAT team"], capture_output=True)

    prs = json.loads(gh("pr", "list", "--state", "open", "--limit", "100", "--json",
                        "number,title,author,headRefName,headRepositoryOwner,baseRefName,url,body,isDraft,labels,files"))
    todo = [p for p in prs
            if not p["isDraft"]
            and LABEL not in {l["name"] for l in p["labels"]}
            and p["headRefName"] not in SKIP_BRANCHES]
    print(f"{len(prs)} open pull request(s), {len(todo)} to email.")

    for p in todo:
        n = p["number"]
        owner = (p.get("headRepositoryOwner") or {}).get("login", "")
        files = [f["path"] for f in (p.get("files") or [])]
        os.environ.update({
            "MAIL_SUBJECT": f"MAT: pull request #{n} needs review - {p['title']}",
            "MAIL_HEADLINE": "A pull request needs your review",
            "MAIL_INTRO": f"A pull request (#{n}) on the Multi-Assay Toolkit repository needs your review.",
            "MAIL_FACTS": "\n".join([
                f"Title: {p['title']}",
                f"Opened by: {(p.get('author') or {}).get('login', 'unknown')}",
                f"Changes from: `{owner + ':' if owner else ''}{p['headRefName']}`",
                f"Into: `{p['baseRefName']}`",
            ]),
            "MAIL_FILES": "\n".join(files),
            "MAIL_BODY_TEXT": p.get("body") or "",
            "MAIL_BODY_FILE": "",
            "MAIL_LINK_URL": p["url"],
            "MAIL_LINK_LABEL": f"Review pull request #{n}",
        })
        send_notification.main()
        gh("pr", "edit", str(n), "--add-label", LABEL)
        print(f"Emailed about #{n} and labelled it.")
    return 0


if __name__ == "__main__":
    sys.exit(main())
