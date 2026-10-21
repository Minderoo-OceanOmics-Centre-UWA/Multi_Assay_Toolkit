### R/user_auth.R
#
# Minimal login system for the app: sign up, email-code verification,
# login, forgot/reset password. Built for a SMALL, TRUSTED group (one
# organisation's own staff) - not a general-purpose auth system. Accounts
# only guard the shared S3 storage (saved drafts, published archives,
# reports); building and downloading an archive needs no account at all.
#
# WHY sign-up is restricted: anyone with an account can see and download
# the team's saved archives, including unpublished drafts. Limiting sign-up
# to one email domain means only the organisation's own staff can get that
# far, and new accounts start as "user" - only an admin can make someone a
# publisher. IF YOU FORK THIS REPO: set ALLOWED_EMAIL_DOMAIN and ADMIN_EMAILS
# in .Renviron to your own organisation (see .Renviron.example), or staff of
# the original organisation could sign up to your instance.
#
# Design choices, and why:
#  - Signup is restricted to ALLOWED_EMAIL_DOMAIN addresses (is_allowed_email()),
#    checked BEFORE anything else happens (no code is generated/sent for a
#    rejected address).
#  - Passwords are never stored or logged in the clear - only a sodium
#    password_store() hash (bcrypt-family, salted, slow-by-design). Emailed
#    verification/reset codes are likewise stored only as a sha256 hash with
#    a short expiry, the same reasoning as a password: even a leaked user
#    store or S3 log should not hand out working codes.
#  - The user store is one small JSON file on S3 (reuses the same .s3_get/
#    .s3_put wrappers as Draft/Publish in archive_history.R), NOT under the
#    biodiversity-public/ prefix used for archives - that prefix's bucket
#    policy is about sharing PUBLISHED DATA, and account credentials must
#    never sit next to it. Every read/write goes through a file lock
#    substitute (read-modify-write against the CURRENT S3 object) - fine at
#    this scale (a handful of staff signing up occasionally), not built for
#    high concurrent write volume.
#  - Email is sent via Gmail SMTP (emayili) using an app password - see
#    .Renviron.example. Chosen over AWS SES because this project's AWS
#    access is CLI/S3-only, with no console access to verify a sending
#    domain or leave the SES sandbox.
#
# Required environment variables (see .Renviron.example):
#   SMTP_USER      - the sending Gmail address (e.g. mat.noreply@gmail.com)
#   SMTP_PASSWORD  - a 16-character Gmail APP PASSWORD, not the account password
#   ALLOWED_EMAIL_DOMAIN - defaults to "uwa.edu.au" if unset

# Deliberately NOT library(sodium)/library(emayili) - both attach functions
# that collide with base R (emayili masks local(), raw() and text(); this
# app's own code uses local({...}) blocks elsewhere, e.g. Step 2's table
# rendering, which broke in a confusing way when emayili was attached).
# Every call below is fully namespaced instead.

# ---------------------------------------------------------------------
# Config
# ---------------------------------------------------------------------
AUTH_S3_KEY   <- "auth/users.json"        # deliberately outside biodiversity-public/
CODE_TTL_MINS <- 15                        # verification/reset codes expire after this long
CODE_LENGTH   <- 6
FAILED_LOGIN_WARNING_THRESHOLD <- 3        # wrong-password attempts before we email the account owner

# Seed admin(s): ALWAYS treated as admin regardless of what's in the user
# store (a hardcoded floor, not just a default), so an admin can never be
# accidentally locked out by a bad edit or a corrupted store. New accounts
# get role "admin" automatically if their email is in this list, "user"
# otherwise - see start_signup(). Set ADMIN_EMAILS in .Renviron
# (comma-separated) to choose your own admins; the address below is only the
# original deployment's default.
ADMIN_SEED_EMAILS <- local({
  v <- Sys.getenv("ADMIN_EMAILS")
  if (nzchar(v)) tolower(trimws(strsplit(v, ",")[[1]])) else c("anushka.dissanayaka@uwa.edu.au")
})

allowed_email_domain <- function() {
  v <- Sys.getenv("ALLOWED_EMAIL_DOMAIN")
  tolower(if (nzchar(v)) v else "uwa.edu.au")
}

#' Who to contact for access if you're outside the allowed domain (can't
#' self-signup) or need a role changed - shown on the signup screen and in
#' the Step 7 / Draft tab "log in required" popups.
contact_email <- function() {
  v <- Sys.getenv("SUPPORT_CONTACT_EMAIL")
  if (nzchar(v)) v else "oceanomics.tech@gmail.com"
}

#' TRUE if the email's domain matches the allowed one (case-insensitive,
#' exact domain match - "uwa.edu.au", not "notuwa.edu.au" or a subdomain
#' typo'd in either direction).
#
# NOTE: \\s does NOT work inside a [...] character class under R's default
# (non-perl) regex engine on at least some builds - [^@\\s] silently matched
# NOTHING containing "@", which meant every real address was rejected as
# "not a valid email". Fixed by excluding whitespace with a separate
# grepl("\\s", ...) check instead (where \s works fine, just not in a class).
is_allowed_email <- function(email) {
  email <- tolower(trimws(email %||% ""))
  grepl("^[^@]+@[^@]+$", email) && !grepl("\\s", email) &&
    identical(sub("^[^@]+@", "", email), allowed_email_domain())
}

email_looks_valid <- function(email) {
  email <- trimws(email %||% "")
  grepl("^[^@]+@[^@]+\\.[^@]+$", email) && !grepl("\\s", email)
}
# %||% is base R's own (R >= 4.4) - app.R already relies on it being
# available, so it is deliberately not redefined here.

# ---------------------------------------------------------------------
# Password / code hashing (sodium - never store either in the clear)
# ---------------------------------------------------------------------
hash_password   <- function(pw) sodium::password_store(pw)
verify_password <- function(pw, hash) {
  tryCatch(sodium::password_verify(hash, pw), error = function(e) FALSE)
}

# A 6-digit code is short enough that a plain fast hash (sha256) plus a
# short TTL is the right trade-off - not worth sodium's slow password hash
# for something this short-lived and low-entropy.
hash_code <- function(code) paste(as.character(sodium::sha256(charToRaw(as.character(code)))), collapse = "")
generate_code <- function() sprintf("%06d", sample(0:999999, 1))

# ---------------------------------------------------------------------
# User store on S3 - one JSON file, read-modify-write.
# Record shape per email (named list keyed by lowercased email):
#   list(email, password_hash, verified = TRUE/FALSE,
#        pending_code_hash, pending_code_expires, pending_purpose,  # signup or reset
#        created_at)
# ---------------------------------------------------------------------
.load_users <- function() {
  if (!.s3_exists(AUTH_S3_KEY)) return(list())
  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp), add = TRUE)
  .s3_get(AUTH_S3_KEY, tmp)
  tryCatch(jsonlite::fromJSON(tmp, simplifyVector = FALSE), error = function(e) list())
}

.save_users <- function(users) {
  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp), add = TRUE)
  jsonlite::write_json(users, tmp, auto_unbox = TRUE, pretty = TRUE)
  .s3_put(tmp, AUTH_S3_KEY)
}

get_user <- function(email) {
  users <- .load_users()
  users[[tolower(trimws(email))]]
}

# ---------------------------------------------------------------------
# Roles ("admin" | "user") - user management tab
# ---------------------------------------------------------------------
#' The email's role. A seed admin (see ADMIN_SEED_EMAILS) is ALWAYS "admin",
#' no matter what the store says. Returns NA if there's no account at all.
user_role <- function(email) {
  email <- tolower(trimws(email %||% ""))
  if (email %in% ADMIN_SEED_EMAILS) return("admin")
  rec <- get_user(email)
  if (is.null(rec)) return(NA_character_)
  rec$role %||% "user"  # accounts created before roles existed default to "user"
}

is_admin <- function(email) identical(user_role(email), "admin")

#' TRUE for "publisher" OR "admin" - admin is treated as a superuser that can
#' do everything a publisher can, on top of managing accounts. Three roles
#' exist: "admin" (manage users, plus everything below), "publisher" (can
#' move a draft into the public Publish folder, plus everything below),
#' "user" (can download and save to Draft only - the default for anyone who
#' just signs up). A signed-in user of ANY role can download/save a draft;
#' only can_publish() gates the Publish action specifically.
can_publish <- function(email) { r <- user_role(email); identical(r, "admin") || identical(r, "publisher") }

#' All accounts, for the User Management tab. Deliberately excludes password
#' hashes and pending codes - the UI never needs them and this keeps them
#' out of anything that gets rendered to a browser.
list_users <- function() {
  users <- .load_users()
  if (length(users) == 0) {
    return(data.frame(email = character(), role = character(), verified = logical(),
                       created_at = character(), stringsAsFactors = FALSE))
  }
  df <- do.call(rbind, lapply(users, function(u) data.frame(
    email = u$email, role = user_role(u$email), verified = isTRUE(u$verified),
    created_at = u$created_at %||% NA_character_, stringsAsFactors = FALSE
  )))
  df[order(df$email), ]
}

#' Change target_email's role. Only an admin may call this (checked here too,
#' not just hidden in the UI, since this is the function that actually
#' writes to the user store).
#' Blocks two things regardless of who's asking, to prevent accidental
#' lockouts: changing your OWN role, and changing a seed admin's role (which
#' would have no real effect anyway, since user_role() always overrides it -
#' better to say so than to silently do nothing).
set_user_role <- function(acting_admin_email, target_email, new_role) {
  if (!is_admin(acting_admin_email)) return(list(ok = FALSE, message = "Only an admin can change roles."))
  if (!new_role %in% c("admin", "publisher", "user")) return(list(ok = FALSE, message = "Invalid role."))
  target_email <- tolower(trimws(target_email))
  if (identical(target_email, tolower(trimws(acting_admin_email)))) {
    return(list(ok = FALSE, message = "You can't change your own role."))
  }
  if (target_email %in% ADMIN_SEED_EMAILS) {
    return(list(ok = FALSE, message = "This account's admin role is fixed in the app's configuration and can't be changed here."))
  }
  users <- .load_users()
  rec <- users[[target_email]]
  if (is.null(rec)) return(list(ok = FALSE, message = "No account with that email."))
  rec$role <- new_role
  users[[target_email]] <- rec
  .save_users(users)
  list(ok = TRUE, message = paste0(target_email, " is now ", new_role, "."))
}

# ---------------------------------------------------------------------
# Email sending
# ---------------------------------------------------------------------
smtp_configured <- function() {
  nzchar(Sys.getenv("SMTP_USER")) && nzchar(Sys.getenv("SMTP_PASSWORD"))
}

#' Accounts need both the S3 user store and email for login codes. Without
#' either, the app runs in local mode: no login screen, everyone can build
#' and download archives, and Draft/Publish are simply unavailable.
accounts_enabled <- function() {
  archive_history_enabled() && smtp_configured()
}

#' SMTP host/port are configurable (SMTP_HOST/SMTP_PORT), defaulting to
#' Gmail, so switching providers (e.g. to Outlook/Office 365) is an
#' .Renviron change, not a code change:
#'   Gmail:            smtp.gmail.com, 587 (the default - needs a Google
#'                      "app password", not the account's normal password)
#'   Outlook.com/Hotmail (personal): smtp-mail.outlook.com, 587 (same idea,
#'                      a Microsoft "app password")
#'   Office 365 (a UWA-managed mailbox): smtp.office365.com, 587 - BUT
#'                      Microsoft has been disabling SMTP AUTH (basic auth
#'                      with a password) tenant-wide by default; a UWA IT
#'                      admin would need to explicitly re-enable "SMTP AUTH"
#'                      for that specific mailbox first, or this fails with
#'                      an authentication error no password change can fix.
#'                      Not something to assume will just work the way
#'                      Gmail's app password does.
.smtp_server <- function() {
  emayili::server(
    host     = if (nzchar(Sys.getenv("SMTP_HOST"))) Sys.getenv("SMTP_HOST") else "smtp.gmail.com",
    port     = if (nzchar(Sys.getenv("SMTP_PORT"))) as.integer(Sys.getenv("SMTP_PORT")) else 587L,
    username = Sys.getenv("SMTP_USER"),
    password = Sys.getenv("SMTP_PASSWORD")
  )
}

send_code_email <- function(to, code, purpose = c("signup", "reset")) {
  purpose <- match.arg(purpose)
  subject <- if (purpose == "signup") "Multi-Assay Toolkit (MAT) - verify your email" else "Multi-Assay Toolkit (MAT) - reset your password"
  action  <- if (purpose == "signup") "finish creating your account" else "reset your password"
  msg <- emayili::envelope() |>
    emayili::from(Sys.getenv("SMTP_USER")) |>
    emayili::to(to) |>
    emayili::subject(subject) |>
    emayili::text(paste0(
      "Your Multi-Assay Toolkit (MAT) verification code is: ", code, "\n\n",
      "Enter this code in the app to ", action, ". ",
      "It expires in ", CODE_TTL_MINS, " minutes.\n\n",
      "If you didn't request this, you can ignore this email."
    ))
  smtp <- .smtp_server()
  smtp(msg)
  invisible(TRUE)
}

#' Sent once a login has failed FAILED_LOGIN_WARNING_THRESHOLD times in a row
#' for an account - a side channel, not shown on the login form itself (the
#' form's own message never changes, correct guess or not, so a failed
#' attempt can't be used to find out whether a warning was just sent, which
#' would itself leak whether the account exists).
send_login_warning_email <- function(to) {
  msg <- emayili::envelope() |>
    emayili::from(Sys.getenv("SMTP_USER")) |>
    emayili::to(to) |>
    emayili::subject("Multi-Assay Toolkit (MAT) - repeated failed login attempts") |>
    emayili::text(paste0(
      "Someone has tried to log in to your Multi-Assay Toolkit (MAT) account with the wrong password, ",
      "more than once.\n\n",
      "If this was you and you've forgotten your password, use \"Forgot password\" on the login screen to reset it.\n\n",
      "If this wasn't you, reset your password as a precaution.\n\n",
      "Questions? Contact ", contact_email(), "."
    ))
  smtp <- .smtp_server()
  smtp(msg)
  invisible(TRUE)
}

# ---------------------------------------------------------------------
# Signup: start (create pending, unverified record + email a code)
# ---------------------------------------------------------------------
#' @return list(ok = TRUE/FALSE, message = "...")
start_signup <- function(email, password) {
  email <- tolower(trimws(email))
  if (!email_looks_valid(email)) return(list(ok = FALSE, message = "Enter a valid email address."))
  if (!is_allowed_email(email)) {
    return(list(ok = FALSE, message = paste0("Only @", allowed_email_domain(), " email addresses can sign up.")))
  }
  if (is.null(password) || nchar(password) < 8) {
    return(list(ok = FALSE, message = "Password must be at least 8 characters."))
  }

  users <- .load_users()
  existing <- users[[email]]
  if (!is.null(existing) && isTRUE(existing$verified)) {
    return(list(ok = FALSE, message = "An account with this email already exists. Try logging in, or use Forgot password."))
  }

  code <- generate_code()
  users[[email]] <- list(
    email = email,
    password_hash = hash_password(password),
    verified = FALSE,
    role = if (email %in% ADMIN_SEED_EMAILS) "admin" else "user",
    pending_code_hash = hash_code(code),
    pending_code_expires = as.character(Sys.time() + CODE_TTL_MINS * 60),
    pending_purpose = "signup",
    created_at = as.character(Sys.time())
  )
  .save_users(users)

  sent <- tryCatch({ send_code_email(email, code, "signup"); TRUE },
                    error = function(e) { message("send_code_email failed: ", conditionMessage(e)); FALSE })
  if (!sent) return(list(ok = FALSE, message = "Could not send the verification email. Contact the app administrator."))
  list(ok = TRUE, message = paste0("A verification code was sent to ", email, "."))
}

#' Re-send a fresh code for a signup that's already pending (doesn't need the
#' password again - only start_signup() needs that, to create the record).
resend_signup_code <- function(email) {
  email <- tolower(trimws(email))
  users <- .load_users()
  rec <- users[[email]]
  if (is.null(rec) || isTRUE(rec$verified)) return(list(ok = FALSE, message = "No pending signup for this email - start signing up again."))
  code <- generate_code()
  rec$pending_code_hash <- hash_code(code)
  rec$pending_code_expires <- as.character(Sys.time() + CODE_TTL_MINS * 60)
  users[[email]] <- rec
  .save_users(users)
  sent <- tryCatch({ send_code_email(email, code, "signup"); TRUE }, error = function(e) FALSE)
  if (!sent) return(list(ok = FALSE, message = "Could not send the verification email. Contact the app administrator."))
  list(ok = TRUE, message = paste0("A new code was sent to ", email, "."))
}

#' Confirm the signup code -> account becomes usable.
verify_signup_code <- function(email, code) {
  email <- tolower(trimws(email))
  users <- .load_users()
  rec <- users[[email]]
  if (is.null(rec) || isTRUE(rec$verified)) return(list(ok = FALSE, message = "No pending signup for this email."))
  if (Sys.time() > as.POSIXct(rec$pending_code_expires)) {
    return(list(ok = FALSE, message = "That code has expired. Request a new one."))
  }
  if (!identical(hash_code(trimws(code)), rec$pending_code_hash)) {
    return(list(ok = FALSE, message = "Incorrect code."))
  }
  rec$verified <- TRUE
  rec$pending_code_hash <- NULL
  rec$pending_code_expires <- NULL
  rec$pending_purpose <- NULL
  users[[email]] <- rec
  .save_users(users)
  list(ok = TRUE, message = "Email verified - you can now log in.")
}

# ---------------------------------------------------------------------
# Login
# ---------------------------------------------------------------------
#' Read-modify-write (not get_user()'s read-only) because a wrong password
#' has to update failed_login_count, and a right one has to clear it.
attempt_login <- function(email, password) {
  email <- tolower(trimws(email))
  users <- .load_users()
  rec <- users[[email]]
  if (is.null(rec)) return(list(ok = FALSE, message = "Incorrect email or password."))
  if (!isTRUE(rec$verified)) return(list(ok = FALSE, message = "Please verify your email first (check your inbox for a code)."))

  if (!verify_password(password, rec$password_hash)) {
    rec$failed_login_count <- (rec$failed_login_count %||% 0) + 1
    users[[email]] <- rec
    .save_users(users)
    # Fires once per run of wrong attempts, not on every one after the
    # threshold too - a fresh burst (after a successful login resets the
    # count to 0) triggers it again.
    if (identical(rec$failed_login_count, FAILED_LOGIN_WARNING_THRESHOLD)) {
      tryCatch(send_login_warning_email(email), error = function(e) message("send_login_warning_email failed: ", conditionMessage(e)))
    }
    return(list(ok = FALSE, message = "Incorrect email or password."))
  }

  if (isTRUE(rec$failed_login_count > 0)) {
    rec$failed_login_count <- 0
    users[[email]] <- rec
    .save_users(users)
  }
  list(ok = TRUE, message = "Logged in.")
}

# ---------------------------------------------------------------------
# "Stay signed in" tokens
# ---------------------------------------------------------------------
# A reload starts a new Shiny session, so without this every reload means
# logging in again. On login the browser gets a long random token (kept in
# localStorage); the user store keeps only its sha256 hash and an expiry,
# like the emailed codes above. On reload the app sends the token back and
# check_login_token() signs the user in again. Tokens expire after
# SESSION_TTL_DAYS, are removed on logout, and ALL of a user's tokens are
# cleared by a password reset (sign out everywhere).
SESSION_TTL_DAYS      <- 14
MAX_SESSIONS_PER_USER <- 10   # one per browser/device; oldest dropped beyond this

.live_sessions <- function(sessions, now = Sys.time()) {
  Filter(function(s) !is.null(s$expires) && as.POSIXct(s$expires) > now, sessions %||% list())
}

#' New token for a verified user, or NULL. The plain token is returned once,
#' for the browser - it is never stored server-side.
create_login_token <- function(email) {
  email <- tolower(trimws(email))
  users <- .load_users()
  rec <- users[[email]]
  if (is.null(rec) || !isTRUE(rec$verified)) return(NULL)
  token <- paste(as.character(sodium::random(32)), collapse = "")
  expires <- Sys.time() + SESSION_TTL_DAYS * 86400
  sessions <- c(.live_sessions(rec$sessions), list(list(token_hash = hash_code(token), expires = as.character(expires))))
  rec$sessions <- utils::tail(sessions, MAX_SESSIONS_PER_USER)
  users[[email]] <- rec
  .save_users(users)
  list(token = token, expires = expires)
}

#' TRUE if `token` is a live (unexpired) token for this verified user.
check_login_token <- function(email, token) {
  email <- tolower(trimws(email %||% ""))
  if (!nzchar(email) || is.null(token) || !nzchar(token)) return(FALSE)
  rec <- get_user(email)
  if (is.null(rec) || !isTRUE(rec$verified)) return(FALSE)
  h <- hash_code(token)
  any(vapply(.live_sessions(rec$sessions), function(s) identical(s$token_hash, h), logical(1)))
}

#' Remove one token (logout on this browser). Expired tokens are tidied too.
revoke_login_token <- function(email, token) {
  email <- tolower(trimws(email %||% ""))
  users <- .load_users()
  rec <- users[[email]]
  if (is.null(rec)) return(invisible(FALSE))
  h <- if (is.null(token)) "" else hash_code(token)
  rec$sessions <- Filter(function(s) !identical(s$token_hash, h), .live_sessions(rec$sessions))
  users[[email]] <- rec
  .save_users(users)
  invisible(TRUE)
}

# ---------------------------------------------------------------------
# Forgot / reset password
# ---------------------------------------------------------------------
start_password_reset <- function(email) {
  email <- tolower(trimws(email))
  users <- .load_users()
  rec <- users[[email]]
  # Deliberately the SAME message whether or not the account exists, so the
  # form can't be used to find out which UWA emails have signed up.
  generic <- list(ok = TRUE, message = "If that email has an account, a reset code has been sent.")
  if (is.null(rec) || !isTRUE(rec$verified)) return(generic)

  code <- generate_code()
  rec$pending_code_hash <- hash_code(code)
  rec$pending_code_expires <- as.character(Sys.time() + CODE_TTL_MINS * 60)
  rec$pending_purpose <- "reset"
  users[[email]] <- rec
  .save_users(users)
  tryCatch(send_code_email(email, code, "reset"), error = function(e) message("send_code_email failed: ", conditionMessage(e)))
  generic
}

#' Resetting a password via "Forgot password" also resets the account back
#' to role "user" (a security measure the user asked for: a password reset
#' is treated as an event that should require an admin to re-vet elevated
#' access, whether it was a genuine self-service reset or a compromise
#' being recovered from). Skipped for a seed admin - user_role() always
#' overrides their role back to "admin" anyway, so touching the store would
#' have no real effect and $role_was_reset would misleadingly say it did.
reset_password <- function(email, code, new_password) {
  email <- tolower(trimws(email))
  if (is.null(new_password) || nchar(new_password) < 8) {
    return(list(ok = FALSE, message = "Password must be at least 8 characters."))
  }
  users <- .load_users()
  rec <- users[[email]]
  if (is.null(rec) || is.null(rec$pending_code_hash) || !identical(rec$pending_purpose, "reset")) {
    return(list(ok = FALSE, message = "No pending reset for this email."))
  }
  if (Sys.time() > as.POSIXct(rec$pending_code_expires)) {
    return(list(ok = FALSE, message = "That code has expired. Request a new one."))
  }
  if (!identical(hash_code(trimws(code)), rec$pending_code_hash)) {
    return(list(ok = FALSE, message = "Incorrect code."))
  }

  role_before <- user_role(email)
  is_seed <- email %in% ADMIN_SEED_EMAILS

  rec$password_hash <- hash_password(new_password)
  rec$pending_code_hash <- NULL
  rec$pending_code_expires <- NULL
  rec$pending_purpose <- NULL
  rec$failed_login_count <- 0
  rec$sessions <- NULL   # sign out every browser that stayed signed in
  if (!is_seed) rec$role <- "user"
  users[[email]] <- rec
  .save_users(users)

  list(ok = TRUE, message = "Password reset - you can now log in.",
       role_was_reset = !is_seed && !identical(role_before, "user"))
}
