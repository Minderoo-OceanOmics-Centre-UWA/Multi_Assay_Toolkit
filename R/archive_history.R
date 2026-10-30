### R/archive_history.R
#
# Persists generated archive .zips to S3 in two places, so anyone using the
# hosted app can come back to a past run WITHOUT re-running the pipeline
# (shinyapps.io/Posit Connect don't guarantee a running app's local disk
# survives an idle restart, a redeploy, or scaling to a second instance):
#
#   DRAFT   s3://<bucket>/biodiversity-public/Draft/<project>_<YYYYmmdd_HHMMSS>.zip
#           A work-in-progress archive. Every save gets its own timestamped
#           file, so drafts never overwrite each other.
#   PUBLISH s3://<bucket>/biodiversity-public/Publish/<project>_MAT_DwCA.zip
#           The archive that is ready to be sent to OBIS. One canonical file
#           per project - publishing again REPLACES it (the app asks first).
#   REPORT  s3://<bucket>/biodiversity-public/Report/<same base name as the zip>.png
#           The one-page analysis report. Always named after its zip, so it is
#           renamed along with it when a draft is moved to Publish.
#
# A draft is promoted with move_draft_to_publish() (copy to Publish, then
# remove from Draft). The Draft and Publish tabs are LIVE listings of those
# two folders - no separate bookkeeping index that could drift out of sync.
#
# Credentials and bucket name come from environment variables only,
# never hardcoded:
#   AWS_ACCESS_KEY_ID, AWS_SECRET_ACCESS_KEY, AWS_DEFAULT_REGION
#   FAIRE2OBIS_S3_BUCKET  - bucket (defaults to the project's bucket)
#   FAIRE2OBIS_S3_PREFIX  - parent folder holding Draft/ and Publish/
#                           (default "biodiversity-public")
#
# NOTE: Draft/ sits under biodiversity-public/, so whether drafts are
# publicly readable depends on the bucket policy for that prefix.

s3_bucket_name <- function() {
  v <- Sys.getenv("FAIRE2OBIS_S3_BUCKET")
  if (nzchar(v)) v else "minderoo-oceanomics"
}
s3_public_prefix <- function() {
  v <- Sys.getenv("FAIRE2OBIS_S3_PREFIX")
  if (nzchar(v)) v else "biodiversity-public"
}
s3_draft_prefix   <- function() paste0(s3_public_prefix(), "/Draft")
s3_publish_prefix <- function() paste0(s3_public_prefix(), "/Publish")
s3_report_prefix  <- function() paste0(s3_public_prefix(), "/Report")

# A report always has the SAME base name as its zip, in the Report folder:
#   Draft/OcOm_2408_20260925_142530.zip   <->  Report/OcOm_2408_20260925_142530.png
#   Publish/OcOm_2408_MAT_DwCA.zip        <->  Report/OcOm_2408_MAT_DwCA.png
report_key_for_zip <- function(zip_key) {
  paste0(s3_report_prefix(), "/", sub("\\.zip$", "", basename(zip_key)), ".png")
}

safe_project_id <- function(project_id) {
  gsub("[^A-Za-z0-9_-]", "_", if (!is.null(project_id) && nzchar(project_id)) project_id else "untitled")
}
# Published archive name: <project>_MAT_DwCA.zip = project, tool, file format.
# (Before 2026-10-30 this was <project>_CoreVersion.zip; such older files are
# still recognised by project_from_filename() below.)
PUBLISH_SUFFIX <- "_MAT_DwCA"
publish_file_name <- function(project_id) paste0(safe_project_id(project_id), PUBLISH_SUFFIX, ".zip")
publish_zip_key <- function(project_id) {
  paste0(s3_publish_prefix(), "/", publish_file_name(project_id))
}
draft_zip_key <- function(project_id, when = Sys.time()) {
  paste0(s3_draft_prefix(), "/", safe_project_id(project_id), "_", format(when, "%Y%m%d_%H%M%S", tz = "UTC"), ".zip")
}

# "OcOm_2408_20260925_142530.zip" -> "OcOm_2408"; "OcOm_2408_MAT_DwCA.zip" -> "OcOm_2408"
# (and the older "OcOm_2408_CoreVersion.zip" -> "OcOm_2408")
project_from_filename <- function(file_name) {
  sub("(_[0-9]{8}_[0-9]{6}|_MAT_DwCA|_CoreVersion)?\\.zip$", "", file_name)
}

#' TRUE if saving to Draft/Publish is configured (bucket known AND AWS
#' credentials present). The app hides the save options, rather than
#' erroring, when this is FALSE.
archive_history_enabled <- function() {
  nzchar(s3_bucket_name()) &&
    nzchar(Sys.getenv("AWS_ACCESS_KEY_ID")) &&
    nzchar(Sys.getenv("AWS_SECRET_ACCESS_KEY"))
}

# ---- thin S3 wrappers (the only place aws.s3 is called; easy to swap out in tests)
.s3_put <- function(file, key) {
  ok <- aws.s3::put_object(file = file, object = key, bucket = s3_bucket_name())
  if (!isTRUE(ok)) stop("Upload to S3 did not report success.")
  invisible(TRUE)
}
.s3_exists <- function(key) {
  isTRUE(aws.s3::object_exists(object = key, bucket = s3_bucket_name()))
}
.s3_copy <- function(from_key, to_key) {
  b <- s3_bucket_name()
  aws.s3::copy_object(from_object = from_key, to_object = to_key, from_bucket = b, to_bucket = b)
  invisible(TRUE)
}
.s3_delete <- function(key) {
  isTRUE(aws.s3::delete_object(object = key, bucket = s3_bucket_name()))
}
.s3_list <- function(prefix) {
  aws.s3::get_bucket_df(bucket = s3_bucket_name(), prefix = prefix, max = Inf)
}
.s3_get <- function(key, dest) {
  aws.s3::save_object(object = key, bucket = s3_bucket_name(), file = dest)
  invisible(dest)
}

# Uploads the report next to its zip's name. A report problem never fails the
# archive save - the zip is what matters - so it returns NULL instead of stopping.
.put_report_for <- function(report_path, zip_key) {
  if (is.null(report_path) || !file.exists(report_path)) return(NULL)
  key <- report_key_for_zip(zip_key)
  ok <- tryCatch({ .s3_put(report_path, key); TRUE }, error = function(e) FALSE)
  if (ok) key else NULL
}

#' Save a generated archive as a DRAFT - a new timestamped file every time.
#' @param report_path Optional local PNG report, saved to Report/ under the same name
#' @return list(project_id, s3_key, file_name, report_key = NULL if no report was saved)
save_archive_as_draft <- function(zip_path, project_id, report_path = NULL) {
  key <- draft_zip_key(project_id)
  .s3_put(zip_path, key)
  list(project_id = safe_project_id(project_id), s3_key = key, file_name = basename(key),
       report_key = .put_report_for(report_path, key))
}

#' TRUE if a published version of this project already exists.
publish_exists <- function(project_id) .s3_exists(publish_zip_key(project_id))

#' Publish an archive straight to the Publish folder, replacing that
#' project's existing published zip (if any).
#' @return list(project_id, s3_key, file_name, report_key = NULL if no report was saved)
publish_archive <- function(zip_path, project_id, report_path = NULL) {
  key <- publish_zip_key(project_id)
  .s3_put(zip_path, key)
  list(project_id = safe_project_id(project_id), s3_key = key, file_name = basename(key),
       report_key = .put_report_for(report_path, key))
}

#' Promote a draft: copy it to Publish as <project>_MAT_DwCA.zip, and
#' only once that copy is confirmed to exist, remove the draft. Refuses to
#' touch anything outside the Draft prefix.
#' @param draft_key Full S3 key of the draft (from list_drafts())
#' @return list(publish_key, file_name, draft_removed = logical)
move_draft_to_publish <- function(draft_key) {
  prefix <- paste0(s3_draft_prefix(), "/")
  if (!startsWith(draft_key, prefix)) stop("That file isn't in the Draft folder - refusing to move it.")

  file_name <- substring(draft_key, nchar(prefix) + 1)
  if (!grepl("^[A-Za-z0-9_-]+_[0-9]{8}_[0-9]{6}\\.zip$", file_name)) {
    stop("Unexpected Draft file name (expected <project>_<timestamp>.zip): ", draft_key)
  }
  publish_key <- publish_zip_key(project_from_filename(file_name))

  .s3_copy(draft_key, publish_key)
  if (!.s3_exists(publish_key)) stop("Copy to the Publish folder could not be confirmed - the draft has been left where it is.")

  removed <- tryCatch(.s3_delete(draft_key), error = function(e) FALSE)

  # The draft's report follows it, renamed to match the published zip. Like the
  # save, a report problem never undoes the (already confirmed) zip move.
  report_status <- tryCatch({
    from <- report_key_for_zip(draft_key); to <- report_key_for_zip(publish_key)
    if (!.s3_exists(from)) "none" else {
      .s3_copy(from, to)
      if (!.s3_exists(to)) "failed" else if (isTRUE(tryCatch(.s3_delete(from), error = function(e) FALSE))) "moved" else "copied"
    }
  }, error = function(e) "failed")

  list(publish_key = publish_key, file_name = basename(publish_key), draft_removed = isTRUE(removed),
       report_status = report_status)
}

#' Download the report that belongs to a zip (Draft or Publish) into `dest`.
#' @return TRUE if a report existed and was fetched, FALSE if there is none
fetch_report_for_zip <- function(zip_key, dest) {
  key <- report_key_for_zip(zip_key)
  if (!.s3_exists(key)) return(FALSE)
  .s3_get(key, dest)
  TRUE
}

#' List everything under one prefix, newest first, for a table. Folder
#' placeholder entries (zero-byte keys) are dropped.
#' @return data.frame(last_modified, project, path, size_mb, s3_key)
list_s3_folder <- function(prefix_root) {
  empty <- data.frame(
    last_modified = character(), project = character(), path = character(),
    size_mb = character(), s3_key = character(), stringsAsFactors = FALSE
  )
  prefix <- paste0(prefix_root, "/")
  objects <- .s3_list(prefix)
  if (nrow(objects) == 0) return(empty)

  objects$Size <- as.numeric(objects$Size)
  objects <- objects[objects$Size > 0, , drop = FALSE]
  if (nrow(objects) == 0) return(empty)

  rel_path <- substring(objects$Key, nchar(prefix) + 1)
  df <- data.frame(
    last_modified = objects$LastModified,
    project       = project_from_filename(rel_path),
    path          = rel_path,
    size_mb       = sprintf("%.1f MB", objects$Size / 1024^2),
    s3_key        = objects$Key,
    stringsAsFactors = FALSE
  )
  df[order(df$last_modified, decreasing = TRUE), , drop = FALSE]
}
list_drafts    <- function() list_s3_folder(s3_draft_prefix())
list_published <- function() list_s3_folder(s3_publish_prefix())

#' Fetch a saved archive's bytes from S3 into a local file (for a downloadHandler).
fetch_archive_from_s3 <- function(s3_key, dest) .s3_get(s3_key, dest)
