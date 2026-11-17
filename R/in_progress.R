### R/in_progress.R
#
# "In progress": an UNFINISHED wizard session saved part-way, so the user (or
# a teammate - anyone signed in) can resume it later. Different from a Draft,
# which is a FINISHED archive waiting to be published.
#
# Stored on the SERVER'S OWN DISK, not in S3, so unpublished data stays private
# whatever the bucket policy is:
#
#   <progress dir>/<project>.zip    the session bundle
#   <progress dir>/<project>.json   small summary for the list
#
# <progress dir> = FAIRE2OBIS_PROGRESS_DIR (e.g. /srv/mat-data/in-progress on
# the Nectar server - outside the app folder, so it is never served to the web
# and `git pull` / `git reset` never touch it). If unset, a per-user folder on
# the machine running the app (tools::R_user_dir) is used.
#
# One per project: saving again replaces it. It is deleted automatically when
# that project is saved as a Draft or published (see app.R).
#
# The bundle holds the uploaded FAIRe workbooks (files/) and state.rds: the
# app's state (validation, build, WoRMS results and name choices, QC,
# metadata) plus the values typed into the form, so resuming needs no re-run.

progress_dir <- function() {
  d <- Sys.getenv("FAIRE2OBIS_PROGRESS_DIR")
  if (!nzchar(d)) d <- file.path(tools::R_user_dir("MAT", which = "data"), "in-progress")
  d
}
#' TRUE if the progress folder exists (or can be created) and is writable.
progress_storage_ok <- function() {
  d <- progress_dir()
  if (!dir.exists(d)) suppressWarnings(dir.create(d, recursive = TRUE, mode = "0700"))
  dir.exists(d) && file.access(d, 2) == 0
}
progress_zip_path  <- function(project_id) file.path(progress_dir(), paste0(safe_project_id(project_id), ".zip"))
progress_meta_path <- function(project_id) file.path(progress_dir(), paste0(safe_project_id(project_id), ".json"))

# Write via a temporary file in the same folder, then rename, so a half-written
# file is never left behind if saving fails part-way.
.progress_write <- function(from, to) {
  tmp <- paste0(to, ".tmp-", Sys.getpid())
  if (!file.copy(from, tmp, overwrite = TRUE)) stop("Couldn't write to ", dirname(to))
  if (!file.rename(tmp, to)) { unlink(tmp); stop("Couldn't write to ", dirname(to)) }
  invisible(TRUE)
}

#' Save a session bundle (replaces any earlier one for the same project).
#' @param state list from the app (rv values + form values)
#' @param files named list assay -> local path of its FAIRe workbook
#' @param meta  list(project, saved_by, step, step_name, assays)
save_progress <- function(project_id, state, files, meta) {
  if (!progress_storage_ok()) stop("The In progress folder (", progress_dir(), ") doesn't exist or isn't writable.")
  dir <- tempfile("mat_progress_")
  dir.create(file.path(dir, "files"), recursive = TRUE)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  # Files are stored by position, so assay names with odd characters are safe.
  file_index <- data.frame(assay = names(files), stored = sprintf("files/%02d.xlsx", seq_along(files)),
                           stringsAsFactors = FALSE)
  for (i in seq_along(files)) {
    if (!file.copy(files[[i]], file.path(dir, file_index$stored[i]))) stop("Couldn't read the FAIRe file for ", names(files)[i])
  }
  state$file_index <- file_index
  saveRDS(state, file.path(dir, "state.rds"), compress = "xz")

  zip_path <- tempfile(fileext = ".zip")
  on.exit(unlink(zip_path), add = TRUE)
  zip::zip(zip_path, files = c("state.rds", file_index$stored), root = dir)

  meta$saved_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  meta_path <- tempfile(fileext = ".json")
  on.exit(unlink(meta_path), add = TRUE)
  writeLines(jsonlite::toJSON(meta, auto_unbox = TRUE, pretty = TRUE), meta_path)

  .progress_write(zip_path, progress_zip_path(project_id))
  .progress_write(meta_path, progress_meta_path(project_id))
  invisible(meta)
}

#' Summary of the saved session for a project, or NULL if there is none.
progress_meta <- function(project_id) {
  p <- progress_meta_path(project_id)
  if (!file.exists(p) || !file.exists(progress_zip_path(project_id))) return(NULL)
  jsonlite::fromJSON(p)
}

#' Every saved session, newest first (one summary per project).
list_progress <- function() {
  empty <- data.frame(project = character(), saved_by = character(), saved_at = character(),
                      step = integer(), step_name = character(), assays = character(), stringsAsFactors = FALSE)
  if (!dir.exists(progress_dir())) return(empty)
  metas <- list.files(progress_dir(), pattern = "\\.json$", full.names = TRUE)
  rows <- lapply(metas, function(p) {
    m <- tryCatch(jsonlite::fromJSON(p), error = function(e) NULL)
    if (is.null(m) || !file.exists(sub("\\.json$", ".zip", p))) return(NULL)
    data.frame(project = m$project %||% "", saved_by = m$saved_by %||% "", saved_at = m$saved_at %||% "",
               step = as.integer(m$step %||% NA), step_name = m$step_name %||% "",
               assays = paste(m$assays %||% character(), collapse = ", "), stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, Filter(Negate(is.null), rows))
  if (is.null(out)) return(empty)
  out[order(out$saved_at, decreasing = TRUE), , drop = FALSE]
}

#' Unpack a saved session into a working folder for this browser session.
#' @return list(state, files = named list assay -> local path)
load_progress <- function(project_id, dest_dir = tempfile("mat_resume_")) {
  zip_path <- progress_zip_path(project_id)
  if (!file.exists(zip_path)) stop("No saved progress for ", safe_project_id(project_id), ".")
  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
  zip::unzip(zip_path, exdir = dest_dir)
  state <- readRDS(file.path(dest_dir, "state.rds"))
  idx <- state$file_index
  files <- setNames(as.list(file.path(dest_dir, idx$stored)), idx$assay)
  list(state = state, files = files)
}

#' Delete a project's saved session (both files). Returns TRUE if anything was there.
delete_progress <- function(project_id) {
  paths <- c(progress_zip_path(project_id), progress_meta_path(project_id))
  had <- any(file.exists(paths))
  unlink(paths)
  invisible(had)
}

#' What already exists for a project ID: a saved session, drafts, a published archive.
project_status <- function(project_id) {
  pid <- safe_project_id(project_id)
  drafts <- if (archive_history_enabled()) tryCatch({
    d <- list_drafts(); d[d$project == pid, , drop = FALSE]
  }, error = function(e) NULL) else NULL
  list(
    progress  = tryCatch(progress_meta(project_id), error = function(e) NULL),
    n_drafts  = if (is.null(drafts)) 0L else nrow(drafts),
    published = if (archive_history_enabled()) tryCatch(publish_exists(project_id), error = function(e) FALSE) else FALSE
  )
}
