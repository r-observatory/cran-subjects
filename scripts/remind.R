# scripts/remind.R: the monthly issue listing subjects that wait for a classification.

reminder_title <- function(n) sprintf("Subjects waiting for a classification: %d", as.integer(n))

reminder_body <- function(waiting) {
  items <- if (nrow(waiting)) sprintf("- %s (%d): %s", waiting$name, waiting$subject_id, waiting$why) else character()
  c(items, "Run /judge-subjects in cran-subjects, then commit `classifications/subjects.json`.")
}

# Writes remind-count.txt and remind.md in out_dir; returns the count.
remind_write <- function(db_path, classifications_path, out_dir, settings) {
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path, flags = RSQLite::SQLITE_RO)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  subjects <- DBI::dbGetQuery(con, "SELECT * FROM subjects")
  members <- DBI::dbGetQuery(con, "SELECT * FROM subject_members")
  basis <- DBI::dbGetQuery(con, "SELECT * FROM subject_name_basis")
  clusters <- if (DBI::dbExistsTable(con, "weekly_clusters")) DBI::dbGetQuery(con, "SELECT * FROM weekly_clusters")
  waiting <- waiting_subjects(subjects, members, basis, read_classifications(classifications_path), settings, clusters)
  n <- nrow(waiting)
  writeLines(as.character(n), file.path(out_dir, "remind-count.txt"))
  writeLines(c(reminder_title(n), "", reminder_body(waiting)), file.path(out_dir, "remind.md"))
  as.integer(n)
}

if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) != 1L) stop("usage: Rscript scripts/remind.R OUT_DIR", call. = FALSE)
  out_dir <- args[[1]]
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  file_arg <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
  script_dir <- if (length(file_arg) == 1L) dirname(normalizePath(file_arg)) else "scripts"
  for (f in c("config.R", "identity.R", "classifications.R")) source(file.path(script_dir, f))
  REPO_ROOT <- dirname(normalizePath(script_dir))
  status <- system2("gh", c("release", "download", "current", "--repo", "r-observatory/cran-subjects",
                            "--pattern", "cran-subjects.db", "--dir", out_dir, "--clobber"))
  if (status != 0L) stop("could not download cran-subjects.db from the current release", call. = FALSE)
  n <- remind_write(file.path(out_dir, "cran-subjects.db"), repo_path(CLASSIFICATIONS_PATH), out_dir, default_settings())
  message("subjects waiting: ", n)
}
