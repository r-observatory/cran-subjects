# scripts/notes.R: the release notes, written from each run's database.

count_of <- function(n, noun) {
  sprintf("**%s %s%s**", format(n, big.mark = ","), noun, if (n == 1L) "" else "s")
}

release_notes <- function(db_path, updated_at = format(Sys.time(), "%Y-%m-%d %H:%M UTC", tz = "UTC"),
                          settings = default_settings()) {
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path, flags = RSQLite::SQLITE_RO)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  one <- function(sql) DBI::dbGetQuery(con, sql)[[1]]
  by_status <- DBI::dbGetQuery(con, "SELECT status, COUNT(*) AS n FROM subjects GROUP BY status")
  n_status <- function(s) sum(by_status$n[by_status$status == s])
  judged <- n_status("judged")
  named <- n_status("named")
  placed <- one("SELECT COUNT(DISTINCT m.package) FROM subject_members m JOIN subjects s USING (subject_id)
                 WHERE s.status IN ('named', 'judged') AND m.\"left\" IS NULL")
  run <- DBI::dbGetQuery(con, "SELECT run_at, mode, n_packages FROM pipeline_runs ORDER BY rowid DESC LIMIT 1")
  read_if <- function(t) if (DBI::dbExistsTable(con, t)) DBI::dbGetQuery(con, sprintf('SELECT * FROM "%s"', t))
  waiting <- published_waiting(DBI::dbGetQuery(con, "SELECT subject_id, status FROM subjects"),
                               DBI::dbGetQuery(con, "SELECT subject_id, package, is_primary, \"left\" FROM subject_members"),
                               read_if("subject_cluster_basis"), read_if("weekly_clusters"), settings)
  kinds <- DBI::dbGetQuery(con, "SELECT kind, COUNT(*) AS n FROM collections WHERE status = 'active' GROUP BY kind")
  n_kind <- function(k) sum(kinds$n[kinds$kind == k])
  mentions <- one("SELECT COUNT(DISTINCT m.package) FROM collection_mentions m JOIN collections c USING (collection_id)
                   WHERE c.status = 'active'")
  fmt <- function(n) format(n, big.mark = ",")
  paste(c(
    "Rolling database of subjects and community collections among current CRAN packages. Subjects are groups of related packages found from package text, repository topics and dependencies. Collections are sets of packages a community declared, such as the tidyverse.",
    "",
    sprintf("%s published, covering **%s** of **%s** CRAN packages:", count_of(judged + named, "subject"), fmt(placed), fmt(run$n_packages)),
    sprintf("- **%s** classified by an LLM", fmt(judged)),
    sprintf("- **%s** named by the open model and waiting for a classification", fmt(named)),
    sprintf("- **%s** drifted or too small, waiting for the classification on %s", fmt(length(waiting)),
            next_classification_day(run$run_at)),
    sprintf("- **%s** held back", fmt(n_status("held"))),
    "",
    sprintf("%s: %s collection packages and %s organization communities, with %s more packages that mention one.",
            count_of(sum(kinds$n), "collection"), fmt(n_kind("package")), fmt(n_kind("organization")), fmt(mentions)),
    "",
    sprintf("Last run: %s | updated %s. Names come from Qwen3 8B (Apache 2.0), and an LLM classifies every subject each month.", run$mode, updated_at),
    "",
    "Assets: `cran-subjects.db` (SQLite) and `manifest.json`. Each month's last state is kept on a `snapshot-YYYY-MM` release, with a copy of that month's classifications as `classifications.json`."
  ), collapse = "\n")
}

if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) < 1L) stop("usage: Rscript scripts/notes.R OUT_DIR")
  file_arg <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
  script_dir <- if (length(file_arg) == 1L) dirname(normalizePath(file_arg)) else "scripts"
  for (f in c("config.R", "identity.R", "classifications.R")) source(file.path(script_dir, f))
  writeLines(release_notes(file.path(args[1], "cran-subjects.db")), file.path(args[1], "release-notes.md"))
}
