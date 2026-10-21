# scripts/helpers.R: integrity helpers and the database export for the cran-subjects pipeline.

#' Compute the lowercase hex SHA-256 of a file's exact on-disk bytes.
#'
#' Uses whatever the runner already provides, in preference order:
#'   1. digest  package        (if installed)
#'   2. openssl package        (if installed)
#'   3. sha256sum (coreutils)  - present on the ubuntu-latest CI runner
#'   4. shasum -a 256 (BSD)    - macOS/local fallback
#' No heavy dependency is declared: on CI (which installs only RSQLite, DBI,
#' jsonlite, testthat, withr) the coreutils `sha256sum` path is used. If a
#' sibling pipeline already declares `digest`, that path wins automatically.
file_sha256 <- function(path) {
  if (requireNamespace("digest", quietly = TRUE)) {
    return(tolower(digest::digest(file = path, algo = "sha256")))
  }
  if (requireNamespace("openssl", quietly = TRUE)) {
    con <- file(path, open = "rb")
    on.exit(close(con), add = TRUE)
    return(tolower(as.character(openssl::sha256(con))))
  }
  sha_tool <- Sys.which("sha256sum")
  if (nzchar(sha_tool)) {
    out <- system2(sha_tool, shQuote(path), stdout = TRUE)
    return(tolower(sub("\\s.*$", "", out[1])))
  }
  shasum_tool <- Sys.which("shasum")
  if (nzchar(shasum_tool)) {
    out <- system2(shasum_tool, c("-a", "256", shQuote(path)), stdout = TRUE)
    return(tolower(sub("\\s.*$", "", out[1])))
  }
  stop("No SHA-256 backend found (need one of: digest, openssl, sha256sum, shasum)")
}

#' Build the integrity / completeness core describing a finalized SQLite file.
#'
#' Returns a named list of TOP-LEVEL manifest fields computed from the exact
#' on-disk bytes of `db_path` (call this only after the file is finalized and
#' its DB connection closed):
#'   * db_filename - basename of the file
#'   * db_bytes    - byte size of the file as a double. Deliberately NOT cast
#'                   to integer: R's integer range is 32-bit and overflows to
#'                   NA (serialized as the string "NA") for files >= ~2 GiB.
#'   * db_sha256   - lowercase hex sha256 of the file's exact bytes
#'   * tables      - named list mapping each user table to its row count
#'   * complete    - passed through by the caller. complete = the DB holds the
#'                   full, non-partial dataset (full-not-partial), NOT freshness:
#'                   freshness is tracked separately via generated_at and the
#'                   db_sha256 fingerprint. A pipeline with a genuine
#'                   partial/bootstrap state would DERIVE this (e.g. remaining ==
#'                   0) instead of hardcoding it; this pipeline always performs a
#'                   full deterministic replay, so the caller passes TRUE.
#' Lets a downstream merge content-verify the asset it pulls and confirm the
#' expected tables/rows are present.
summary_integrity_core <- function(db_path, complete = TRUE) {
  stopifnot(file.exists(db_path))

  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  tables <- tryCatch({
    tbl_names <- DBI::dbGetQuery(con, "
      SELECT name FROM sqlite_master
       WHERE type = 'table' AND name NOT LIKE 'sqlite_%'
       ORDER BY name")$name

    stats::setNames(
      lapply(tbl_names, function(t) {
        DBI::dbGetQuery(con, sprintf('SELECT count(*) AS n FROM "%s"', t))$n
      }),
      tbl_names
    )
  }, finally = DBI::dbDisconnect(con))

  # db_bytes/db_sha256 read the raw on-disk file only after the connection
  # above is closed, so no open handle or journal file skews the hash/size.
  list(
    db_filename = basename(db_path),
    db_bytes    = file.size(db_path),
    db_sha256   = file_sha256(db_path),
    tables      = tables,
    complete    = complete
  )
}

#' Write the release manifest.json describing the finalized primary DB.
#'
#' Top-level fields: generated_at plus the integrity/completeness core produced
#' by summary_integrity_core(). `core` is merged as TOP-LEVEL fields (not nested)
#' so a downstream merge can read db_filename/db_bytes/db_sha256/tables/complete
#' directly. generated_at records freshness independently of `complete`.
write_manifest <- function(path, core,
                           generated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ",
                                                 tz = "UTC")) {
  obj <- c(list(generated_at = generated_at), core)
  json <- jsonlite::toJSON(obj, auto_unbox = TRUE, pretty = TRUE, null = "null")
  writeLines(json, path)
  invisible(path)
}

#' Write the pipeline tables to a fresh SQLite database.
#' `tables` is a named list of data frames, keyed by table name.
export_db <- function(path, tables) {
  if (file.exists(path)) unlink(path)
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  ok <- FALSE
  on.exit(if (!ok) DBI::dbDisconnect(con), add = TRUE)

  ddl <- c(
    "CREATE TABLE subjects (
       subject_id  INTEGER PRIMARY KEY,
       slug        TEXT UNIQUE NOT NULL,
       name        TEXT,
       description TEXT,
       status      TEXT CHECK (status IN ('held','named','judged','retired')),
       held_reason TEXT,
       named_by    TEXT,
       named_at    TEXT,
       classification_at  TEXT,
       first_seen  TEXT,
       last_seen   TEXT,
       redirect_to INTEGER,
       size        INTEGER
     )",
    "CREATE TABLE subject_members (
       subject_id   INTEGER,
       package      TEXT,
       is_core      INTEGER,
       is_primary   INTEGER,
       reason_kind  TEXT,
       reason_value TEXT,
       joined       TEXT,
       left         TEXT,
       PRIMARY KEY (subject_id, package)
     )",
    "CREATE TABLE subject_name_basis (
       subject_id INTEGER,
       package    TEXT,
       PRIMARY KEY (subject_id, package)
     )",
    "CREATE TABLE subject_task_views (
       subject_id INTEGER,
       view       TEXT,
       in_common  INTEGER
     )",
    "CREATE TABLE collections (
       collection_id INTEGER PRIMARY KEY,
       slug          TEXT UNIQUE,
       kind          TEXT CHECK (kind IN ('package','organization')),
       package       TEXT,
       github_owner  TEXT,
       title         TEXT,
       description   TEXT,
       maintainer    TEXT,
       first_seen    TEXT,
       last_seen     TEXT,
       status        TEXT CHECK (status IN ('active','gone'))
     )",
    "CREATE TABLE collection_members (
       collection_id INTEGER,
       package       TEXT,
       tier          TEXT CHECK (tier IN ('installed','organization')),
       attached      INTEGER,
       PRIMARY KEY (collection_id, package)
     )",
    "CREATE TABLE collection_mentions (
       collection_id INTEGER,
       package       TEXT,
       source        TEXT CHECK (source IN ('description','topic')),
       PRIMARY KEY (collection_id, package, source)
     )",
    "CREATE TABLE seen_packages (
       package    TEXT PRIMARY KEY,
       first_seen TEXT
     )",
    "CREATE TABLE pipeline_runs (
       run_at               TEXT,
       mode                 TEXT,
       n_packages           INTEGER,
       n_subjects_published INTEGER,
       n_held               INTEGER,
       n_named_calls        INTEGER,
       n_collections        INTEGER
     )"
  )
  for (s in ddl) DBI::dbExecute(con, s)

  for (nm in names(tables)) {
    df <- tables[[nm]]
    if (!is.null(df) && nrow(df) > 0L) DBI::dbWriteTable(con, nm, df, append = TRUE)
  }
  # Close before any hashing so no handle skews the bytes.
  ok <- TRUE
  DBI::dbDisconnect(con)
  invisible(path)
}
