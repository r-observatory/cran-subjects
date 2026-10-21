# Integrity / completeness core for the primary published DB (cran-subjects.db).

# Build a tiny, real DB on disk with the canonical schema (export_db).
build_subjects_db <- function() {
  tmp <- tempfile(fileext = ".db")
  subjects <- data.frame(
    subject_id = 1L, slug = "spatial-data", name = "Spatial data",
    description = "Packages for spatial data.", status = "named",
    held_reason = NA_character_, named_by = "model", named_at = "2026-10-20",
    classification_at = NA_character_, first_seen = "2026-10-20", last_seen = "2026-10-20",
    redirect_to = NA_integer_, size = 25L, stringsAsFactors = FALSE)
  export_db(tmp, list(subjects = subjects))
  tmp
}

test_that("summary_integrity_core reports filename, bytes, sha256, tables, complete", {
  db <- build_subjects_db()
  on.exit(unlink(db))

  core <- summary_integrity_core(db, complete = TRUE)

  expect_equal(core$db_filename, basename(db))
  # db_bytes stays a double so files >= ~2 GiB do not overflow to NA
  expect_type(core$db_bytes, "double")
  expect_equal(core$db_bytes, file.size(db))
  expect_match(core$db_sha256, "^[0-9a-f]{64}$")
  expect_equal(core$tables$subjects, 1L)
  expect_equal(core$tables$subject_members, 0L)
  expect_equal(core$tables$collections, 0L)
  expect_true(core$complete)
})

test_that("summary_integrity_core sha256 matches an independent digest of the bytes", {
  sha256sum_bin <- Sys.which("sha256sum")
  shasum_bin    <- Sys.which("shasum")
  if (!nzchar(sha256sum_bin) && !nzchar(shasum_bin)) {
    skip("neither sha256sum nor shasum is on PATH")
  }

  db <- build_subjects_db()
  on.exit(unlink(db))

  core <- summary_integrity_core(db)

  if (nzchar(sha256sum_bin)) {
    out <- system2(sha256sum_bin, shQuote(db), stdout = TRUE)
  } else {
    out <- system2(shasum_bin, c("-a", "256", shQuote(db)), stdout = TRUE)
  }
  independent <- tolower(sub("\\s.*$", "", out[1]))

  expect_equal(core$db_sha256, independent)
})

test_that("write_manifest emits generated_at plus the integrity core as top-level fields", {
  db <- build_subjects_db()
  on.exit(unlink(db), add = TRUE)
  core <- summary_integrity_core(db, complete = TRUE)

  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp), add = TRUE)

  write_manifest(tmp, core)

  parsed <- jsonlite::fromJSON(tmp)
  expect_true(nzchar(parsed$generated_at))
  expect_equal(parsed$db_filename, basename(db))
  expect_equal(parsed$db_bytes, file.size(db))
  expect_match(parsed$db_sha256, "^[0-9a-f]{64}$")
  expect_equal(parsed$tables$subjects, 1L)
  expect_equal(parsed$tables$seen_packages, 0L)
  expect_true(parsed$complete)
})

test_that("export_db creates every table with its columns", {
  db <- withr::local_tempfile(fileext = ".db")
  export_db(db, list())
  con <- DBI::dbConnect(RSQLite::SQLite(), db); on.exit(DBI::dbDisconnect(con))
  expect_setequal(DBI::dbListTables(con), c("subjects", "subject_members", "subject_name_basis",
    "subject_task_views", "collections", "collection_members", "collection_mentions",
    "seen_packages", "pipeline_runs"))
  expect_true("held_reason" %in% DBI::dbListFields(con, "subjects"))
})

test_that("default_settings carries every constant", {
  s <- default_settings()
  expect_named(s, c("title_weight", "min_df", "max_df", "svd_dim", "knn_k", "knn_min_sim",
    "resolution", "seed", "iterations", "min_subject_size", "min_maintainers",
    "max_maintainer_share", "second_share", "daily_primary_share", "core_min_users",
    "core_large_subject", "core_top_fraction", "match_jaccard", "drift_jaccard",
    "name_cap", "name_members", "loc_r_max", "org_min_packages"))
  expect_equal(s$resolution, LEIDEN_RESOLUTION)
  expect_equal(s$min_df, TFIDF_MIN_DF)
  expect_equal(s$org_min_packages, ORG_MIN_PACKAGES)
})

test_that("export_db rejects a bad subject status", {
  db <- withr::local_tempfile(fileext = ".db")
  bad <- data.frame(subject_id = 1L, slug = "x", status = "bogus", stringsAsFactors = FALSE)
  expect_error(export_db(db, list(subjects = bad)))
})
