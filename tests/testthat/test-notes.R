test_that("release notes give the run's figures and assets", {
  r <- cold()
  notes <- release_notes(r$out, updated_at = "2026-10-26 03:41 UTC")
  expect_match(notes, "^Rolling database of subjects and community collections among current CRAN packages\\.")
  expect_match(notes, "**2 subjects** published, covering **60** of", fixed = TRUE)
  expect_match(notes, "- **0** classified by an LLM", fixed = TRUE)
  expect_match(notes, "- **2** named by the open model and waiting for a classification", fixed = TRUE)
  expect_match(notes, "Last run: weekly | updated 2026-10-26 03:41 UTC.", fixed = TRUE)
  expect_match(notes, "Assets: `cran-subjects.db` (SQLite) and `manifest.json`.", fixed = TRUE)
  expect_match(notes, "kept on a `snapshot-YYYY-MM` release", fixed = TRUE)
})

test_that("release notes use the singular for one", {
  expect_equal(count_of(1L, "subject"), "**1 subject**")
  expect_equal(count_of(45L, "collection"), "**45 collections**")
  expect_equal(count_of(25406L, "package"), "**25,406 packages**")
})

test_that("release notes count the published subjects that wait for the next classification", {
  r <- cold()
  expect_match(release_notes(r$out, "x"), "- **0** drifted or too small, waiting for the classification on 2026-11-01", fixed = TRUE)
  con <- DBI::dbConnect(RSQLite::SQLite(), r$db)
  DBI::dbExecute(con, "UPDATE packages SET is_archived = 1 WHERE name LIKE 'pal%' AND name > 'pal09'"); DBI::dbDisconnect(con)
  out2 <- run_update(fake_io(r$db, r$out), withr::local_tempdir(), "weekly", r$vf, st)
  expect_match(release_notes(out2, "x"), "- **1** drifted or too small, waiting for the classification on 2026-11-01", fixed = TRUE)
})
