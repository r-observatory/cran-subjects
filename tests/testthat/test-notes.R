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
