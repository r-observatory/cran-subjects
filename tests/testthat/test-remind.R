LAST_LINE <- "Run /judge-subjects in cran-subjects, then commit `classifications/subjects.json`."

test_that("the title carries the count", {
  expect_equal(reminder_title(3), "Subjects waiting for a classification: 3")
})

test_that("the body lists each subject then the instruction", {
  w <- data.frame(subject_id = 7L, slug = "graphs", name = "Graph analysis",
                  why = "new name", stringsAsFactors = FALSE)
  expect_equal(reminder_body(w), c("- Graph analysis (7): new name", LAST_LINE))
  w2 <- rbind(w, data.frame(subject_id = 9L, slug = "maps", name = "Maps",
                            why = "drifted", stringsAsFactors = FALSE))
  expect_equal(reminder_body(w2),
               c("- Graph analysis (7): new name", "- Maps (9): drifted", LAST_LINE))
})

test_that("remind.yml has the schedule, narrow permissions and no secret but the token", {
  path <- test_path("..", "..", ".github", "workflows", "remind.yml")
  txt <- paste(readLines(path, warn = FALSE), collapse = "\n")
  expect_true(grepl("0 9 1 * *", txt, fixed = TRUE))
  expect_true(grepl("issues: write", txt, fixed = TRUE))
  expect_true(grepl("contents: read", txt, fixed = TRUE))
  expect_false(grepl("|| true", txt, fixed = TRUE))
  used <- regmatches(txt, gregexpr("secrets\\.[A-Za-z_]+", txt))[[1]]
  expect_true(all(used == "secrets.GITHUB_TOKEN"))
})

test_that("remind_write counts waiting subjects in a database", {
  skip_if_not_installed("RSQLite")
  out <- withr::local_tempdir()
  db <- file.path(out, "s.db")
  con <- DBI::dbConnect(RSQLite::SQLite(), db)
  DBI::dbExecute(con, "CREATE TABLE subjects (subject_id INTEGER, slug TEXT, name TEXT, status TEXT, held_reason TEXT)")
  DBI::dbExecute(con, "CREATE TABLE subject_members (subject_id INTEGER, package TEXT, is_primary INTEGER, \"left\" TEXT)")
  DBI::dbExecute(con, "INSERT INTO subjects VALUES (1, 'a', 'Alpha', 'named', NULL), (2, 'b', 'Beta', 'judged', NULL)")
  DBI::dbDisconnect(con)
  vp <- file.path(out, "v.json")
  writeLines("[]", vp)
  n <- remind_write(db, vp, out, default_settings())
  expect_equal(n, 1L)
  expect_equal(readLines(file.path(out, "remind-count.txt")), "1")
  md <- readLines(file.path(out, "remind.md"))
  expect_equal(md[1], "Subjects waiting for a classification: 1")
  expect_equal(md[2], "")
  expect_equal(md[3], "- Alpha (1): new name")
  expect_equal(md[length(md)], LAST_LINE)
})

test_that("a database with no subjects gives a count of 0", {
  skip_if_not_installed("RSQLite")
  out <- withr::local_tempdir()
  db <- file.path(out, "s.db")
  con <- DBI::dbConnect(RSQLite::SQLite(), db)
  DBI::dbExecute(con, "CREATE TABLE subjects (subject_id INTEGER, slug TEXT, name TEXT, status TEXT, held_reason TEXT)")
  DBI::dbExecute(con, "CREATE TABLE subject_members (subject_id INTEGER, package TEXT, is_primary INTEGER, \"left\" TEXT)")
  DBI::dbDisconnect(con)
  vp <- file.path(out, "v.json")
  writeLines("[]", vp)
  expect_equal(remind_write(db, vp, out, default_settings()), 0L)
  expect_equal(readLines(file.path(out, "remind-count.txt")), "0")
})
