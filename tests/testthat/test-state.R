state_file <- function(drop = character(), env = parent.frame()) {
  p <- withr::local_tempfile(fileext = ".db", .local_envir = env)
  export_db(p, list())
  con <- DBI::dbConnect(RSQLite::SQLite(), p)
  for (t in drop) DBI::dbExecute(con, sprintf("DROP TABLE %s", t))
  DBI::dbDisconnect(con)
  p
}

test_that("read_state gives NULL for a cold start and every table otherwise", {
  expect_null(read_state(NULL))
  s <- read_state(state_file())
  expect_setequal(names(s), c("subjects", "subject_members", "subject_name_basis", "subject_task_views",
                              "collections", "collection_members", "collection_mentions",
                              "seen_packages", "pipeline_runs", "weekly_clusters"))
  expect_equal(nrow(s$subjects), 0L)
  expect_true("left" %in% names(s$subject_members))
  expect_named(s$weekly_clusters, c("package", "cluster"))
})

test_that("read_state reads a state from before weekly_clusters, leaving them NULL", {
  s <- read_state(state_file("weekly_clusters"))
  expect_null(s$weekly_clusters)
  expect_equal(nrow(s$subjects), 0L)
})

test_that("read_state stops on a missing table", {
  p <- state_file("subject_name_basis")
  expect_error(read_state(p), "state")
  expect_error(read_state(p), "subject_name_basis")
})

test_that("read_state stops on a missing or empty file", {
  expect_error(read_state(file.path(withr::local_tempdir(), "none.db")), "state")
  p <- withr::local_tempfile(fileext = ".db")
  file.create(p)
  expect_error(read_state(p), "state")
})

subj <- function(ids, slugs = paste0("s", ids)) data.frame(subject_id = ids, slug = slugs)
coll <- function(ids) data.frame(collection_id = ids)

test_that("check_retention stops when a previous subject_id is absent", {
  prev <- list(subjects = subj(1:3), collections = coll(1:2))
  expect_silent(check_retention(prev, list(subjects = subj(1:4), collections = coll(1:2))))
  expect_error(check_retention(prev, list(subjects = subj(c(1L, 3L)), collections = coll(1:2))),
               "subject_id 2")
  expect_silent(check_retention(NULL, list(subjects = subj(1L), collections = coll(1L))))
})

test_that("check_retention stops when a previous collection_id is absent", {
  prev <- list(subjects = subj(1:2), collections = coll(1:3))
  expect_error(check_retention(prev, list(subjects = subj(1:2), collections = coll(c(1L, 2L)))),
               "collection_id 3")
})

test_that("check_retention stops when a published slug changes but lets a placeholder go", {
  prev <- list(subjects = subj(1:2, c("maps", "held-2")), collections = coll(integer()))
  ok <- list(subjects = subj(1:2, c("maps", "graphs")), collections = coll(integer()))
  expect_silent(check_retention(prev, ok))
  bad <- list(subjects = subj(1:2, c("maps-2", "held-2")), collections = coll(integer()))
  expect_error(check_retention(prev, bad), "slug")
})
