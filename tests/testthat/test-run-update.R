test_that("a cold weekly run publishes two named subjects with clean names and slugs", {
  r <- cold(); s <- q(r$out, "SELECT * FROM subjects")
  expect_setequal(s$slug, c("survival-analysis", "colour-palettes"))
  expect_true(all(s$status == "named"))
  expect_equal(nrow(q(r$out, "SELECT * FROM pipeline_runs")), 1L)
})
test_that("a second weekly run keeps ids and slugs", {
  r <- cold(); a <- q(r$out, "SELECT subject_id, slug FROM subjects")
  out2 <- run_update(fake_io(r$db, r$out), withr::local_tempdir(), "weekly", r$vf, st)
  b <- q(out2, "SELECT subject_id, slug FROM subjects")
  expect_equal(b[order(b$slug), ], a[order(a$slug), ], ignore_attr = TRUE)
})
test_that("a hold classification hides a subject on the next run", {
  r <- cold(); m <- q(r$out, "SELECT m.package FROM subject_members m JOIN subjects s USING (subject_id) WHERE s.slug = 'colour-palettes' AND m.is_primary = 1")$package
  id <- q(r$out, "SELECT subject_id FROM subjects WHERE slug = 'colour-palettes'")$subject_id
  jsonlite::write_json(list(list(subject_id = id, members = sort(m), fingerprint = "x", classification = "hold",
    judged_at = "2026-11-01", model = "llm")), r$vf, auto_unbox = TRUE)
  out2 <- run_update(fake_io(r$db, r$out), withr::local_tempdir(), "weekly", r$vf, st)
  s <- q(out2, "SELECT status, held_reason FROM subjects WHERE slug = 'colour-palettes'")
  expect_equal(s$status, "held"); expect_equal(s$held_reason, "classification")
})
test_that("the naming cap holds the rest as name_missing", {
  db <- withr::local_tempfile(fileext = ".db"); make_fixture_store(db)
  vf <- withr::local_tempfile(fileext = ".json"); writeLines("[]", vf)
  out <- run_update(fake_io(db, NULL), withr::local_tempdir(), "weekly", vf, modifyList(st, list(name_cap = 1L)))
  s <- q(out, "SELECT status, held_reason FROM subjects")
  expect_equal(sort(s$status), c("held", "named")); expect_equal(s$held_reason[s$status == "held"], "name_missing")
})
test_that("a daily run places a new package and marks a departed one", {
  r <- cold()
  con <- DBI::dbConnect(RSQLite::SQLite(), r$db)
  DBI::dbExecute(con, "INSERT INTO packages (name, title, description, maintainer, imports, version, is_archived)
    VALUES ('surv31', 'Survival Analysis With Method 31', 'Hazard models for censored data with Kaplan-Meier curves.', 'Surv Author 1', 'surv01', '1.0', 0)")
  DBI::dbExecute(con, "UPDATE packages SET is_archived = 1 WHERE name = 'pal30'"); DBI::dbDisconnect(con)
  out2 <- run_update(fake_io(r$db, r$out), withr::local_tempdir(), "daily", r$vf, st)
  m <- q(out2, "SELECT s.slug, m.package, m.joined, m.left FROM subject_members m JOIN subjects s USING (subject_id)")
  expect_equal(m$slug[m$package == "surv31"], "survival-analysis")
  expect_equal(m$joined[m$package == "surv31"], "2026-10-26")
  expect_equal(m$left[m$package == "pal30"], "2026-10-26")
})
test_that("a failed previous download stops the run", {
  io <- fake_io("x", NULL); io$fetch_previous <- function(dir) stop("download failed")
  expect_error(run_update(io, withr::local_tempdir(), "weekly", tempfile(), st), "download failed")
})

# Helpers for the tests below.
subject_id_of <- function(db, slug) q(db, sprintf("SELECT subject_id FROM subjects WHERE slug = '%s'", slug))$subject_id
primary_of <- function(db, id) {
  sort(q(db, sprintf("SELECT package FROM subject_members WHERE subject_id = %d AND is_primary = 1", id))$package)
}
write_classification <- function(path, ...) {
  jsonlite::write_json(list(list(fingerprint = "x", judged_at = "2026-11-01", model = "llm", ...)),
                       path, auto_unbox = TRUE)
}

test_that("a cold run writes the manifest and records the run", {
  r <- cold()
  man <- jsonlite::fromJSON(file.path(dirname(r$out), "manifest.json"))
  expect_equal(man$db_filename, "cran-subjects.db")
  expect_equal(man$db_sha256, file_sha256(r$out))
  expect_equal(man$tables$subjects, 2L)
  run <- q(r$out, "SELECT * FROM pipeline_runs")
  expect_equal(run$mode, "weekly"); expect_equal(run$n_subjects_published, 2L); expect_equal(run$n_named_calls, 2L)
  expect_equal(q(r$out, "SELECT size FROM subjects")$size, c(30L, 30L))
  expect_equal(nrow(q(r$out, "SELECT * FROM seen_packages")), 83L)
})

test_that("a keep classification publishes the LLM's name as judged and keeps the slug", {
  r <- cold(); id <- subject_id_of(r$out, "colour-palettes")
  write_classification(r$vf, subject_id = id, members = primary_of(r$out, id), classification = "keep",
                name = "Colour scales", description = "These packages give colour scales for plots.")
  out2 <- run_update(fake_io(r$db, r$out), withr::local_tempdir(), "weekly", r$vf, st)
  s <- q(out2, sprintf("SELECT * FROM subjects WHERE subject_id = %d", id))
  expect_equal(s$status, "judged"); expect_equal(s$name, "Colour scales"); expect_equal(s$slug, "colour-palettes")
  expect_equal(s$named_by, "llm"); expect_equal(s$classification_at, "2026-11-01")
})

test_that("a merge classification keeps the target's id and retires the source with a redirect", {
  r <- cold(); pal <- subject_id_of(r$out, "colour-palettes"); surv <- subject_id_of(r$out, "survival-analysis")
  write_classification(r$vf, subject_id = pal, members = primary_of(r$out, pal), classification = "merge", merge_with = surv)
  # The union overlaps the target by half, below this match threshold, so only the merge keeps the id.
  out2 <- run_update(fake_io(r$db, r$out), withr::local_tempdir(), "weekly", r$vf,
                     modifyList(st, list(match_jaccard = 0.6)))
  s <- q(out2, "SELECT subject_id, slug, status, redirect_to, size FROM subjects ORDER BY subject_id")
  expect_equal(s$slug[s$subject_id == surv], "survival-analysis")
  expect_equal(s$size[s$subject_id == surv], 60L)
  expect_equal(s$status[s$subject_id == pal], "retired")
  expect_equal(s$redirect_to[s$subject_id == pal], surv)
  expect_equal(s$size[s$subject_id == pal], 0L)
  left <- q(out2, sprintf("SELECT left FROM subject_members WHERE subject_id = %d", pal))$left
  expect_equal(unique(left), "2026-10-26")
})

test_that("a split classification retires the old id with a redirect to the largest part", {
  r <- cold(); surv <- subject_id_of(r$out, "survival-analysis"); m <- primary_of(r$out, surv)
  write_classification(r$vf, subject_id = surv, members = m, classification = "split",
                groups = list(list(name = "Survival models", packages = m[1:18],
                                   description = "These packages fit survival models."),
                              list(name = "Hazard curves", packages = m[19:30])))
  io <- fake_io(r$db, r$out)
  io$name_cluster <- function(system, user) {
    nm <- if (grepl("pal[0-9]", user)) "Colour Palettes" else "Tidy collections"
    list(name = nm, description = "These packages share one subject.")
  }
  out2 <- run_update(io, withr::local_tempdir(), "weekly", r$vf, modifyList(st, list(min_subject_size = 12L)))
  s <- q(out2, "SELECT * FROM subjects")
  big <- s[s$slug == "survival-models", ]; small <- s[s$slug == "hazard-curves", ]
  expect_equal(c(big$status, small$status), c("judged", "judged"))
  expect_equal(c(big$size, small$size), c(18L, 12L))
  expect_true(all(c(big$subject_id, small$subject_id) > max(c(surv, subject_id_of(r$out, "colour-palettes")))))
  expect_equal(big$description, "These packages fit survival models."); expect_true(is.na(small$description))
  old <- s[s$subject_id == surv, ]
  expect_equal(old$status, "retired"); expect_equal(old$redirect_to, big$subject_id)
  expect_equal(old$slug, "survival-analysis")
})

test_that("a cluster failing the maintainer check is held and never sent for naming", {
  db <- withr::local_tempfile(fileext = ".db"); make_fixture_store(db)
  con <- DBI::dbConnect(RSQLite::SQLite(), db)
  DBI::dbExecute(con, "UPDATE packages SET maintainer = 'One Person' WHERE name LIKE 'pal%'"); DBI::dbDisconnect(con)
  vf <- withr::local_tempfile(fileext = ".json"); writeLines("[]", vf)
  out <- run_update(fake_io(db, NULL), withr::local_tempdir(), "weekly", vf, st)
  s <- q(out, "SELECT subject_id, slug, status, held_reason FROM subjects ORDER BY status")
  expect_equal(s$status, c("held", "named")); expect_equal(s$held_reason[1], "maintainers")
  expect_equal(s$slug[1], paste0("held-", s$subject_id[1]))
  expect_equal(q(out, "SELECT n_named_calls FROM pipeline_runs")$n_named_calls, 1L)
})

test_that("a daily run needs a previous state and carries first_seen days", {
  db <- withr::local_tempfile(fileext = ".db"); make_fixture_store(db)
  expect_error(run_update(fake_io(db, NULL), withr::local_tempdir(), "daily", tempfile(), st), "weekly")
  r <- cold()
  con <- DBI::dbConnect(RSQLite::SQLite(), r$db)
  DBI::dbExecute(con, "INSERT INTO packages (name, title, description, maintainer, imports, version, is_archived)
    VALUES ('surv31', 'Survival Analysis With Method 31', 'Hazard models for censored data with Kaplan-Meier curves.', 'Surv Author 1', 'surv01', '1.0', 0)")
  DBI::dbExecute(con, "UPDATE packages SET is_archived = 1 WHERE name = 'pal30'"); DBI::dbDisconnect(con)
  io <- fake_io(r$db, r$out); io$today <- function() "2026-10-27"
  out2 <- run_update(io, withr::local_tempdir(), "daily", r$vf, st)
  seen <- q(out2, "SELECT * FROM seen_packages")
  expect_equal(seen$first_seen[seen$package == "surv31"], "2026-10-27")
  expect_equal(seen$first_seen[seen$package == "surv01"], "2026-10-26")
  expect_false("pal30" %in% seen$package)
  s <- q(out2, "SELECT slug, size FROM subjects ORDER BY slug")
  expect_equal(s$size, c(29L, 31L))
  expect_equal(q(out2, "SELECT mode FROM pipeline_runs")$mode, c("weekly", "daily"))
  r31 <- q(out2, "SELECT is_primary, reason_kind, reason_value FROM subject_members WHERE package = 'surv31'")
  expect_equal(r31$is_primary, 1L); expect_equal(r31$reason_kind, "uses"); expect_equal(r31$reason_value, "surv01")
})

test_that("name_groups holds generic and duplicate names", {
  db <- withr::local_tempfile(fileext = ".db"); make_fixture_store(db); x <- read_inputs(db)
  groups <- list(empty_group("c1", sprintf("surv%02d", 1:30)), empty_group("c2", sprintf("pal%02d", 1:30)))
  ids <- c(c1 = 1L, c2 = 2L)
  naming <- function(surv, pal) {
    io <- list(name_cluster = function(system, user) {
      list(name = if (grepl("surv", user)) surv else pal, description = "These packages share one subject.")
    })
    name_groups(groups, ids, typed(NULL, "subjects"), list(), list(character(), character()), x,
                empty_edges(), io, st, "2026-10-26")
  }
  g <- naming("Survival Analysis", "Utility Functions")$groups
  expect_equal(g[[1]]$status, "named"); expect_equal(c(g[[2]]$status, g[[2]]$held_reason), c("held", "name_generic"))
  g <- naming("Survival Analysis", "Survival Models")$groups
  expect_equal(vapply(g, `[[`, "", "held_reason"), c("name_duplicate", "name_duplicate"))
})

test_that("name_groups keeps a drifted subject's name when the cap is reached", {
  db <- withr::local_tempfile(fileext = ".db"); make_fixture_store(db); x <- read_inputs(db)
  groups <- list(empty_group("c1", sprintf("surv%02d", 1:30)), empty_group("c2", sprintf("pal%02d", 1:30)))
  ps <- typed(data.frame(subject_id = 1L, slug = "colour-palettes", name = "Colour palettes", status = "named",
                         named_by = MODEL_SHA256, named_at = "2026-10-01", first_seen = "2026-10-01"), "subjects")
  io <- list(name_cluster = function(system, user) stop("no call expected"))
  out <- name_groups(groups, c(c1 = 2L, c2 = 1L), ps, list(`1` = sprintf("pal%02d", 1:10)),
                     list(character(), character()), x, empty_edges(), io,
                     modifyList(st, list(name_cap = 0L)), "2026-10-26")
  expect_equal(out$calls, 0L)
  expect_equal(c(out$groups[[1]]$status, out$groups[[1]]$held_reason), c("held", "name_missing"))
  expect_equal(c(out$groups[[2]]$status, out$groups[[2]]$name), c("named", "Colour palettes"))
  expect_length(out$basis, 0L)
})

test_that("site_sentence drops sentences that break the site's text rules", {
  expect_equal(site_sentence(" These packages fit  models. "), "These packages fit models.")
  expect_true(is.na(site_sentence("A set of packages for models.")))
  expect_true(is.na(site_sentence("These fit models. An extra one follows.")))
  expect_true(is.na(site_sentence("These fit models; they plot too.")))
  expect_true(is.na(site_sentence(NULL)))
})

test_that("parse_cli reads the flags and rejects unknown ones", {
  a <- parse_cli(c("out/", "--mode", "daily", "--previous", "none", "--gpu"))
  expect_equal(a$out_dir, "out/"); expect_equal(a$mode, "daily"); expect_equal(a$previous, "none")
  expect_true(a$gpu); expect_null(a$input); expect_equal(a$llama, "llama-cli")
  expect_error(parse_cli(c("out/", "--bootstrap")), "usage")
  expect_error(parse_cli(c("out/", "--mode", "monthly")), "usage")
})

test_that("default_io reads the previous state fail-closed", {
  dir <- withr::local_tempdir()
  fake_gh <- function(view_status, view_out = "", dl_status = 0L, bytes = 10L) function(args) {
    if (args[2] == "view") return(list(status = view_status, output = view_out))
    if (dl_status == 0L) writeBin(as.raw(seq_len(bytes) %% 256), file.path(gsub("^'|'$", "", args[which(args == "--dir") + 1L]), "cran-subjects.db"))
    list(status = dl_status, output = if (dl_status == 0L) "" else "HTTP 502")
  }
  prev <- function(gh) default_io(list(), gh)$fetch_previous(dir)
  expect_null(prev(fake_gh(1L, "release not found")))
  expect_error(prev(fake_gh(1L, "HTTP 401: Bad credentials")), "look up")
  expect_error(prev(fake_gh(0L, dl_status = 1L)), "could not download")
  expect_error(prev(fake_gh(0L, bytes = 0L)), "empty")
  expect_equal(prev(fake_gh(0L)), file.path(dir, "cran-subjects.db"))
  expect_null(default_io(list(previous = "none"))$fetch_previous(dir))
  expect_error(default_io(list(previous = file.path(dir, "gone.db")))$fetch_previous(dir), "previous state")
  expect_equal(default_io(list(input = file.path(dir, "cran-subjects.db")))$fetch_input(dir),
               file.path(dir, "cran-subjects.db"))
})
