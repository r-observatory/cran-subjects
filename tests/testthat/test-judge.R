read_lines_json <- function(path) {
  lapply(readLines(path, warn = FALSE), jsonlite::fromJSON, simplifyVector = FALSE)
}
write_lines_json <- function(x, path) {
  writeLines(vapply(x, function(e) as.character(jsonlite::toJSON(e, auto_unbox = TRUE)), ""), path)
}
prepared <- function(z, env = parent.frame()) {
  out <- withr::local_tempdir(.local_envir = env)
  vf <- withr::local_tempfile(fileext = ".json", .local_envir = env)
  writeLines("[]", vf)
  judge_prepare(z$out, z$db, out, vf, settings = st)
  list(z = z, out = out, vf = vf, queue = read_lines_json(file.path(out, "judge-queue.jsonl")))
}
keep_answer <- function(e, ...) c(list(subject_id = e$subject_id, classification = "keep", name = "Survival analysis",
                                       description = "These packages fit survival models."), list(...))

test_that("prepare writes one queue line per waiting subject", {
  p <- prepared(cold())
  expect_length(p$queue, 2L)
  for (e in p$queue) {
    expect_length(e$members, 30L)
    expect_length(e$listed, min(30L, st$name_members))
    expect_match(e$prompt, "^These 30 R packages")
    expect_match(e$prompt, "Current name: ", fixed = TRUE)
    expect_match(e$prompt, "Other groups with close names: ", fixed = TRUE)
    expect_match(e$fingerprint, "^[0-9a-f]{64}$")
  }
  expect_true(all(file.exists(file.path(p$out, c("judge-system.txt", "classification-schema.json", "judge-ids.json")))))
})

test_that("the fingerprint is the hash of the sorted members", {
  p <- prepared(cold())
  e <- p$queue[[1]]
  f <- withr::local_tempfile()
  writeBin(charToRaw(paste(sort(unlist(e$members), method = "radix"), collapse = "\n")), f)
  expect_equal(e$fingerprint, file_sha256(f))
})

test_that("merge writes valid classifications and reports the invalid", {
  p <- prepared(cold())
  a <- list(keep_answer(p$queue[[1]]),
            list(subject_id = p$queue[[2]]$subject_id, classification = "split",
                 groups = list(list(name = "Colour scales", packages = list("zzz")))))
  write_lines_json(a, file.path(p$out, "judge-answers.jsonl"))
  res <- judge_merge(p$out, p$vf, "llm", "2026-11-01")
  v <- jsonlite::fromJSON(p$vf, simplifyVector = FALSE)
  expect_length(v, 1L)
  expect_equal(v[[1]]$model, "llm")
  expect_equal(v[[1]]$judged_at, "2026-11-01")
  expect_equal(v[[1]]$fingerprint, p$queue[[1]]$fingerprint)
  expect_equal(unlist(v[[1]]$members), unlist(p$queue[[1]]$members))
  bad <- read_lines_json(file.path(p$out, "judge-invalid.jsonl"))
  expect_length(bad, 1L)
  expect_match(paste(unlist(bad[[1]]$errors), collapse = " "), "zzz")
  expect_equal(res$written, 1L)
  expect_equal(res$invalid, 1L)
  expect_length(read_classifications(p$vf), 1L)
})

test_that("merge replaces an older answer for the same subject", {
  p <- prepared(cold())
  id <- p$queue[[1]]$subject_id
  write_lines_json(list(keep_answer(p$queue[[1]])), file.path(p$out, "judge-answers.jsonl"))
  judge_merge(p$out, p$vf, "llm", "2026-11-01")
  write_lines_json(list(list(subject_id = id, classification = "hold")), file.path(p$out, "judge-answers.jsonl"))
  judge_merge(p$out, p$vf, "llm", "2026-11-02")
  v <- read_classifications(p$vf)
  expect_length(v, 1L)
  expect_equal(v[[1]]$classification, "hold")
  expect_equal(v[[1]]$judged_at, "2026-11-02")
})

test_that("a later answer for the same subject wins within one merge", {
  p <- prepared(cold())
  id <- p$queue[[1]]$subject_id
  write_lines_json(list(list(subject_id = id, classification = "split", groups = list(list(name = "x", packages = list("zzz")))),
                        list(subject_id = id, classification = "hold")), file.path(p$out, "judge-answers.jsonl"))
  res <- judge_merge(p$out, p$vf, "m", "2026-11-01")
  expect_equal(res$written, 1L)
  expect_equal(res$invalid, 0L)
})

queue_dir <- function(members, ids = 1:9, env = parent.frame()) {
  out <- withr::local_tempdir(.local_envir = env)
  e <- list(subject_id = 1L, slug = "s", name = "Old name", members = members, fingerprint = "f",
            listed = members, prompt = "p")
  writeLines(as.character(jsonlite::toJSON(e, auto_unbox = TRUE)), file.path(out, "judge-queue.jsonl"))
  writeLines(as.character(jsonlite::toJSON(ids)), file.path(out, "judge-ids.json"))
  out
}
merge_one <- function(out, answer, env = parent.frame()) {
  vf <- withr::local_tempfile(fileext = ".json", .local_envir = env)
  writeLines("[]", vf)
  write_lines_json(list(c(list(subject_id = 1L), answer)), file.path(out, "judge-answers.jsonl"))
  res <- judge_merge(out, vf, "m", "2026-11-01")
  list(res = res, vf = vf, invalid = if (file.size(file.path(out, "judge-invalid.jsonl")) > 0)
    read_lines_json(file.path(out, "judge-invalid.jsonl")) else list())
}

test_that("package names with dots merge as valid", {
  m <- c("data.table", "R6", "x.y")
  r <- merge_one(queue_dir(m), list(classification = "split", groups = list(list(name = "Tables", packages = list("data.table")))))
  expect_equal(r$res$written, 1L)
  expect_length(r$invalid, 0L)
})

test_that("packages that were not listed are invalid", {
  out <- queue_dir(c("a", "b", "c"))
  q <- read_lines_json(file.path(out, "judge-queue.jsonl"))[[1]]
  q$members <- list("a", "b", "c", "hidden")
  writeLines(as.character(jsonlite::toJSON(q, auto_unbox = TRUE)), file.path(out, "judge-queue.jsonl"))
  r <- merge_one(out, list(classification = "keep", name = "Things", description = "These packages do things.", strays = list("hidden")))
  expect_equal(r$res$invalid, 1L)
  expect_match(paste(unlist(r$invalid[[1]]$errors), collapse = " "), "hidden")
})

test_that("merge_with must be a live subject in the state", {
  out <- queue_dir(c("a", "b"), ids = c(1L, 2L, 3L))
  expect_equal(merge_one(out, list(classification = "merge", merge_with = 2L))$res$written, 1L)
  r <- merge_one(out, list(classification = "merge", merge_with = 99L))
  expect_equal(r$res$invalid, 1L)
  expect_match(paste(unlist(r$invalid[[1]]$errors), collapse = " "), "99")
})

test_that("site text rules send an answer back", {
  m <- c("a", "b")
  ok <- list(classification = "keep", name = "Things", description = "These packages do things.")
  expect_equal(merge_one(queue_dir(m), ok)$res$written, 1L)
  for (bad in list(modifyList(ok, list(name = "Things; stuff")),
                   modifyList(ok, list(description = "These do things; some more.")),
                   modifyList(ok, list(description = "A set of things.")),
                   modifyList(ok, list(description = "An odd set of things.")))) {
    expect_equal(merge_one(queue_dir(m), bad)$res$invalid, 1L)
  }
  grp <- function(...) list(classification = "split", groups = list(list(name = "Tables", packages = list("a"), ...)))
  expect_equal(merge_one(queue_dir(m), grp(description = "These hold tables."))$res$written, 1L)
  expect_equal(merge_one(queue_dir(m), grp(description = "A table set."))$res$invalid, 1L)
  expect_equal(merge_one(queue_dir(m), grp(description = "Tables; more"))$res$invalid, 1L)
  g <- list(classification = "split", groups = list(list(name = "Tables; more", packages = list("a"))))
  expect_equal(merge_one(queue_dir(m), g)$res$invalid, 1L)
})

test_that("a name opening with A or An sends an answer back", {
  m <- c("a", "b")
  ok <- list(classification = "keep", name = "Analysis of tables", description = "These packages do things.")
  expect_equal(merge_one(queue_dir(m), ok)$res$written, 1L)
  for (nm in c("A table set", "An overview", " an overview")) {
    r <- merge_one(queue_dir(m), modifyList(ok, list(name = nm)))
    expect_equal(r$res$invalid, 1L, info = nm)
    expect_match(paste(unlist(r$invalid[[1]]$errors), collapse = " "), "name must not start with A or An", info = nm)
  }
  g <- list(classification = "split", groups = list(list(name = "An overview", packages = list("a"))))
  expect_equal(merge_one(queue_dir(m), g)$res$invalid, 1L)
})

test_that("an answer whose subject_id is not one value is invalid and the merge goes on", {
  out <- queue_dir(c("a", "b"))
  vf <- withr::local_tempfile(fileext = ".json")
  writeLines("[]", vf)
  writeLines(c('{"subject_id": [1, 2], "classification": "hold"}', '{"subject_id": {"id": 1}, "classification": "hold"}',
               '{"subject_id": [], "classification": "hold"}', '{"subject_id": 1, "classification": "hold"}'),
             file.path(out, "judge-answers.jsonl"))
  res <- judge_merge(out, vf, "m", "2026-11-01")
  expect_equal(res$written, 1L)
  expect_equal(res$invalid, 3L)
  bad <- read_lines_json(file.path(out, "judge-invalid.jsonl"))
  expect_true(all(vapply(bad, function(b) grepl("subject_id", paste(unlist(b$errors), collapse = " ")), NA)))
  expect_equal(read_classifications(vf)[[1]]$classification, "hold")
})

test_that("an answer for a subject outside the queue, or unparsable, is invalid", {
  out <- queue_dir(c("a", "b"))
  vf <- withr::local_tempfile(fileext = ".json")
  writeLines("[]", vf)
  writeLines(c('{"subject_id": 7, "classification": "hold"}', "{broken"), file.path(out, "judge-answers.jsonl"))
  res <- judge_merge(out, vf, "m", "2026-11-01")
  expect_equal(res$written, 0L)
  expect_equal(res$invalid, 2L)
})

test_that("close names are other live subjects sharing a main word", {
  s <- data.frame(subject_id = 1:8,
                  name = c("Bayesian survival", "Survival trees", "Bayesian survival models", "Colour palettes",
                           "Survival forests", "Survival curves", "Survival tests", "Survival data"),
                  status = c("named", "named", "judged", "named", "held", "retired", "named", "named"),
                  stringsAsFactors = FALSE)
  expect_equal(close_subjects(s, 1L), c("3: Bayesian survival models", "2: Survival trees", "5: Survival forests",
                                        "7: Survival tests", "8: Survival data"))
  expect_length(close_subjects(s, 4L), 0L)
})

test_that("judge_prompt appends the current name and the close names", {
  p <- prepared(cold())
  inputs <- read_inputs(p$z$db)
  m <- unlist(p$queue[[1]]$members)
  e <- dependency_edges(inputs$deps, m)
  txt <- judge_prompt(list(subject_id = 1L, name = "Old name"), m, inputs, e, c("2: Foo", "3: Bar"), st)
  expect_match(txt, "Current name: Old name", fixed = TRUE)
  expect_match(txt, "Other groups with close names: 2: Foo, 3: Bar", fixed = TRUE)
  none <- judge_prompt(list(subject_id = 1L, name = "Old name"), m, inputs, e, character(), st)
  expect_match(none, "Other groups with close names: none", fixed = TRUE)
})

test_that("the prompt files follow the rulings", {
  sysf <- readLines(file.path(.judge_dir, "prompts", "judge-system.txt"), warn = FALSE)
  expect_true(any(grepl("^split: the packages cover two or more subjects. Give each subject a name, one plain sentence on what its packages have in common, and the listed packages that belong to it. Leave out packages that fit none.$", sysf)))
  sch <- jsonlite::fromJSON(file.path(.judge_dir, "prompts", "classification-schema.json"), simplifyVector = FALSE)
  expect_equal(sch$required, list("classification"))
  expect_equal(sch$properties$groups$items$properties$description$type, "string")
  expect_equal(sch$properties$groups$items$required, list("name", "packages"))
})
