test_that("the update CLI finds repository files from any working directory", {
  root <- normalizePath(dirname(.script_dir))
  work <- withr::local_tempdir()
  db <- file.path(work, "observatory.db")
  make_fixture_store(db)
  out <- file.path(work, "out")
  res <- withr::with_dir(work, suppressWarnings(system2(
    "Rscript",
    c(shQuote(file.path(root, "scripts", "update.R")), shQuote(out), "--mode", "weekly",
      "--input", shQuote(db), "--previous", "none", "--llama", "false",
      "--model", shQuote(file.path(work, "no-such-model.gguf"))),
    stdout = TRUE, stderr = TRUE
  )))
  expect_null(attr(res, "status"), info = paste(res, collapse = "\n"))
  expect_true(file.exists(file.path(out, "cran-subjects.db")))
})

test_that("repo_path joins onto REPO_ROOT, or the working directory when unset", {
  expect_equal(repo_path("classifications", "subjects.json"), file.path("classifications", "subjects.json"))
  old <- REPO_ROOT
  assign("REPO_ROOT", "/some/root", envir = globalenv())
  withr::defer(assign("REPO_ROOT", old, envir = globalenv()))
  expect_equal(repo_path("classifications", "subjects.json"), "/some/root/classifications/subjects.json")
})
