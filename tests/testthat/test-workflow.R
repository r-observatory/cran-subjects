workflow_text <- function(file = "update.yml") {
  path <- test_path("..", "..", ".github", "workflows", file)
  paste(readLines(path, warn = FALSE), collapse = "\n")
}

# The lines of one block, from its opening line up to the next line at the same indent.
yaml_block <- function(txt, opens, indent) {
  lines <- strsplit(txt, "\n", fixed = TRUE)[[1]]
  starts <- grep(sprintf("^%s\\S", strrep(" ", indent)), lines)
  hit <- starts[vapply(starts, opens, NA, lines = lines)]
  if (length(hit) != 1L) return("")
  end <- c(starts[starts > hit] - 1L, length(lines))[1]
  paste(lines[hit:end], collapse = "\n")
}
workflow_step <- function(txt, name) {
  yaml_block(txt, function(i, lines) {
    nxt <- grep("^      - ", lines)
    to <- c(nxt[nxt > i] - 1L, length(lines))[1]
    any(trimws(sub("^      - ", "", lines[i:to])) == paste("name:", name))
  }, 6L)
}
workflow_job <- function(txt, job) yaml_block(txt, function(i, lines) lines[i] == paste0("  ", job, ":"), 2L)

test_that("the workflow has both schedules and the pinned values", {
  txt <- workflow_text()
  for (needle in c("0 3 * * 1", "30 5 * * *", "MODEL_SHA256", "LLAMA_SHA256",
                   "qwen3-8b-q4km-d98cdcbd", "keepalive:", "cran-subjects-update",
                   "cancel-in-progress: false", "timeout-minutes: 330")) {
    expect_true(grepl(needle, txt, fixed = TRUE), info = needle)
  }
})

test_that("the pinned hashes match the config", {
  txt <- workflow_text()
  expect_true(grepl(MODEL_SHA256, txt, fixed = TRUE))
  expect_true(grepl(LLAMA_SHA256, txt, fixed = TRUE))
  expect_true(grepl(LLAMA_ASSET, txt, fixed = TRUE))
  expect_true(grepl(MODEL_URL, txt, fixed = TRUE))
})

test_that("downloads are not softened and the script fetches the previous database", {
  txt <- workflow_text()
  expect_false(grepl("|| true", txt, fixed = TRUE))
  expect_false(grepl("|| echo", txt, fixed = TRUE))
  expect_false(grepl("gh release download[^\n]*cran-subjects\\.db", txt))
})

test_that("naming setup never fails the run, while its hash checks stay strict", {
  txt <- workflow_text()
  for (step in c("Install llama.cpp", "Restore the model", "Download and check the model")) {
    expect_match(workflow_step(txt, step), "continue-on-error: true", fixed = TRUE, info = step)
  }
  llama <- workflow_step(txt, "Install llama.cpp")
  model <- workflow_step(txt, "Download and check the model")
  expect_match(llama, 'echo "$LLAMA_SHA256  $LLAMA_ASSET" | sha256sum -c -', fixed = TRUE)
  expect_match(llama, 'rm -f "$LLAMA_ASSET"', fixed = TRUE)
  expect_match(model, 'echo "$MODEL_SHA256  $MODEL_PATH" | sha256sum -c -', fixed = TRUE)
  expect_match(model, 'rm -f "$MODEL_PATH"', fixed = TRUE)
  expect_match(workflow_step(txt, "Restore the model"), "path: ${{ env.MODEL_PATH }}", fixed = TRUE)
  build <- workflow_step(txt, "Build the subjects database")
  expect_match(build, "Rscript scripts/update.R", fixed = TRUE)
  expect_no_match(build, "continue-on-error", fixed = TRUE)
})

test_that("every job that runs R sets a UTF-8 locale", {
  for (wf in list(c("update.yml", "update"), c("remind.yml", "remind"), c("test.yml", "test"))) {
    job <- workflow_job(workflow_text(wf[1]), wf[2])
    expect_match(job, "\n    env:\n      LANG: C.UTF-8\n", fixed = TRUE, info = wf[1])
  }
})

test_that("the workflow holds no key and no secret other than the token", {
  txt <- workflow_text()
  expect_false(grepl("ANTHROPIC", txt, fixed = TRUE))
  used <- regmatches(txt, gregexpr("secrets\\.[A-Za-z_]+", txt))[[1]]
  expect_true(all(used == "secrets.GITHUB_TOKEN"))
})

test_that("every publish rewrites the release notes from the run", {
  build <- workflow_step(workflow_text(), "Build the subjects database")
  expect_match(build, "Rscript scripts/notes.R out/", fixed = TRUE)
  publish <- workflow_step(workflow_text(), "Publish to the \"current\" release")
  expect_match(publish, "--notes-file out/release-notes.md --latest", fixed = TRUE)
  expect_match(publish, "gh release edit current --notes-file out/release-notes.md", fixed = TRUE)
})

test_that("every publish also keeps the month's snapshot release", {
  snap <- workflow_step(workflow_text(), "Keep the month's snapshot release")
  expect_match(snap, "snapshot-$(date -u +%Y-%m)", fixed = TRUE)
  expect_match(snap, "cp classifications/subjects.json out/classifications.json", fixed = TRUE)
  expect_match(snap, "--latest=false", fixed = TRUE)
  expect_match(snap, "out/cran-subjects.db out/manifest.json out/classifications.json --clobber", fixed = TRUE)
})
