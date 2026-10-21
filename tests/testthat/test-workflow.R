workflow_text <- function() {
  path <- test_path("..", "..", ".github", "workflows", "update.yml")
  paste(readLines(path, warn = FALSE), collapse = "\n")
}

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

test_that("the workflow holds no key and no secret other than the token", {
  txt <- workflow_text()
  expect_false(grepl("ANTHROPIC", txt, fixed = TRUE))
  used <- regmatches(txt, gregexpr("secrets\\.[A-Za-z_]+", txt))[[1]]
  expect_true(all(used == "secrets.GITHUB_TOKEN"))
})
