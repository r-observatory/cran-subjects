test_that("clean_name applies sentence case and keeps acronyms and package names", {
  expect_equal(clean_name("Shiny Applications", "shiny"), "Shiny applications")
  expect_equal(clean_name("Code Analysis Tools", character()), "Code analysis")
  expect_equal(clean_name("OMOP Common Data Model Tools", character()), "OMOP common data model")
  expect_equal(clean_name("ggplot2 extensions", "ggplot2"), "ggplot2 extensions")
  expect_equal(clean_name("Data Manipulation with data.table", "data.table"), "Data manipulation with data.table")
  expect_equal(clean_name("C++ Library Interfaces", character()), "C++ library interfaces")
  expect_equal(clean_name("Single-Cell RNA-Seq Analysis", character()), "Single-cell RNA-Seq analysis")
  expect_equal(clean_name("Meta-Analysis Techniques", character()), "Meta-analysis")
  expect_equal(clean_name("\"Survival Analysis.\"", character()), "Survival analysis")
  expect_equal(clean_name("Tools", character()), "")
})
test_that("is_generic_name flags names made only of generic words", {
  expect_true(is_generic_name("Utility functions")); expect_true(is_generic_name("Data analysis"))
  expect_false(is_generic_name("Survival analysis"))
})
test_that("main_words drops generic words and joiners", {
  expect_equal(main_words("Models for the Survival of Data"), "survival")
  expect_equal(main_words("Tools"), character())
})
test_that("near_duplicate_names flags names whose main words nest", {
  expect_equal(near_duplicate_names(c("API interfaces", "API clients", "Survival analysis")), c(TRUE, TRUE, FALSE))
})
test_that("name_prompt follows the trial's form and copes with short clusters and missing descriptions", {
  db <- withr::local_tempfile(fileext = ".db"); make_fixture_store(db); x <- read_inputs(db)
  x$packages$description[x$packages$name == "surv03"] <- NA
  p <- name_prompt(sprintf("surv%02d", 1:5), x, data.frame(from = character(), to = character(), weight = numeric()),
                   phrases = "Survival Analysis", settings = default_settings())
  expect_match(p, "^These 5 R packages were placed together by a clustering of CRAN\\. Here are the 5 most central")
  expect_match(p, "- surv03: Survival Analysis With Method 03\\.\n")
  expect_match(p, "Phrases common in their titles: Survival Analysis\n")
  expect_match(p, "CRAN task views with the most overlap: Survival \\(5 in common\\)\n$")
})
test_that("name_prompt orders members by in-set link weight, then name", {
  db <- withr::local_tempfile(fileext = ".db"); make_fixture_store(db); x <- read_inputs(db)
  e <- data.frame(from = c("surv01", "surv01", "surv02"), to = c("surv02", "surv04", "surv04"), weight = c(1, 1, 5))
  p <- name_prompt(sprintf("surv%02d", 1:5), x, e, phrases = character(), settings = default_settings())
  pos <- vapply(c("surv02", "surv04", "surv01", "surv03", "surv05"), function(n) regexpr(paste0("- ", n, ":"), p), 1L)
  expect_false(is.unsorted(unname(pos)))
  expect_match(p, "Phrases common in their titles: \\(none\\)\n")
})
test_that("title_phrases prefers a longer phrase that covers nearly as many titles", {
  t <- c(setNames(rep("Item Response Theory Models", 8), sprintf("i%d", 1:8)), setNames(sprintf("Other %d", 1:92), sprintf("o%d", 1:92)))
  expect_equal(title_phrases(t, sprintf("i%d", 1:8), title_gram_df(t))[1], "Item Response Theory")
})
test_that("llama_name returns NULL when the binary fails", {
  expect_null(llama_name("s", "u", llama_bin = "false", model_path = "x", schema_path = "y"))
})
