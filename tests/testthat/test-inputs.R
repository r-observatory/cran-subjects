test_that("parse_dep_field drops versions, R and blanks", {
  expect_equal(parse_dep_field("R (>= 3.3), dplyr (>= 1.1.0),\n  ggplot2"), c("dplyr", "ggplot2"))
  expect_equal(parse_dep_field(NA), character())
  expect_equal(parse_dep_field(""), character())
  expect_equal(parse_dep_field("R (>= 4.0)"), character())
})

test_that("read_inputs keeps current CRAN and joins owners and loc_r", {
  db <- withr::local_tempfile(fileext = ".db")
  make_fixture_store(db)
  x <- read_inputs(db)
  expect_false("oldpkg" %in% x$packages$name)
  expect_equal(x$packages$loc_r[x$packages$name == "tidyverse"], 181)
  expect_true(all(c("from", "to", "type") %in% names(x$deps)))
  expect_setequal(x$deps$to[x$deps$from == "tidyverse" & x$deps$type == "imports"],
                  c("cli", "dplyr", "ggplot2", "tidyr"))
  expect_equal(x$owners$owner_type[x$owners$package == "dplyr"], "Organization")
  expect_true("tidyverse" %in% x$topics$topic[x$topics$package == "tidytext"])
})

test_that("read_inputs reads task views and repo links, current members only", {
  db <- withr::local_tempfile(fileext = ".db")
  make_fixture_store(db)
  x <- read_inputs(db)
  expect_true(all(c("view", "package", "core") %in% names(x$task_views)))
  expect_true("surv01" %in% x$task_views$package[x$task_views$view == "Survival"])
  expect_false("surv30" %in% x$task_views$package[x$task_views$view == "Survival"])
  expect_equal(nrow(x$owners[x$owners$package == "tidyverse", ]), 1L)
  expect_equal(x$owners$owner_login[x$owners$package == "foo1"], "fooverse")
})

test_that("fixture sizes follow n_per_subject and extra rows are added", {
  db <- withr::local_tempfile(fileext = ".db")
  extra <- list(packages = data.frame(name = "zzz", version = "1.0", title = "Zzz",
                                      is_archived = 0L))
  make_fixture_store(db, n_per_subject = 5L, extra = extra)
  x <- read_inputs(db)
  expect_equal(sum(grepl("^surv[0-9]+$", x$packages$name)), 5L)
  expect_true("zzz" %in% x$packages$name)
})

test_that("only github.com repos join to owners", {
  db <- withr::local_tempfile(fileext = ".db")
  extra <- list(repo_package_links = data.frame(
    repo_id = "gitlab.com/x/y", package = "glue", origin = "cran"))
  make_fixture_store(db, extra = extra)
  x <- read_inputs(db)
  expect_true(all(startsWith(x$owners$repo_id, "github.com/")))
})
