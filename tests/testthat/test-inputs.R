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
  expect_setequal(x$deps$to[x$deps$from == "tidyverse" & x$deps$type == "Imports"],
                  c("cli", "dplyr", "ggplot2", "tidyr"))
  expect_equal(x$owners$owner_type[x$owners$package == "dplyr"], "Organization")
  expect_true("tidyverse" %in% x$topics$topic[x$topics$package == "tidytext"])
})

test_that("read_inputs reads task views and repo links, current members only", {
  db <- withr::local_tempfile(fileext = ".db")
  make_fixture_store(db)
  x <- read_inputs(db)
  expect_named(x$views, c("view", "package"))
  expect_true("surv01" %in% x$views$package[x$views$view == "Survival"])
  expect_false("surv30" %in% x$views$package[x$views$view == "Survival"])
  expect_named(x$owners, c("package", "owner", "owner_type"))
  expect_equal(nrow(x$owners[x$owners$package == "tidyverse", ]), 1L)
  expect_equal(x$owners$owner[x$owners$package == "foo1"], "fooverse")
})

test_that("packages has the shared columns in order", {
  db <- withr::local_tempfile(fileext = ".db")
  make_fixture_store(db)
  x <- read_inputs(db)
  expect_named(x$packages, c("name", "title", "description", "maintainer",
                             "version", "first_published", "loc_r"))
  expect_named(x$deps, c("from", "to", "type"))
  expect_setequal(unique(x$deps$type), c("Imports", "Suggests"))
})

test_that("deps keep only edges between current CRAN packages", {
  db <- withr::local_tempfile(fileext = ".db")
  extra <- list(packages = data.frame(name = "edgy", version = "1.0", title = "Edgy",
                                      imports = "stats, oldpkg, glue", is_archived = 0L))
  make_fixture_store(db, extra = extra)
  x <- read_inputs(db)
  expect_equal(x$deps$to[x$deps$from == "edgy"], "glue")
  expect_false(any(x$deps$to %in% c("stats", "oldpkg")))
})

test_that("loc_r falls back to the newest row when the version has none", {
  db <- withr::local_tempfile(fileext = ".db")
  extra <- list(
    packages = data.frame(name = c("locfb", "locnone"), version = "3.0",
                          title = c("Loc Fallback", "Loc None"), is_archived = 0L),
    cran_code_summary = data.frame(package = "locfb", version = c("1.0", "2.0"),
                                   loc_r = c(11L, 22L)))
  make_fixture_store(db, extra = extra)
  x <- read_inputs(db)
  expect_equal(x$packages$loc_r[x$packages$name == "locfb"], 22)
  expect_true(is.na(x$packages$loc_r[x$packages$name == "locnone"]))
})

test_that("topics are lower case", {
  db <- withr::local_tempfile(fileext = ".db")
  extra <- list(vcs_signals_summary = data.frame(
    package = "glue", origin = "cran", repo_id = "github.com/x/glue", topics = "Text-Mining,RStats"))
  make_fixture_store(db, extra = extra)
  x <- read_inputs(db)
  expect_setequal(x$topics$topic[x$topics$package == "glue"], c("text-mining", "rstats"))
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
  extra <- list(
    repo_package_links = data.frame(repo_id = "gitlab.com/glab/y", package = "janitor",
                                    origin = "cran"),
    vcs_repo_owner = data.frame(repo_id = "gitlab.com/glab/y", owner_login_current = "glab",
                                owner_type = "Organization", observed_on = "2026-10-20"))
  make_fixture_store(db, extra = extra)
  x <- read_inputs(db)
  expect_false("glab" %in% x$owners$owner)
  expect_false("janitor" %in% x$owners$package)
})
