db <- withr::local_tempfile(fileext = ".db"); make_fixture_store(db); x <- read_inputs(db)
f <- find_collections(x, default_settings())

test_that("collection packages and organization communities are found, lookalikes are not", {
  expect_equal(sort(f$collections$slug), c("dartrverse", "pharmaverse", "tidyverse"))
  expect_equal(f$collections$kind[f$collections$slug == "pharmaverse"], "organization")
  expect_equal(f$collections$kind[f$collections$slug == "tidyverse"], "package")
})

test_that("tidyverse members and tiers", {
  m <- f$members[f$members$slug == "tidyverse", ]
  expect_setequal(m$package[m$tier == "installed"], c("cli", "dplyr", "ggplot2", "tidyr"))
  expect_equal(m$package[m$tier == "organization"], "glue")
})

test_that("dartRverse takes its kin from Suggests", {
  expect_setequal(f$members$package[f$members$slug == "dartrverse"],
                  c("dartR.base", "dartR.popgen", "dartR.sim"))
})

test_that("an organization community has its owner's packages and the package named like it", {
  m <- f$members[f$members$slug == "pharmaverse", ]
  expect_setequal(m$package, c("admiral", "admiraldev", "pharmaversesdtm"))
  expect_true(all(m$tier == "organization"))
  cl <- f$collections[f$collections$slug == "pharmaverse", ]
  expect_equal(cl$package, "pharmaverse")
  expect_equal(cl$maintainer, "Pharma Maintainer")
})

test_that("mentions are kept apart", {
  mm <- f$mentions[f$mentions$slug == "tidyverse", ]
  expect_equal(mm$source[mm$package == "janitor"], "description")
  expect_equal(mm$source[mm$package == "tidytext"], "topic")
  expect_false(any(mm$package %in% f$members$package[f$members$slug == "tidyverse"]))
  expect_false("tidyverse" %in% mm$package)
})

test_that("tables are sorted and have the agreed columns", {
  expect_named(f$collections, c("slug", "kind", "package", "github_owner", "title",
                                "description", "maintainer"))
  expect_named(f$members, c("slug", "package", "tier", "attached"))
  expect_named(f$mentions, c("slug", "package", "source"))
  expect_false(is.unsorted(f$members$slug))
  expect_equal(f$members$attached[f$members$package == "cli" & f$members$slug == "tidyverse"], 0L)
})

test_that("is_collection_text matches install wording and ignores case", {
  expect_true(is_collection_text("Easily Install and Load the 'Tidyverse'", NA))
  expect_true(is_collection_text("x", "An umbrella package for things."))
  expect_false(is_collection_text("Tidy Messy Data", "Reshape data."))
  expect_equal(is_collection_text(c("A", NA), c(NA, NA)), c(FALSE, FALSE))
})

test_that("Depends marks attached, and a missing loc_r fails", {
  y <- x
  y$deps$type[y$deps$from == "tidyverse" & y$deps$to == "dplyr"] <- "Depends"
  g <- find_collections(y, default_settings())
  expect_equal(g$members$attached[g$members$slug == "tidyverse" & g$members$package == "dplyr"], 1L)
  y$packages$loc_r[y$packages$name == "tidyverse"] <- NA
  g <- find_collections(y, default_settings())
  # no longer a collection package, so its organization stands on its own
  expect_equal(g$collections$kind[g$collections$slug == "tidyverse"], "organization")
  expect_false(any(g$members$package[g$members$slug == "tidyverse"] == "tidyverse"))
})

test_that("curly quotes count as quotes", {
  y <- x
  y$packages$description[y$packages$name == "glue"] <- "Pairs with ‘Tidyverse’ tools."
  g <- find_collections(y, default_settings())
  # glue is an organization member, so it is not a mention
  expect_false("glue" %in% g$mentions$package[g$mentions$slug == "tidyverse"])
  y$packages$description[y$packages$name == "admiral"] <- "Works with “tidyverse”."
  g <- find_collections(y, default_settings())
  expect_equal(g$mentions$source[g$mentions$slug == "tidyverse" & g$mentions$package == "admiral"],
               "description")
})

prev_row <- function(id, slug, last_seen = "2026-10-20") {
  data.frame(collection_id = id, slug = slug, kind = "package", package = slug,
             github_owner = NA_character_, title = "t", description = "d", maintainer = "m",
             first_seen = "2026-10-01", last_seen = last_seen, status = "active",
             stringsAsFactors = FALSE)
}

test_that("a collection that disappears is kept as gone", {
  prev <- list(collections = data.frame(collection_id = 4L, slug = "oldverse", kind = "package", package = "oldverse",
    github_owner = NA, title = "t", description = "d", maintainer = "m", first_seen = "2026-10-01", last_seen = "2026-10-20", status = "active"),
    members = data.frame(), mentions = data.frame())
  out <- carry_collections(prev, f, "2026-10-27")
  expect_equal(out$collections$status[out$collections$slug == "oldverse"], "gone")
  expect_true(all(out$collections$collection_id[out$collections$slug != "oldverse"] > 4L))
})

test_that("cold start numbers in slug order from 1", {
  out <- carry_collections(NULL, f, "2026-10-27")
  expect_equal(out$collections$collection_id, 1:3)
  expect_equal(out$collections$slug, c("dartrverse", "pharmaverse", "tidyverse"))
  expect_true(all(out$collections$first_seen == "2026-10-27" & out$collections$status == "active"))
  expect_setequal(names(out$members), c("collection_id", "package", "tier", "attached"))
  expect_setequal(names(out$mentions), c("collection_id", "package", "source"))
  expect_true(all(out$members$collection_id %in% 1:3))
})

test_that("a found slug keeps its id and first_seen, and gone rows keep members", {
  prev <- list(
    collections = rbind(prev_row(7L, "tidyverse"), prev_row(9L, "oldverse")),
    members = data.frame(collection_id = c(7L, 9L), package = c("zzz", "old1"),
                         tier = "installed", attached = 0L, stringsAsFactors = FALSE),
    mentions = data.frame(collection_id = 9L, package = "old2", source = "topic",
                          stringsAsFactors = FALSE))
  out <- carry_collections(prev, f, "2026-10-27")
  tv <- out$collections[out$collections$slug == "tidyverse", ]
  expect_equal(tv$collection_id, 7L)
  expect_equal(tv$first_seen, "2026-10-01")
  expect_equal(tv$last_seen, "2026-10-27")
  old <- out$collections[out$collections$slug == "oldverse", ]
  expect_equal(c(old$status, old$last_seen), c("gone", "2026-10-20"))
  expect_equal(out$collections$collection_id[out$collections$slug %in% c("dartrverse", "pharmaverse")], c(10L, 11L))
  expect_false("zzz" %in% out$members$package)
  expect_true("old1" %in% out$members$package[out$members$collection_id == 9L])
  expect_equal(out$mentions$package[out$mentions$collection_id == 9L], "old2")
})
