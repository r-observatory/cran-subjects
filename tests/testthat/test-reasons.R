test_that("reason_text writes plain facts", {
  expect_equal(reason_text("uses", "ggplot2"), "Uses ggplot2")
  expect_equal(reason_text("used_by", "survival"), "Used by survival")
  expect_equal(reason_text("title", "survival analysis"), "Its title names survival analysis")
  expect_equal(reason_text("topic", "spatial"), "Repository topic: spatial")
})
test_that("reason_text is vectorised and gives NA for NA kinds", {
  expect_equal(
    reason_text(c("uses", NA, "topic"), c("dplyr", "x", "gis")),
    c("Uses dplyr", NA, "Repository topic: gis")
  )
})
test_that("placement_reasons follows the order uses, used by, title, topic", {
  m <- c("survival", "flexsurv", "muhaz", "km1")
  d <- data.frame(from = "flexsurv", to = "survival", type = "Imports")
  t <- c(survival = "Survival Analysis", flexsurv = "Flexible Models", muhaz = "Hazard Survival Analysis", km1 = "Kaplan Plots")
  tp <- data.frame(package = c("km1", "a", "b"), topic = "kaplan-meier")
  r <- placement_reasons(m, d, t, "survival analysis", tp)
  expect_equal(r$reason_kind[r$package == "flexsurv"], "uses")
  expect_equal(r$reason_kind[r$package == "survival"], "used_by")
  expect_equal(r$reason_kind[r$package == "muhaz"], "title")
  expect_true(is.na(r$reason_kind[r$package == "km1"]))
})
test_that("uses picks the dependency with the most in-subject users", {
  m <- c("app", "zeta", "alpha", "helper")
  d <- data.frame(
    from = c("app", "app", "helper"),
    to = c("zeta", "alpha", "zeta"),
    type = c("Depends", "Imports", "LinkingTo")
  )
  r <- placement_reasons(m, d, character(0), NA, NULL)
  expect_equal(r$reason_value[r$package == "app"], "zeta")
  expect_equal(r$reason_kind[r$package == "app"], "uses")
})
test_that("uses breaks ties by name, bytewise", {
  m <- c("app", "zeta", "alpha")
  d <- data.frame(from = "app", to = c("zeta", "alpha"), type = "Imports")
  r <- placement_reasons(m, d, character(0), NA, NULL)
  expect_equal(r$reason_value[r$package == "app"], "alpha")
})
test_that("used by picks the dependent with the most in-subject users, then by name", {
  m <- c("core", "x", "y", "z", "w")
  d <- data.frame(
    from = c("x", "y", "z", "w"),
    to = c("core", "core", "core", "z"),
    type = "Imports"
  )
  r <- placement_reasons(m, d, character(0), NA, NULL)
  expect_equal(r$reason_value[r$package == "core"], "z")
  expect_equal(r$reason_kind[r$package == "core"], "used_by")
  d2 <- data.frame(from = c("x", "y"), to = "core", type = "Imports")
  r2 <- placement_reasons(c("core", "x", "y"), d2, character(0), NA, NULL)
  expect_equal(r2$reason_value[r2$package == "core"], "x")
})
test_that("Suggests, outside dependencies and self loops give no reason", {
  d <- data.frame(
    from = c("a", "b", "c", "c"),
    to = c("b", "a", "zz", "c"),
    type = c("Suggests", "Suggests", "Imports", "Imports")
  )
  r <- placement_reasons(c("a", "b", "c"), d, character(0), NA, NULL)
  expect_true(all(is.na(r$reason_kind)))
  expect_true(all(is.na(r$reason_value)))
})
test_that("title needs a phrase and ignores case", {
  t <- c(a = "SURVIVAL Analysis", b = "Plots")
  r <- placement_reasons(c("a", "b"), NULL, t, "survival analysis", NULL)
  expect_equal(r$reason_kind, c("title", NA))
  expect_equal(r$reason_value, c("survival analysis", NA))
  r2 <- placement_reasons(c("a", "b"), NULL, t, NA, NULL)
  expect_true(all(is.na(r2$reason_kind)))
})
test_that("topic needs at least 3 members, the package included", {
  m <- c("a", "b", "c", "d")
  tp <- data.frame(package = c("a", "b", "c", "d", "a", "b"), topic = c("spatial", "spatial", "spatial", "spatial", "gis", "gis"))
  r <- placement_reasons(m, NULL, character(0), NA, tp)
  expect_equal(r$reason_kind, rep("topic", 4))
  expect_equal(r$reason_value, rep("spatial", 4))
  tp2 <- data.frame(package = c("a", "b", "c"), topic = c("spatial", "spatial", "gis"))
  r2 <- placement_reasons(c("a", "b", "c"), NULL, character(0), NA, tp2)
  expect_true(all(is.na(r2$reason_kind)))
  expect_true(all(is.na(r2$reason_value)))
})
test_that("topic takes the most shared topic when several qualify", {
  m <- c("a", "b", "c", "d")
  tp <- data.frame(
    package = c("a", "a", "b", "b", "c", "c", "d", "d"),
    topic = c("gis", "spatial", "gis", "spatial", "gis", "spatial", "spatial", "x")
  )
  r <- placement_reasons(m, NULL, character(0), NA, tp)
  expect_equal(r$reason_value[r$package == "a"], "spatial")
  expect_equal(r$reason_value[r$package == "d"], "spatial")
})
test_that("output keeps member order with reset row names", {
  m <- c("zz", "aa", "mm")
  r <- placement_reasons(m, NULL, character(0), NA, NULL)
  expect_equal(r$package, m)
  expect_equal(names(r), c("package", "reason_kind", "reason_value"))
  expect_equal(rownames(r), as.character(1:3))
})
