test_that("jaccard", expect_equal(jaccard(c("a", "b"), c("b", "c")), 1 / 3))
test_that("match_to_previous is one to one, best first", {
  g <- list(g1 = letters[1:10], g2 = letters[11:20], g3 = letters[1:6])
  p <- list(`7` = c(letters[1:9], "z"), `9` = letters[21:26])
  m <- match_to_previous(g, p, threshold = 0.5)
  expect_equal(m$subject_id[m$key == "g1"], 7L)
  expect_true(is.na(m$subject_id[m$key == "g2"]))
  expect_true(is.na(m$subject_id[m$key == "g3"]))
})
test_that("make_slug lowers, hyphenates and never reuses", {
  expect_equal(make_slug("ggplot2 extensions", character()), "ggplot2-extensions")
  expect_equal(make_slug("C++ library interfaces", character()), "c-library-interfaces")
  expect_equal(make_slug("Colour palettes", c("colour-palettes", "colour-palettes-2")), "colour-palettes-3")
})
