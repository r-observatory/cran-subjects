sw <- load_stopwords()

test_that("tokenize drops stop words and wording-only words, then adds bigrams", {
  expect_equal(tokenize("an api client wrapper for spotify", sw), c("api", "spotify", "api spotify"))
})

test_that("tokenize keeps going on accented text", {
  expect_true(length(tokenize(clean_text("Initiation à La Statistique Avec R"), sw)) > 0)
})

test_that("topic_text drops generic topics and turns hyphens into spaces", {
  expect_equal(topic_text(c("r-package", "survival-analysis", "rstats")), "survival analysis")
})

test_that("clean_text removes markup, links and DOIs", {
  out <- clean_text("See <https://x.org> and doi:10.1/abc for 'pkg'")
  expect_false(grepl("https|doi|<|'", out))
})

test_that("tfidf_matrix applies min_df and max_df and normalises rows", {
  docs <- c(a = "kaplan hazard", b = "kaplan hazard", c = "kaplan hazard", d = "rare word")
  docs <- c(docs, setNames(rep("filler text", 16), paste0("f", 1:16)))
  X <- tfidf_matrix(docs, sw, min_df = 3L, max_df = 0.2)
  expect_true("kaplan" %in% colnames(X))
  expect_false("rare" %in% colnames(X))
  expect_false("filler" %in% colnames(X))
  n <- sqrt(Matrix::rowSums(X^2))
  expect_true(all(abs(n[n > 0] - 1) < 1e-8))
})

test_that("knn_edges keeps close pairs once, summing both directions", {
  Z <- rbind(a = c(1, 0), b = c(1, 0), c = c(1, 0), d = c(0, 1))
  e <- knn_edges(Z, k = 2L, min_sim = 0.3)
  expect_setequal(paste(e$from, e$to), c("a b", "a c", "b c"))
  expect_true(all(abs(e$weight - 2) < 1e-8))
})

test_that("topic_text orders topics bytewise, not by locale", {
  expect_equal(topic_text(c("survivalx", "survival-zoo")), "survival zoo survivalx")
})

test_that("knn_edges gives the same edges for a small chunk", {
  Z <- rbind(a = c(1, 0), b = c(1, 0), c = c(1, 0), d = c(0, 1))
  expect_equal(knn_edges(Z, k = 2L, min_sim = 0.3, chunk = 2L),
               knn_edges(Z, k = 2L, min_sim = 0.3))
  expect_equal(rownames(knn_edges(Z, k = 2L, min_sim = 0.3)), as.character(1:3))
})

test_that("embed leaves the global RNG alone, repeats exactly and normalises rows", {
  X <- withr::with_seed(3, Matrix::rsparsematrix(40, 30, 0.3))
  X[5, ] <- 0
  X <- methods::as(X, "CsparseMatrix")
  rownames(X) <- paste0("p", 1:40)
  a <- withr::with_seed(5, runif(1))
  b <- withr::with_seed(5, { embed(X, 6L, 1L); runif(1) })
  expect_equal(a, b)
  z1 <- embed(X, 6L, 1L)
  expect_identical(z1, embed(X, 6L, 1L))
  expect_equal(dim(z1), c(40L, 6L)); expect_equal(rownames(z1), rownames(X))
  n <- sqrt(rowSums(z1^2))
  expect_true(all(abs(n[-5] - 1) < 1e-8)); expect_equal(n[[5]], 0)
  expect_equal(ncol(embed(X[1:5, 1:4], 50L, 1L)), 3L)
})

test_that("build_documents repeats the title, then description and topics", {
  pk <- data.frame(name = c("p1", "p2"), title = c("Alpha Tool", "Beta"),
                   description = c("Does 'things'", NA), stringsAsFactors = FALSE)
  tp <- data.frame(package = c("p1", "p1"), topic = c("rstats", "genomics-data"),
                   stringsAsFactors = FALSE)
  d <- build_documents(pk, tp, 2L)
  expect_equal(names(d), c("p1", "p2"))
  expect_match(gsub("\\s+", " ", d[["p1"]]), "alpha tool alpha tool does things genomics data")
  expect_true(nzchar(trimws(d[["p2"]])))
})
