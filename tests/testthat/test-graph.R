test_that("dependency_edges weighs by the target's in-degree and ignores Suggests", {
  d <- data.frame(from = c("a", "b", "c"), to = c("z", "z", "y"), type = c("Imports", "Depends", "Suggests"))
  e <- dependency_edges(d, universe = c("a", "b", "c", "y", "z"))
  expect_equal(nrow(e), 2L)
  expect_equal(unique(e$weight), 1 / log2(4))
})

clique <- function(p) {
  m <- t(combn(p, 2))
  data.frame(from = m[, 1], to = m[, 2], weight = 1)
}

test_that("find_clusters separates two cliques joined by one edge", {
  a <- sprintf("a%02d", 1:25)
  b <- sprintf("b%02d", 1:25)
  e <- rbind(clique(a), clique(b), data.frame(from = "a01", to = "b01", weight = 1))
  cl <- find_clusters(e, c(a, b), resolution = 1, seed = 1L, iterations = 20L)
  expect_equal(sort(as.integer(table(cl$cluster))), c(25L, 25L))
})

test_that("find_clusters gives the same clusters under any global seed and leaves the global RNG alone", {
  a <- sprintf("a%02d", 1:25)
  b <- sprintf("b%02d", 1:25)
  e <- rbind(clique(a), clique(b), data.frame(from = "a01", to = "b01", weight = 1))
  run <- function() find_clusters(e, c(a, b), resolution = 1, seed = 1L, iterations = 20L)
  expect_identical(withr::with_seed(99, run()), withr::with_seed(7, run()))
  next_draw <- withr::with_seed(5, { run(); runif(1) })
  expect_equal(next_draw, withr::with_seed(5, runif(1)))
})

test_that("link_shares and second_memberships use the share of link weight", {
  e <- data.frame(from = c("x", "x"), to = c("p", "q"), weight = c(3, 1))
  asg <- data.frame(package = c("x", "p", "q"), cluster = c(1L, 1L, 2L))
  s <- link_shares(e, asg)
  expect_equal(s$share[s$package == "x" & s$cluster == 1L], 0.75)
  expect_equal(nrow(second_memberships(s, asg, min_share = 0.30)), 0L)
  low <- second_memberships(s, asg, min_share = 0.20)
  expect_equal(nrow(low), 1L)
  expect_equal(low$package, "x")
  expect_equal(low$cluster, 2L)
})

test_that("second_memberships keeps every qualifying cluster and needs a primary link", {
  e <- data.frame(from = c("x", "x", "x", "y"), to = c("p", "q", "r", "q"), weight = c(2, 1, 1, 1))
  asg <- data.frame(package = c("x", "p", "q", "r", "y"), cluster = c(1L, 1L, 2L, 3L, 1L))
  sec <- second_memberships(link_shares(e, asg), asg, min_share = 0.20)
  expect_equal(sec$package, c("x", "x"))
  expect_equal(sec$cluster, c(2L, 3L))
  expect_equal(rownames(sec), c("1", "2"))
})

test_that("subject_core takes members used by at least three others", {
  m <- c("survival", "flexsurv", "x", sprintf("u%d", 1:7))
  d <- data.frame(from = c(sprintf("u%d", 1:6), "u1", "u2", "u3", "u1", "u2"),
                  to = c(rep("survival", 6), rep("flexsurv", 3), "x", "x"), type = "Imports")
  expect_setequal(subject_core(m, d, default_settings()), c("survival", "flexsurv"))
})

test_that("combine_edges sums duplicate pairs and keeps the edge convention", {
  e1 <- data.frame(from = c("a", "b"), to = c("b", "c"), weight = c(1, 2))
  e2 <- data.frame(from = c("b", "d"), to = c("a", "c"), weight = c(0.5, 1))
  e <- combine_edges(e1, e2)
  expect_equal(e$from, c("a", "b", "c"))
  expect_equal(e$to, c("b", "c", "d"))
  expect_equal(e$weight, c(1.5, 2, 1))
  expect_equal(rownames(e), as.character(1:3))
})

test_that("find_clusters returns every package and gives isolated ones their own cluster", {
  e <- clique(c("a", "b", "c"))
  cl <- find_clusters(e, c("a", "b", "c", "i", "j"), resolution = 1, seed = 1L, iterations = 10L)
  expect_equal(sort(cl$package), c("a", "b", "c", "i", "j"))
  expect_type(cl$cluster, "integer")
  expect_equal(length(unique(cl$cluster)), 3L)
  none <- find_clusters(e[0, ], c("p", "q"), resolution = 1, seed = 1L, iterations = 10L)
  expect_equal(length(unique(none$cluster)), 2L)
})

# Planted groups of 25 with sparse links between them, and new packages tied to one group each.
planted <- function(n_groups, p_in, p_out) withr::with_seed(11, {
  p <- sprintf("p%04d", seq_len(n_groups * 25L))
  grp <- rep(seq_len(n_groups), each = 25L)
  pairs <- t(combn(length(p), 2))
  same <- grp[pairs[, 1]] == grp[pairs[, 2]]
  keep <- stats::runif(nrow(pairs)) < ifelse(same, p_in, p_out)
  list(p = p, grp = grp, e = data.frame(from = p[pairs[keep, 1]], to = p[pairs[keep, 2]],
                                        weight = stats::runif(sum(keep), 0.3, 1)))
})
next_week <- function(g, jitter, n_new) withr::with_seed(3, {
  newp <- sprintf("n%03d", seq_len(n_new))
  links <- do.call(rbind, lapply(newp, function(x) {
    data.frame(from = x, to = sample(g$p[g$grp == sample(g$grp, 1)], 4), weight = 0.8)
  }))
  e <- rbind(g$e, links)
  e$weight <- e$weight * (1 + jitter * stats::rnorm(nrow(e)))
  list(p = c(g$p, newp), e = e)
})
# Share of the clusters of 20 or more found again at Jaccard 0.7 or more.
kept_share <- function(before, after) {
  big <- function(cl) {
    s <- table(cl$cluster)
    lapply(as.integer(names(s)[s >= 20L]), function(k) cl$package[cl$cluster == k])
  }
  b <- big(before); a <- big(after)
  mean(vapply(b, function(x) max(vapply(a, function(y) jaccard(x, y), 0)), 0) >= 0.7)
}

test_that("find_clusters started from last week's clusters keeps them as weights shift and packages arrive", {
  g <- planted(40L, 0.20, 0.010)
  before <- find_clusters(g$e, g$p, resolution = 3, seed = 1L, iterations = 20L)
  h <- next_week(g, jitter = 0.05, n_new = 10L)
  after <- find_clusters(h$e, h$p, resolution = 3, seed = 1L, iterations = 20L, start = before)
  expect_gte(kept_share(before, after), 0.95)
  expect_setequal(after$package, h$p)
})

test_that("a package missing from the start clusters joins the cluster it links to", {
  a <- sprintf("a%02d", 1:25)
  b <- sprintf("b%02d", 1:25)
  e <- rbind(clique(a), clique(b), data.frame(from = "a01", to = "b01", weight = 1))
  before <- rbind(find_clusters(e, c(a, b), resolution = 1, seed = 1L, iterations = 20L),
                  data.frame(package = "gone", cluster = 99L))
  e2 <- rbind(e, data.frame(from = "new", to = a[2:6], weight = 1))
  after <- find_clusters(e2, c(a, b, "new"), resolution = 1, seed = 1L, iterations = 20L, start = before)
  expect_equal(after$cluster[after$package == "new"], after$cluster[after$package == "a02"])
  expect_equal(sort(as.integer(table(after$cluster))), c(25L, 26L))
  expect_false("gone" %in% after$package)
})

test_that("a package that lost every link leaves last week's cluster", {
  a <- sprintf("a%02d", 1:25)
  b <- sprintf("b%02d", 1:25)
  e <- rbind(clique(a), clique(b), data.frame(from = "a01", to = "b01", weight = 1))
  before <- find_clusters(e, c(a, b), resolution = 1, seed = 1L, iterations = 20L)
  after <- find_clusters(e[e$from != "a25" & e$to != "a25", ], c(a, b), resolution = 1, seed = 1L,
                         iterations = 20L, start = before)
  expect_equal(sum(after$cluster == after$cluster[after$package == "a25"]), 1L)
})

test_that("an empty start clusters from scratch", {
  g <- planted(40L, 0.20, 0.010)
  empty <- data.frame(package = character(), cluster = integer())
  expect_identical(find_clusters(g$e, g$p, resolution = 3, seed = 1L, iterations = 20L, start = empty),
                   find_clusters(g$e, g$p, resolution = 3, seed = 1L, iterations = 20L))
})

test_that("subject_core keeps only the top fraction in a large subject", {
  m <- sprintf("m%02d", 1:10)
  d <- data.frame(from = rep(m[-1], 2), to = rep(c("m01", "m02"), c(9, 9)), type = "Imports")
  s <- modifyList(default_settings(), list(core_large_subject = 5L, core_top_fraction = 0.1))
  expect_equal(subject_core(m, d, s), "m01")
})
