grp <- function(key, m) list(key = key, members = sort(m), name = NA, description = NA, status = "named",
                             held_reason = NA, named_by = NA, named_at = NA, classification_at = NA, redirect_from = NA)
v <- function(id, m, classification, ...) c(list(subject_id = id, members = sort(m), fingerprint = "x", classification = classification,
                                     judged_at = "2026-11-01", model = "llm"), list(...))
s <- default_settings()
no_basis <- data.frame(subject_id = integer(), package = character())
u <- sprintf("u%02d", 1:25); st <- sprintf("s%02d", 1:25)
clique <- function(p) { m <- t(combn(p, 2)); data.frame(from = m[, 1], to = m[, 2], weight = 1) }
e <- rbind(clique(u), clique(st), data.frame(from = "u01", to = "s01", weight = 1))
test_that("keep names the group and removes strays", {
  out <- apply_classifications(list(grp("k1", c(u, "data.table"))), list(v(3L, c(u, "data.table"), "keep",
    name = "Unit testing", description = "These packages test R code.", strays = "data.table")), e, list(), s)
  expect_equal(out[[1]]$name, "Unit testing"); expect_equal(out[[1]]$status, "judged")
  expect_false("data.table" %in% out[[1]]$members)
})
test_that("hold holds", {
  out <- apply_classifications(list(grp("k1", u)), list(v(3L, u, "hold")), e, list(), s)
  expect_equal(out[[1]]$status, "held"); expect_equal(out[[1]]$held_reason, "classification")
})
test_that("split seeds parts from the LLM's groups", {
  out <- apply_classifications(list(grp("k1", c(u, st))), list(v(3L, c(u, st), "split", groups = list(
    list(name = "Unit testing", packages = u[1:5]), list(name = "Statistical tests", packages = st[1:5])))), e, list(), s)
  expect_setequal(vapply(out, `[[`, "", "name"), c("Unit testing", "Statistical tests"))
  expect_equal(sort(vapply(out, function(g) length(g$members), 1L)), c(25L, 25L))
  expect_true(all(vapply(out, `[[`, "", "redirect_from") == "k1"))
})
test_that("a classification stops applying once membership drifts", {
  out <- apply_classifications(list(grp("k1", u)), list(v(3L, c(u[1:15], sprintf("z%02d", 1:10)), "hold")), e, list(), s)
  expect_equal(out[[1]]$status, "named")
})
test_that("read_classifications fails loudly", {
  f <- withr::local_tempfile(fileext = ".json"); writeLines("{not json", f)
  expect_error(read_classifications(f), "classifications"); expect_error(read_classifications(tempfile()), "classifications")
})
test_that("validate_classification keeps names with dots and digits", {
  expect_length(validate_classification(v(3L, c("data.table", "R6"), "split", groups = list(list(name = "x", packages = "data.table")))), 0L)
  expect_true(length(validate_classification(v(3L, "a", "split", groups = list(list(name = "x", packages = "zzz"))))) > 0L)
})

test_that("keep records the judge and its date", {
  out <- apply_classifications(list(grp("k1", u)), list(v(3L, u, "keep", name = "Unit testing",
    description = "These packages test R code.")), e, list(), s)
  expect_equal(out[[1]]$description, "These packages test R code.")
  expect_equal(out[[1]]$named_by, "llm"); expect_equal(out[[1]]$classification_at, "2026-11-01")
  expect_equal(out[[1]]$members, u)
})
test_that("split parts carry the LLM's descriptions, or NA", {
  out <- apply_classifications(list(grp("k1", c(u, st))), list(v(3L, c(u, st), "split", groups = list(
    list(name = "Unit testing", description = "These packages test R code.", packages = u[1:5]),
    list(name = "Statistical tests", packages = st[1:5])))), e, list(), s)
  names(out) <- vapply(out, `[[`, "", "name")
  expect_equal(out[["Unit testing"]]$description, "These packages test R code.")
  expect_true(is.na(out[["Statistical tests"]]$description))
  expect_equal(out[["Unit testing"]]$members, u); expect_equal(out[["Statistical tests"]]$members, st)
  expect_true(all(vapply(out, `[[`, "", "status") == "judged"))
  expect_true(all(vapply(out, `[[`, "", "named_by") == "llm"))
  expect_equal(length(unique(vapply(out, `[[`, "", "key"))), 2L)
})
test_that("split drops small parts and packages with no links inside the group", {
  m <- c(u, st[1:10], "iso")
  out <- apply_classifications(list(grp("k1", m)), list(v(3L, m, "split", groups = list(
    list(name = "Unit testing", packages = u[1:5]), list(name = "Statistical tests", packages = st[1:5])))), e, list(), s)
  expect_length(out, 1L)
  expect_equal(out[[1]]$name, "Unit testing"); expect_equal(out[[1]]$members, u)
})
test_that("merge joins the group matching the target's last members and takes its key", {
  out <- apply_classifications(list(grp("k1", u), grp("k2", st)), list(v(3L, u, "merge", merge_with = 9L)), e,
                        list(`9` = st[1:20], `4` = u), s)
  expect_length(out, 1L)
  expect_equal(out[[1]]$key, "k2"); expect_equal(out[[1]]$members, sort(c(u, st)))
  expect_equal(out[[1]]$status, "named")
})
test_that("merge with no matching target leaves the group as it was", {
  gs <- list(grp("k1", u), grp("k2", st))
  out <- apply_classifications(gs, list(v(3L, u, "merge", merge_with = 9L)), e, list(`9` = sprintf("z%02d", 1:20)), s)
  expect_equal(out, gs)
  out <- apply_classifications(gs, list(v(3L, u, "merge", merge_with = 9L)), e, list(`9` = u), s)
  expect_equal(out, gs)
})
test_that("a stray joins the other group holding enough of its links, or is dropped", {
  e2 <- rbind(e, data.frame(from = c("s01", "s02", "s03", "u02", "u03"), to = c("x", "x", "x", "x", "y"), weight = 1))
  out <- apply_classifications(list(grp("k1", c(u, "x", "y")), grp("k2", st)), list(v(3L, c(u, "x", "y"), "keep",
    name = "Unit testing", description = "These packages test R code.", strays = c("x", "y"))), e2, list(), s)
  names(out) <- vapply(out, `[[`, "", "key")
  expect_equal(out$k1$members, u)
  expect_equal(out$k2$members, sort(c(st, "x")))
  expect_false("y" %in% unlist(lapply(out, `[[`, "members")))
})
test_that("a keep classification does not lift a hold from the automatic checks", {
  g <- grp("k1", u); g$status <- "held"; g$held_reason <- "maintainers"
  out <- apply_classifications(list(g), list(v(3L, u, "keep", name = "Unit testing",
    description = "These packages test R code.")), e, list(), s)
  expect_equal(out[[1]]$status, "held"); expect_equal(out[[1]]$held_reason, "maintainers")
  expect_equal(out[[1]]$name, "Unit testing")
})
test_that("a keep classification lifts an earlier hold classification", {
  g <- grp("k1", u); g$status <- "held"; g$held_reason <- "classification"
  out <- apply_classifications(list(g), list(v(3L, u, "keep", name = "Unit testing",
    description = "These packages test R code.")), e, list(), s)
  expect_equal(out[[1]]$status, "judged"); expect_true(is.na(out[[1]]$held_reason))
})
test_that("split gives the same parts under any global seed and leaves the global RNG alone", {
  run <- function() apply_classifications(list(grp("k1", c(u, st))), list(v(3L, c(u, st), "split", groups = list(
    list(name = "Unit testing", packages = u[1:2]), list(name = "Statistical tests", packages = st[1:2])))), e, list(), s)
  a <- withr::with_seed(99, run())
  b <- withr::with_seed(7, run())
  expect_identical(a, b)
  expect_equal(sort(vapply(a, function(g) length(g$members), 1L)), c(25L, 25L))
  next_draw <- withr::with_seed(5, { run(); runif(1) })
  expect_equal(next_draw, withr::with_seed(5, runif(1)))
})
test_that("split parts start from last week's parts, so a block between them stays where it was", {
  cc <- sprintf("c%02d", 1:10)
  links <- rbind(e, clique(cc), data.frame(from = rep(cc, each = 3), to = rep(u[1:3], 10), weight = 1),
                 data.frame(from = rep(cc, each = 3), to = rep(st[1:3], 10), weight = 0.9))
  cv <- v(3L, c(u, st, cc), "split", groups = list(list(name = "Unit testing", packages = u[1:5]),
                                                   list(name = "Statistical tests", packages = st[1:5])))
  last <- data.frame(subject_id = c(7L, 8L), name = c("Unit testing", "Statistical tests"),
                     classification_at = "2026-11-01", status = "judged")
  out <- apply_classifications(list(grp("k1", c(u, st, cc))), list(cv), links, list(`7` = u, `8` = c(st, cc)), s, last)
  names(out) <- vapply(out, `[[`, "", "name")
  expect_equal(out[["Statistical tests"]]$members, sort(c(st, cc)))
  expect_equal(out[["Unit testing"]]$members, u)
})

test_that("last week's members of a part with no placed packages left join the parts that have them", {
  z <- sprintf("z%02d", 1:5)
  cv <- v(3L, c(u, st, z), "split", groups = list(list(name = "Unit testing", packages = u[1:5]),
                                                  list(name = "Statistical tests", packages = z)))
  last <- data.frame(subject_id = c(7L, 8L), name = c("Unit testing", "Statistical tests"),
                     classification_at = "2026-11-01", status = "judged")
  out <- apply_classifications(list(grp("k1", c(u, st))), list(cv), e, list(`7` = u, `8` = st), s, last)
  expect_length(out, 1L)
  expect_equal(out[[1]]$members, sort(c(u, st)))
})

test_that("last week's part is not used when its name is ambiguous", {
  cc <- sprintf("c%02d", 1:10)
  links <- rbind(e, clique(cc), data.frame(from = rep(cc, each = 3), to = rep(u[1:3], 10), weight = 1),
                 data.frame(from = rep(cc, each = 3), to = rep(st[1:3], 10), weight = 0.9))
  cv <- v(3L, c(u, st, cc), "split", groups = list(list(name = "Unit testing", packages = u[1:5]),
                                                   list(name = "Statistical tests", packages = st[1:5])))
  # Another subject classified the same day has the same name, so neither tells where the block was.
  last <- data.frame(subject_id = c(7L, 8L, 9L), name = c("Unit testing", "Statistical tests", "Statistical tests"),
                     classification_at = "2026-11-01", status = "judged")
  out <- apply_classifications(list(grp("k1", c(u, st, cc))), list(cv), links,
                               list(`7` = u, `8` = c(st, cc), `9` = sprintf("y%02d", 1:20)), s, last)
  names(out) <- vapply(out, `[[`, "", "name")
  expect_equal(out[["Unit testing"]]$members, sort(c(u, cc)))
})

test_that("a member with no links inside the group is dropped even when last week's part held it", {
  last <- data.frame(subject_id = c(7L, 8L), name = c("Unit testing", "Statistical tests"),
                     classification_at = "2026-11-01", status = "judged")
  cv <- v(3L, c(u, st, "iso"), "split", groups = list(list(name = "Unit testing", packages = u[1:5]),
                                                      list(name = "Statistical tests", packages = st[1:5])))
  out <- apply_classifications(list(grp("k1", c(u, st, "iso"))), list(cv), e, list(`7` = u, `8` = c(st, "iso")), s, last)
  expect_false("iso" %in% unlist(lapply(out, `[[`, "members")))
})

test_that("a part under 20 packages stays only while it keeps 10 of last week's part", {
  m <- c(u, st[1:15])
  cv <- v(3L, m, "split", groups = list(list(name = "Unit testing", packages = u[1:5]),
                                        list(name = "Statistical tests", packages = st[1:5])))
  last <- data.frame(subject_id = c(7L, 8L), name = c("Unit testing", "Statistical tests"),
                     classification_at = "2026-11-01", status = "judged")
  out <- apply_classifications(list(grp("k1", m)), list(cv), e, list(`7` = u, `8` = st[1:20]), s, last)
  names(out) <- vapply(out, `[[`, "", "name")
  expect_equal(out[["Statistical tests"]]$members, st[1:15])
  expect_length(apply_classifications(list(grp("k1", c(u, st[1:9]))), list(cv), e,
                                      list(`7` = u, `8` = st[1:9]), s, last), 1L)
  expect_length(apply_classifications(list(grp("k1", m)), list(cv), e,
                                      list(`7` = u, `8` = c(st[1:15], sprintf("y%02d", 1:16))), s, last), 1L)
})

test_that("a keep holds the subject it judged even when its strays leave it unlike last week's members", {
  junk <- sprintf("j%02d", 1:20)
  g <- grp("k1", c(u[1:15], junk))
  ps <- data.frame(subject_id = c(7L, 9L))
  keep7 <- v(7L, c(u[1:15], junk), "keep", name = "Unit testing", strays = junk)
  prev <- list(`7` = c(u[1:12], junk), `9` = st)
  after <- apply_classifications(list(g), list(keep7), e, prev, s)
  expect_equal(after[[1]]$members, u[1:15])
  fx <- classification_effects(list(g), after, list(keep7), prev, s)
  expect_equal(unname(assign_ids(after, prev, ps, fx, s)), 7L)
  # A keep for a subject that is no longer live gives a new id.
  keep8 <- v(8L, c(u[1:15], junk), "keep", name = "Unit testing", strays = junk)
  fx <- classification_effects(list(g), after, list(keep8), prev, s)
  expect_equal(unname(assign_ids(after, prev, ps, fx, s)), 10L)
})

test_that("a keep does not take a live subject whose members its group no longer holds", {
  junk <- sprintf("j%02d", 1:20)
  g <- grp("k1", c(u[1:15], junk))
  ps <- data.frame(subject_id = c(7L, 9L))
  prev <- list(`7` = c(u[1:12], junk), `9` = st)
  keep9 <- v(9L, c(u[1:15], junk), "keep", name = "Unit testing", strays = junk)
  after <- apply_classifications(list(g), list(keep9), e, prev, s)
  fx <- classification_effects(list(g), after, list(keep9), prev, s)
  expect_equal(unname(assign_ids(after, prev, ps, fx, s)), 10L)
})

test_that("a new subject never takes an id that a classification names", {
  ps <- data.frame(subject_id = c(7L, 9L))
  keep12 <- v(12L, u, "keep", name = "Unit testing")
  after <- apply_classifications(list(grp("k1", u)), list(keep12), e, list(`9` = st), s)
  fx <- classification_effects(list(grp("k1", u)), after, list(keep12), list(`9` = st), s)
  expect_equal(unname(assign_ids(after, list(`9` = st), ps, fx, s)), 13L)
  into15 <- v(3L, u, "merge", merge_with = 15L)
  fx <- classification_effects(list(), list(), list(into15), list(), s)
  expect_equal(unname(assign_ids(list(grp("k1", u)), list(), ps, fx, s)), 16L)
})

test_that("a group under 20 packages that carries no previous subject is dropped", {
  ids <- c(k1 = 7L, k2 = 10L, k3 = 11L)
  out <- drop_new_small(list(grp("k1", u[1:12]), grp("k2", u[13:24]), grp("k3", st)), ids, list(`7` = u[1:14]), s)
  expect_equal(vapply(out$groups, `[[`, "", "key"), c("k1", "k3"))
  expect_equal(out$ids, c(k1 = 7L, k3 = 11L))
})

test_that("a small part stays only when last week's part was published", {
  m <- c(u, st[1:15])
  cv <- v(3L, m, "split", groups = list(list(name = "Unit testing", packages = u[1:5]),
                                        list(name = "Statistical tests", packages = st[1:5])))
  last <- data.frame(subject_id = c(7L, 8L), name = c("Unit testing", "Statistical tests"),
                     classification_at = "2026-11-01", status = c("judged", "held"))
  expect_length(apply_classifications(list(grp("k1", m)), list(cv), e, list(`7` = u, `8` = st[1:20]), s, last), 1L)
})

test_that("one classification applies to one group only, the best match", {
  out <- apply_classifications(list(grp("k1", u[1:10]), grp("k2", u[11:25])), list(v(3L, u, "hold")), e, list(),
                        modifyList(s, list(drift_jaccard = 0.3)))
  expect_equal(vapply(out, `[[`, "", "status"), c("named", "held"))
})
test_that("classification_for takes the closest classification at or above the threshold", {
  vs <- list(v(3L, u[1:20], "hold"), v(4L, u, "hold"))
  expect_equal(classification_for(u, vs, 0.7)$subject_id, 4L)
  expect_equal(classification_for(u[1:20], vs, 0.7)$subject_id, 3L)
  expect_null(classification_for(st, vs, 0.7))
  expect_null(classification_for(u, list(), 0.7))
})
test_that("read_classifications reads what the judge writes and rejects bad files and entries", {
  f <- withr::local_tempfile(fileext = ".json")
  jsonlite::write_json(list(v(3L, c("data.table", "R6"), "split", groups = list(list(name = "x", packages = "data.table"))),
                            v(5L, u, "merge", merge_with = 9L, strays = list())), f, auto_unbox = TRUE)
  r <- read_classifications(f)
  expect_equal(r[[1]]$subject_id, 3L); expect_setequal(r[[1]]$members, c("data.table", "R6"))
  expect_type(r[[1]]$members, "character")
  expect_equal(r[[1]]$groups[[1]]$packages, "data.table")
  expect_equal(r[[2]]$merge_with, 9L); expect_equal(r[[2]]$strays, character())
  writeLines("[]", f); expect_equal(read_classifications(f), list())
  writeLines('{"subject_id": 3}', f); expect_error(read_classifications(f), "classifications")
  writeLines('[{"subject_id": 3, "classification": "keep"}]', f); expect_error(read_classifications(f), "classifications")
  jsonlite::write_json(list(v(3L, u, "hold"), v(3L, u, "hold")), f, auto_unbox = TRUE)
  expect_error(read_classifications(f), "classifications")
})
test_that("validate_classification checks merge targets and where packages are placed", {
  expect_length(validate_classification(v(3L, u, "merge", merge_with = 9L), known_ids = c(3L, 9L)), 0L)
  expect_true(length(validate_classification(v(3L, u, "merge", merge_with = 9L), known_ids = c(3L, 4L))) > 0L)
  expect_true(length(validate_classification(v(3L, u, "merge", merge_with = 3L))) > 0L)
  two <- validate_classification(v(3L, u, "split", groups = list(list(name = "a", packages = u[1:2]), list(name = "b", packages = u[2:3]))))
  expect_match(two, "u02", all = FALSE)
  expect_true(length(validate_classification(v(3L, u, "split", groups = list(list(name = "a", packages = u[1:2])), strays = "u01"))) > 0L)
  expect_match(validate_classification(v(3L, u, "keep", name = "n", description = "d", strays = "zzz")), "zzz", all = FALSE)
  expect_true(length(validate_classification(v(3L, u, "keep", name = "n"))) > 0L)
  expect_length(validate_classification(v(3L, u, "split", groups = list(list(name = "a", description = "d", packages = u[1:2])))), 0L)
  expect_true(length(validate_classification(v(3L, u, "split", groups = list(list(name = "a", description = 4, packages = u[1:2]))))) > 0L)
  expect_true(length(validate_classification(v(3L, u, "rename"))) > 0L)
  expect_true(length(validate_classification(modifyList(v(3L, u, "hold"), list(judged_at = "soon")))) > 0L)
})
test_that("waiting_subjects lists new names, name holds and drifted classifications", {
  p <- sprintf("p%02d", 1:10); q <- sprintf("q%02d", 1:10)
  subj <- data.frame(subject_id = 1:8, slug = paste0("s", 1:8), name = paste("Name", 1:8), description = NA,
    status = c("named", "held", "judged", "judged", "held", "held", "retired", "held"),
    held_reason = c(NA, "name_generic", NA, NA, "classification", "classification", NA, "maintainers"),
    named_by = NA, named_at = NA, classification_at = NA, first_seen = "2026-10-01", last_seen = "2026-10-20",
    redirect_to = NA, size = 10L)
  mem <- function(id, pk, primary = 1L, left = NA) data.frame(subject_id = id, package = pk, is_core = 0L,
    is_primary = primary, reason_kind = NA, reason_value = NA, joined = "2026-10-01", left = left)
  m <- rbind(mem(3L, p), mem(3L, q, primary = 0L), mem(3L, paste0("r", 1:10), left = "2026-10-15"),
             mem(4L, c(p[1:5], q[1:5])), mem(5L, p), mem(6L, q), mem(7L, q), mem(8L, q))
  vs <- list(v(3L, p, "keep", name = "n", description = "d"), v(4L, p, "keep", name = "n", description = "d"),
             v(5L, p, "hold"), v(6L, p, "hold"), v(7L, p, "keep", name = "n", description = "d"), v(8L, p, "hold"))
  w <- waiting_subjects(subj, m, no_basis, vs, s)
  expect_named(w, c("subject_id", "slug", "name", "why"))
  expect_equal(w$subject_id, c(1L, 2L, 4L, 6L))
  expect_equal(w$why, c("new name", "held: name_generic", "drifted", "drifted"))
  expect_equal(w$slug, c("s1", "s2", "s4", "s6"))
  expect_equal(nrow(waiting_subjects(subj[0, ], m[0, ], no_basis, list(), s)), 0L)
})
test_that("a judged split part drifts against its name basis", {
  p <- sprintf("p%02d", 1:10); q <- sprintf("q%02d", 1:15)
  subj <- data.frame(subject_id = c(11L, 12L), slug = c("a", "b"), name = c("Part A", "Part B"), status = "judged",
                     held_reason = NA, stringsAsFactors = FALSE)
  m <- data.frame(subject_id = rep(c(11L, 12L), each = 10), package = c(p, q[1:10]), is_primary = 1L, left = NA)
  split_v <- list(v(3L, c(p, q[1:10]), "split", groups = list(list(name = "Part A", packages = p[1:2]),
                                                              list(name = "Part B", packages = q[1:2]))))
  # Part A matches its basis; Part B shares 5 of 15 packages with its basis.
  basis <- data.frame(subject_id = rep(c(11L, 12L), each = 10), package = c(p, q[c(1:5, 11:15)]))
  w <- waiting_subjects(subj, m, basis, split_v, s)
  expect_equal(w$subject_id, 12L); expect_equal(w$why, "drifted")
  # Part B now shares 9 of 11 packages with its basis, above drift_jaccard.
  basis$package[basis$subject_id == 12L] <- q[c(1:9, 11)]
  expect_equal(nrow(waiting_subjects(subj, m, basis, split_v, s)), 0L)
  expect_equal(nrow(waiting_subjects(subj, m, basis, list(), s)), 0L)
  expect_equal(nrow(waiting_subjects(subj, m, no_basis, split_v, s)), 0L)
})
test_that("drift is measured without strays and with the groups merged in", {
  p <- sprintf("p%02d", 1:10)
  subj <- data.frame(subject_id = c(1L, 2L), slug = c("s1", "s2"), name = c("Name 1", "Name 2"), status = "judged",
                     held_reason = NA, stringsAsFactors = FALSE)
  m <- data.frame(subject_id = c(rep(1L, 6), rep(2L, 50)), package = c(p[5:10], u, st), is_primary = 1L, left = NA)
  vs <- list(v(1L, p, "keep", name = "n", description = "d", strays = p[1:4]),
             v(2L, st, "keep", name = "n", description = "d"), v(3L, u, "merge", merge_with = 2L))
  expect_equal(nrow(waiting_subjects(subj, m, no_basis, vs, s)), 0L)
  w <- waiting_subjects(subj, m, no_basis, vs[1:2], s)
  expect_equal(w$subject_id, 2L); expect_equal(w$why, "drifted")
  w <- waiting_subjects(subj, m, no_basis, list(v(1L, p, "keep", name = "n", description = "d")), s)
  expect_equal(w$subject_id, 1L)
})
