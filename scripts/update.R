#!/usr/bin/env Rscript
# scripts/update.R: the weekly and daily runs that write cran-subjects.db and manifest.json.
#
# run_update(io, out_dir, mode) takes an injectable io for offline testing; default_io()
# supplies the real gh, zstd and llama.cpp calls.
options(timeout = 600)

.this_file <- function() {
  for (i in rev(seq_len(sys.nframe()))) {
    of <- sys.frame(i)$ofile
    if (!is.null(of) && nzchar(of)) return(normalizePath(of))
  }
  a <- commandArgs(FALSE); f <- sub("^--file=", "", grep("^--file=", a, value = TRUE))
  if (length(f) == 1L && nzchar(f)) return(normalizePath(f)) else NA_character_
}
.script_dir <- { tf <- .this_file(); if (!is.na(tf)) dirname(tf) else "scripts" }
if (!exists("read_state", mode = "function")) {
  for (.f in c("config.R", "helpers.R", "inputs.R", "text.R", "graph.R", "checks.R", "identity.R",
               "naming.R", "classifications.R", "reasons.R", "collections.R", "state.R")) {
    source(file.path(.script_dir, .f))
  }
}

PUBLISHED <- c("named", "judged")

STATE_COLS <- list(
  subjects = c(subject_id = "integer", slug = "character", name = "character", description = "character",
               status = "character", held_reason = "character", named_by = "character",
               named_at = "character", classification_at = "character", first_seen = "character",
               last_seen = "character", redirect_to = "integer", size = "integer"),
  subject_members = c(subject_id = "integer", package = "character", is_core = "integer",
                      is_primary = "integer", reason_kind = "character", reason_value = "character",
                      joined = "character", left = "character"),
  subject_name_basis = c(subject_id = "integer", package = "character"),
  subject_task_views = c(subject_id = "integer", view = "character", in_common = "integer"),
  seen_packages = c(package = "character", first_seen = "character"),
  pipeline_runs = c(run_at = "character", mode = "character", n_packages = "integer",
                    n_subjects_published = "integer", n_held = "integer", n_named_calls = "integer",
                    n_collections = "integer"),
  weekly_clusters = c(package = "character", cluster = "integer")
)

# A state table with its export columns and types; NULL gives zero rows.
typed <- function(df, table) {
  cols <- STATE_COLS[[table]]
  n <- if (is.null(df)) 0L else nrow(df)
  out <- lapply(names(cols), function(col) {
    x <- if (!is.null(df) && col %in% names(df)) df[[col]] else rep(NA, n)
    as.vector(x, cols[[col]])
  })
  names(out) <- names(cols)
  out <- as.data.frame(out, stringsAsFactors = FALSE)
  rownames(out) <- NULL
  out
}

run_update <- function(io, out_dir, mode = c("weekly", "daily"), classifications_path = repo_path(CLASSIFICATIONS_PATH),
                       settings = default_settings()) {
  mode <- match.arg(mode)
  say <- if (is.function(io$log)) io$log else function(msg) invisible(NULL)
  clock <- new.env()
  clock$t <- proc.time()[["elapsed"]]
  started <- clock$t
  tick <- function(stage) {
    now <- proc.time()[["elapsed"]]
    say(sprintf("%s: %.1f s", stage, now - clock$t))
    clock$t <- now
  }
  work <- tempfile("cran-subjects-")
  dir.create(work, recursive = TRUE)
  on.exit(unlink(work, recursive = TRUE), add = TRUE)

  # State first, so lost state fails before anything heavy.
  prev <- read_state(io$fetch_previous(work))
  if (mode == "daily" && is.null(prev)) {
    stop("the daily run needs the previous state, so a weekly run must come first", call. = FALSE)
  }
  classifications <- if (mode == "weekly") read_classifications(classifications_path) else list()
  # An emptied classifications file would quietly undo every classification.
  if (mode == "weekly" && !length(classifications) && !is.null(prev) &&
      any(prev$subjects$status %in% "judged" | prev$subjects$held_reason %in% "classification")) {
    stop("the previous state has subjects under a classification, but the classifications file ", classifications_path,
         " holds none", call. = FALSE)
  }
  inputs <- read_inputs(io$fetch_input(work))
  today <- io$today()
  tick("inputs")

  pass <- if (mode == "weekly") weekly_pass(prev, inputs, classifications, io, settings, today, tick, started)
          else daily_pass(prev, inputs, settings, today, tick)
  tables <- pass$tables

  pc <- if (is.null(prev)) NULL else
    list(collections = prev$collections, members = prev$collection_members, mentions = prev$collection_mentions)
  cc <- carry_collections(pc, find_collections(inputs, settings), today)
  tables$collections <- cc$collections
  tables$collection_members <- cc$members
  tables$collection_mentions <- cc$mentions
  tick("collections")

  s <- tables$subjects
  run <- data.frame(run_at = today, mode = mode, n_packages = nrow(inputs$packages),
                    n_subjects_published = sum(s$status %in% PUBLISHED),
                    n_held = sum(s$status %in% "held"), n_named_calls = as.integer(pass$calls),
                    n_collections = sum(cc$collections$status %in% "active"), stringsAsFactors = FALSE)
  tables$pipeline_runs <- rbind(typed(prev$pipeline_runs, "pipeline_runs"), typed(run, "pipeline_runs"))

  check_retention(prev, tables)
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  db_path <- file.path(out_dir, DB_FILENAME)
  export_db(db_path, tables[STATE_TABLES])
  write_manifest(file.path(out_dir, MANIFEST_FILENAME), summary_integrity_core(db_path, complete = TRUE))
  tick("export")
  say(sprintf("%s run: %d packages, %d subjects published, %d held, %d naming calls, %d collections",
              mode, run$n_packages, run$n_subjects_published, run$n_held, run$n_named_calls,
              run$n_collections))
  invisible(db_path)
}

# ---- weekly ----------------------------------------------------------------

weekly_pass <- function(prev, inputs, classifications, io, settings, today, tick, started) {
  pk <- inputs$packages
  ps <- typed(prev$subjects, "subjects")
  pm <- typed(prev$subject_members, "subject_members")
  pb <- typed(prev$subject_name_basis, "subject_name_basis")

  emb <- embed_packages(inputs, settings)
  tick("text")
  edges <- combine_edges(knn_edges(emb$Z, settings$knn_k, settings$knn_min_sim),
                         dependency_edges(inputs$deps, pk$name))
  emb$Z <- NULL
  tick("knn")
  start <- prev$weekly_clusters
  n_start <- sum(pk$name %in% start$package)
  if (is.function(io$log)) {
    io$log(if (n_start) sprintf("%d of %d packages start from last week's clusters", n_start, nrow(pk))
           else "no clusters from last week, so clustering starts from scratch")
  }
  cl <- find_clusters(edges, pk$name, settings$resolution, settings$seed, settings$iterations, start,
                      settings$start_iterations)
  groups <- candidate_groups(cl, pk, settings)
  tick("clusters")

  live <- ps$subject_id[!ps$status %in% "retired"]
  open <- pm[pm$is_primary %in% 1L & is.na(pm$left), , drop = FALSE]
  prev_members <- lapply(live, function(id) sort(open$package[open$subject_id == id], method = "radix"))
  names(prev_members) <- as.character(live)
  after <- apply_classifications(groups, classifications, edges, prev_members, settings, ps)
  fx <- classification_effects(groups, after, classifications, prev_members, settings)
  ids <- assign_ids(after, prev_members, ps, fx, settings)
  tick("classifications and matching")

  titles <- stats::setNames(pk$title, pk$name)
  gram_df <- title_gram_df(titles)
  phrases <- lapply(after, function(g) title_phrases(titles, g$members, gram_df))
  nm <- name_groups(after, ids, ps, split(pb$package, pb$subject_id), phrases, inputs, edges, io,
                    settings, today, started)
  after <- nm$groups
  tick("naming")

  gid <- vapply(after, function(g) ids[[g$key]], 1L)
  fld <- function(f) vapply(after, function(g) {
    x <- g[[f]]
    if (is.null(x) || !length(x)) NA_character_ else as.character(x[[1]])
  }, "")
  j <- match(gid, ps$subject_id)
  current <- typed(data.frame(
    subject_id = gid, slug = assign_slugs(after, gid, ps), name = fld("name"),
    description = fld("description"), status = fld("status"), held_reason = fld("held_reason"),
    named_by = fld("named_by"), named_at = fld("named_at"), classification_at = fld("classification_at"),
    first_seen = ifelse(is.na(j), today, ps$first_seen[j]), last_seen = rep(today, length(gid)),
    stringsAsFactors = FALSE), "subjects")
  if (anyNA(current$status)) stop("a subject was left without a status", call. = FALSE)
  subjects <- rbind(current, ps[ps$status %in% "retired", , drop = FALSE],
                    retire_rows(ps, setdiff(live, gid), after, gid, prev_members, fx))

  members <- carry_members(pm, member_rows(after, gid, inputs, edges, titles, phrases, emb, settings), today)
  keep <- pb[!as.character(pb$subject_id) %in% names(nm$basis), , drop = FALSE]
  basis <- rbind(keep, typed(data.frame(
    subject_id = as.integer(rep(names(nm$basis), lengths(nm$basis))),
    package = unlist(nm$basis, use.names = FALSE), stringsAsFactors = FALSE), "subject_name_basis"))
  tick("members")

  list(tables = list(subjects = with_sizes(subjects, members), subject_members = members,
                     subject_name_basis = sort_rows(basis, c("subject_id", "package")),
                     subject_task_views = task_view_rows(members, inputs$views),
                     seen_packages = seen_rows(prev$seen_packages, pk$name, today),
                     weekly_clusters = typed(cl, "weekly_clusters")),
       calls = nm$calls)
}

# TF-IDF and text vectors for every current package, with the stop words used.
embed_packages <- function(inputs, settings) {
  sw <- load_stopwords(file.path(.script_dir, "stopwords.txt"))
  docs <- build_documents(inputs$packages, inputs$topics, settings$title_weight)
  X <- tfidf_matrix(docs, sw, settings$min_df, settings$max_df)
  list(X = X, Z = embed(X, settings$svd_dim, settings$seed), stopwords = sw)
}

empty_group <- function(key, members, status = NA_character_, held_reason = NA_character_) {
  list(key = key, members = members, name = NA_character_, description = NA_character_,
       status = status, held_reason = held_reason, named_by = NA_character_,
       named_at = NA_character_, classification_at = NA_character_, redirect_from = NA_character_)
}

# Clusters big enough to be subjects; those failing the maintainer check are held.
candidate_groups <- function(cl, packages, settings) {
  maint <- stats::setNames(packages$maintainer, packages$name)
  sizes <- table(cl$cluster)
  big <- sort(as.integer(names(sizes)[sizes >= settings$min_subject_size]))
  lapply(big, function(k) {
    m <- sort(cl$package[cl$cluster == k], method = "radix")
    key <- sprintf("c%05d", k)
    if (maintainer_check(m, maint, settings)) empty_group(key, m) else empty_group(key, m, "held", "maintainers")
  })
}

# What the classifications did: subjects split, merge sources gone, and merged groups pinned to their target.
classification_effects <- function(groups, after, classifications, prev_members, settings) {
  fx <- list(split = stats::setNames(integer(), character()), gone = integer(),
             pin = stats::setNames(integer(), character()))
  if (!length(groups) || !length(classifications)) return(fx)
  pick <- match_classifications(groups, classifications, settings$drift_jaccard)
  keys <- vapply(after, `[[`, "", "key")
  hit <- which(!is.na(pick))
  for (i in hit) {
    v <- classifications[[pick[i]]]
    if (v[["classification"]] == "split") fx$split[groups[[i]]$key] <- as.integer(v[["subject_id"]])
  }
  merged <- hit[vapply(hit, function(i) {
    classifications[[pick[i]]][["classification"]] == "merge" && !groups[[i]]$key %in% keys
  }, NA)]
  src <- vapply(merged, function(i) as.integer(classifications[[pick[i]]][["subject_id"]]), 1L)
  dst <- vapply(merged, function(i) as.integer(classifications[[pick[i]]][["merge_with"]]), 1L)
  fx$gone <- src
  # A into B and B into C ends at C.
  final <- function(id) {
    seen <- integer()
    while (id %in% src && !id %in% seen) {
      seen <- c(seen, id)
      id <- dst[match(id, src)]
    }
    id
  }
  for (k in seq_along(merged)) {
    v <- classifications[[pick[merged[k]]]]
    into <- final(dst[k])
    moved <- setdiff(groups[[merged[k]]]$members, v[["strays"]])
    held <- vapply(after, function(g) sum(moved %in% g$members), 0L)
    if (!as.character(into) %in% names(prev_members) || into %in% c(fx$split, fx$gone, fx$pin) ||
        !any(held > 0L)) next
    to <- keys[order(-held, keys, method = "radix")][1]
    if (!to %in% names(fx$pin)) fx$pin[to] <- into
  }
  fx
}

# Previous ids for matched or pinned groups, then new ids above every previous one, in key order.
assign_ids <- function(after, prev_members, ps, fx, settings) {
  keys <- vapply(after, `[[`, "", "key")
  ids <- stats::setNames(rep(NA_integer_, length(keys)), keys)
  ids[names(fx$pin)] <- fx$pin
  free <- setdiff(names(prev_members), as.character(c(fx$split, fx$gone, fx$pin)))
  open_keys <- keys[is.na(ids)]
  if (length(open_keys) && length(free)) {
    gm <- stats::setNames(lapply(after[match(open_keys, keys)], `[[`, "members"), open_keys)
    mt <- match_to_previous(gm, prev_members[free], settings$match_jaccard)
    ids[mt$key] <- mt$subject_id
  }
  fresh <- sort(keys[is.na(ids)], method = "radix")
  ids[fresh] <- max(c(0L, ps$subject_id)) + seq_along(fresh)
  ids
}

# Last run's name for a group, with the status that goes with it; NULL when it had none.
carry_name <- function(g, p) {
  if (is.null(p) || is.na(p$name) || !nzchar(p$name)) return(NULL)
  g$name <- p$name
  g$description <- p$description
  g$named_by <- p$named_by
  g$named_at <- p$named_at
  g$classification_at <- p$classification_at
  g$held_reason <- NA_character_
  # No keep or split classification applies this run, so a carried name is never judged.
  if (!p$status %in% PUBLISHED && p$held_reason %in% c("name_generic", "name_duplicate")) {
    g$status <- "held"
    g$held_reason <- p$held_reason
  } else {
    g$status <- "named"
  }
  g
}

# Site text rules for a name: no semicolon, and no first word A or An.
site_name_ok <- function(x) {
  first <- strsplit(trimws(x), "\\s+")[[1]][1]
  !grepl(";", x, fixed = TRUE) && !tolower(if (is.na(first)) "" else first) %in% c("a", "an")
}

usable_answer <- function(a) {
  is.list(a) && is.character(a[["name"]]) && length(a[["name"]]) == 1L && !is.na(a[["name"]]) &&
    nzchar(trimws(a[["name"]]))
}

# The open model's sentence, or NA when it breaks the site's text rules.
site_sentence <- function(x) {
  if (!is.character(x) || length(x) != 1L || is.na(x)) return(NA_character_)
  x <- trimws(gsub("\\s+", " ", x))
  if (!nzchar(x) || grepl(";", x, fixed = TRUE) || grepl("(^|[.!?]\\s+)(A|An)\\s", x)) return(NA_character_)
  x
}

# Judged groups keep the LLM's name, held ones keep last run's, and the rest are named when new or drifted.
# No call starts once name_budget_s seconds have passed since `started`.
name_groups <- function(groups, ids, ps, basis, phrases, inputs, edges, io, settings, today,
                        started = proc.time()[["elapsed"]]) {
  force(started)
  new_basis <- list()
  queue <- integer()
  fallback <- vector("list", length(groups))
  was_published <- logical(length(groups))
  for (i in seq_along(groups)) {
    g <- groups[[i]]
    id <- ids[[g$key]]
    j <- match(id, ps$subject_id)
    p <- if (is.na(j)) NULL else ps[j, , drop = FALSE]
    was_published[i] <- !is.null(p) && p$status %in% PUBLISHED
    if (identical(g$status, "judged") || (identical(g$status, "held") && !is.na(g$name))) {
      new_basis[[as.character(id)]] <- g$members
    } else if (identical(g$status, "held")) {
      if (!is.null(p)) {
        for (f in c("name", "description", "named_by", "named_at")) g[[f]] <- p[[f]]
        if (is.na(g$classification_at)) g$classification_at <- p$classification_at
      }
    } else {
      b <- basis[[as.character(id)]]
      kept <- if (!is.null(b) && jaccard(g$members, b) >= settings$drift_jaccard - 1e-12) carry_name(g, p)
      if (!is.null(kept)) {
        g <- kept
      } else {
        if (!is.null(p)) g$classification_at <- p$classification_at
        fallback[i] <- list(carry_name(g, p))
        queue <- c(queue, i)
      }
    }
    groups[[i]] <- g
  }

  calls <- 0L
  fresh <- integer()
  ready <- FALSE
  if (length(queue)) {
    size <- vapply(groups[queue], function(g) length(g$members), 0L)
    keys <- vapply(groups[queue], `[[`, "", "key")
    unnamed <- vapply(fallback[queue], is.null, NA)
    queue <- queue[order(!unnamed, -size, keys, method = "radix")]
    system_prompt <- paste(readLines(file.path(.script_dir, "prompts", "name-system.txt"),
                                     warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    ready <- !is.function(io$can_name) || isTRUE(io$can_name())
  }
  # A subject published last run never unpublishes through its own rename.
  keep_old <- function(i) was_published[i] && !is.null(fallback[[i]])
  over <- FALSE
  for (i in queue) {
    g <- groups[[i]]
    ans <- NULL
    if (ready && calls < settings$name_cap && !over) {
      over <- proc.time()[["elapsed"]] - started >= settings$name_budget_s
      if (over && is.function(io$log)) io$log(sprintf("naming budget reached after %d calls", calls))
      if (!over) {
        calls <- calls + 1L
        ans <- io$name_cluster(system_prompt, name_prompt(g$members, inputs, edges, phrases[[i]], settings))
      }
    }
    if (usable_answer(ans)) {
      clean <- clean_name(ans[["name"]], g$members)
      bad <- is_generic_name(clean) || !site_name_ok(clean)
      desc <- site_sentence(ans[["description"]])
      if ((bad || is.na(desc)) && keep_old(i)) {
        g <- fallback[[i]]
      } else if (!bad && is.na(desc)) {
        # No page without a sentence, so the name is not kept either.
        g$status <- "held"
        g$held_reason <- "name_missing"
      } else {
        g$name <- if (nzchar(clean)) clean else trimws(ans[["name"]])
        g$description <- desc
        g$named_by <- MODEL_SHA256
        g$named_at <- today
        g$status <- if (bad) "held" else "named"
        g$held_reason <- if (bad) "name_generic" else NA_character_
        if (!bad) fresh <- c(fresh, i)
        new_basis[[as.character(ids[[g$key]])]] <- g$members
      }
    } else if (!is.null(fallback[[i]])) {
      g <- fallback[[i]]
    } else {
      g$status <- "held"
      g$held_reason <- "name_missing"
    }
    groups[[i]] <- g
  }

  # Names given this run that nest with any other subject's name are held; a subject
  # published last run takes its old name back instead, and the check runs again.
  repeat {
    named <- which(vapply(groups, function(g) !is.na(g$name) && nzchar(g$name), NA))
    dup <- if (length(fresh) && length(named) > 1L) {
      intersect(fresh, named[near_duplicate_names(vapply(groups[named], `[[`, "", "name"))])
    } else integer()
    back <- dup[vapply(dup, keep_old, NA)]
    if (!length(back)) break
    for (i in back) {
      groups[i] <- fallback[i]
      new_basis[[as.character(ids[[groups[[i]]$key]])]] <- NULL
    }
    fresh <- setdiff(fresh, back)
  }
  for (i in dup) {
    groups[[i]]$status <- "held"
    groups[[i]]$held_reason <- "name_duplicate"
  }
  list(groups = groups, basis = new_basis, calls = calls)
}

# A published slug never changes; a first publish takes one from the name; otherwise held-<id>.
assign_slugs <- function(groups, gid, ps) {
  placeholder <- paste0("held-", gid)
  taken <- c(as.character(ps$slug), placeholder)
  slug <- character(length(groups))
  for (i in order(gid)) {
    old <- ps$slug[match(gid[i], ps$subject_id)]
    if (!is.na(old) && old != placeholder[i]) {
      slug[i] <- old
    } else if (groups[[i]]$status %in% PUBLISHED) {
      slug[i] <- make_slug(groups[[i]]$name, taken)
      taken <- c(taken, slug[i])
    } else {
      slug[i] <- placeholder[i]
    }
  }
  slug
}

# Unmatched previous subjects, retired with a redirect to the largest split part or the main heir
# when that heir is published.
retire_rows <- function(ps, gone, groups, gid, prev_members, fx) {
  rows <- ps[ps$subject_id %in% gone, , drop = FALSE]
  for (r in seq_len(nrow(rows))) {
    id <- rows$subject_id[r]
    to <- NA_integer_
    k <- names(fx$split)[fx$split == id]
    parts <- which(vapply(groups, function(g) isTRUE(g$redirect_from %in% k), NA))
    if (length(parts)) {
      sz <- vapply(groups[parts], function(g) length(g$members), 0L)
      to <- gid[parts][order(-sz, gid[parts])][1]
    } else if (length(groups)) {
      held <- vapply(groups, function(g) sum(prev_members[[as.character(id)]] %in% g$members), 0L)
      if (any(held > 0L)) to <- gid[order(-held, gid)][1]
    }
    # Only a published heir; otherwise NA, which the site answers with 410.
    if (!is.na(to) && !groups[[match(to, gid)]]$status %in% PUBLISHED) to <- NA_integer_
    rows$redirect_to[r] <- to
  }
  rows$status <- rep("retired", nrow(rows))
  rows$held_reason <- rep(NA_character_, nrow(rows))
  rows
}

# Primary and second members of each group, with core flags and reasons.
member_rows <- function(groups, gid, inputs, edges, titles, phrases, emb, settings) {
  if (!length(groups)) return(typed(NULL, "subject_members"))
  desc <- stats::setNames(inputs$packages$description, inputs$packages$name)
  prim <- lapply(groups, `[[`, "members")
  asg <- data.frame(package = unlist(prim, use.names = FALSE),
                    cluster = rep(seq_along(groups), lengths(prim)), stringsAsFactors = FALSE)
  rest <- setdiff(inputs$packages$name, asg$package)
  sh <- link_shares(edges, rbind(asg, data.frame(package = rest, cluster = rep(0L, length(rest)),
                                                 stringsAsFactors = FALSE)))
  sec <- second_memberships(sh, asg, settings$second_share)
  sec <- sec[sec$cluster > 0L, , drop = FALSE]
  rows <- lapply(seq_along(groups), function(i) {
    p <- prim[[i]]
    all <- sort(union(p, sec$package[sec$cluster == i]), method = "radix")
    core <- subject_core(all, inputs$deps, settings)
    ph <- phrases[[i]]
    rs <- placement_reasons(all, inputs$deps, titles, if (length(ph)) ph[[1]] else NA_character_,
                            inputs$topics, description_words(emb$X, p, all, desc, emb$stopwords))
    data.frame(subject_id = gid[i], package = all, is_core = as.integer(all %in% core),
               is_primary = as.integer(all %in% p), reason_kind = rs$reason_kind[match(all, rs$package)],
               reason_value = rs$reason_value[match(all, rs$package)], stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}

# This run's rows keep their joined day; rows no longer current are kept with a left day.
carry_members <- function(pm, cur, today) {
  key <- function(d) paste(d$subject_id, d$package, sep = "\r")
  cur <- typed(cur, "subject_members")
  j <- match(key(cur), key(pm))
  still <- !is.na(j) & is.na(pm$left[j])
  cur$joined <- ifelse(still, pm$joined[j], today)
  cur$left <- rep(NA_character_, nrow(cur))
  gone <- pm[!key(pm) %in% key(cur), , drop = FALSE]
  gone$left <- ifelse(is.na(gone$left), today, gone$left)
  sort_rows(rbind(cur, gone), c("subject_id", "package"))
}

# ---- daily -----------------------------------------------------------------

daily_pass <- function(prev, inputs, settings, today, tick) {
  pk <- inputs$packages
  ps <- typed(prev$subjects, "subjects")
  pm <- typed(prev$subject_members, "subject_members")
  seen <- typed(prev$seen_packages, "seen_packages")
  off <- is.na(pm$left) & !pm$package %in% pk$name
  pm$left[off] <- today
  open <- pm[is.na(pm$left), , drop = FALSE]
  fresh <- setdiff(pk$name, c(seen$package, open$package))
  placed <- if (length(fresh)) place_new(fresh, inputs, ps, open, settings) else NULL
  if (!is.null(placed)) {
    placed$joined <- today
    placed$left <- NA_character_
    placed <- typed(placed, "subject_members")
    key <- function(d) paste(d$subject_id, d$package, sep = "\r")
    pm <- rbind(pm[!key(pm) %in% key(placed), , drop = FALSE], placed)
  }
  pm <- sort_rows(pm, c("subject_id", "package"))
  tick("placement")
  list(tables = list(subjects = with_sizes(ps, pm), subject_members = pm,
                     subject_name_basis = typed(prev$subject_name_basis, "subject_name_basis"),
                     subject_task_views = task_view_rows(pm, inputs$views),
                     seen_packages = seen_rows(seen, pk$name, today),
                     weekly_clusters = typed(prev$weekly_clusters, "weekly_clusters")),
       calls = 0L)
}

# New packages join the published subject holding enough of their link weight, plus second ones.
place_new <- function(fresh, inputs, ps, open, settings) {
  pk <- inputs$packages
  emb <- embed_packages(inputs, settings)
  dep <- dependency_edges(inputs$deps, pk$name)
  e <- combine_edges(knn_rows(emb$Z, fresh, settings$knn_k, settings$knn_min_sim),
                     dep[dep$from %in% fresh | dep$to %in% fresh, , drop = FALSE])
  pub <- ps$subject_id[ps$status %in% PUBLISHED]
  home <- open[open$is_primary %in% 1L & open$subject_id %in% pub, c("package", "subject_id"), drop = FALSE]
  rest <- setdiff(pk$name, home$package)
  sh <- link_shares(e, rbind(data.frame(package = home$package, cluster = home$subject_id, stringsAsFactors = FALSE),
                             data.frame(package = rest, cluster = rep(0L, length(rest)), stringsAsFactors = FALSE)))
  sh <- sh[sh$package %in% fresh & sh$cluster > 0L, , drop = FALSE]
  rows <- list()
  for (p in fresh) {
    s <- sh[sh$package == p, , drop = FALSE]
    s <- s[order(-s$share, s$cluster, method = "radix"), , drop = FALSE]
    if (!nrow(s) || s$share[1] < settings$daily_primary_share - 1e-12) next
    second <- s$cluster[-1][s$share[-1] >= settings$second_share - 1e-12]
    rows[[p]] <- data.frame(subject_id = c(s$cluster[1], second), package = p,
                            is_primary = c(1L, rep(0L, length(second))), stringsAsFactors = FALSE)
  }
  if (!length(rows)) return(NULL)
  rows <- do.call(rbind, rows)
  rows$is_core <- 0L
  rows$reason_kind <- NA_character_
  rows$reason_value <- NA_character_
  titles <- stats::setNames(pk$title, pk$name)
  desc <- stats::setNames(pk$description, pk$name)
  gram_df <- title_gram_df(titles)
  for (id in unique(rows$subject_id)) {
    j <- rows$subject_id == id
    prim <- union(home$package[home$subject_id == id], rows$package[j & rows$is_primary == 1L])
    ph <- title_phrases(titles, prim, gram_df)
    rs <- placement_reasons(union(open$package[open$subject_id == id], rows$package[j]), inputs$deps,
                            titles, if (length(ph)) ph[[1]] else NA_character_, inputs$topics,
                            description_words(emb$X, prim, rows$package[j], desc, emb$stopwords))
    rows$reason_kind[j] <- rs$reason_kind[match(rows$package[j], rs$package)]
    rows$reason_value[j] <- rs$reason_value[match(rows$package[j], rs$package)]
  }
  rownames(rows) <- NULL
  rows
}

# Top text neighbours of the given packages only, as undirected edges.
knn_rows <- function(Z, rows, k, min_sim, chunk = 2000L) {
  nm <- rownames(Z)
  idx <- match(rows, nm)
  idx <- idx[!is.na(idx)]
  k <- min(k, nrow(Z) - 1L)
  if (!length(idx) || k < 1L) return(empty_edges())
  tZ <- t(Z)
  parts <- list()
  for (s in seq(1L, length(idx), by = chunk)) {
    ii <- idx[s:min(s + chunk - 1L, length(idx))]
    S <- Z[ii, , drop = FALSE] %*% tZ
    S[cbind(seq_along(ii), ii)] <- -Inf
    for (r in seq_along(ii)) {
      top <- order(S[r, ], decreasing = TRUE)[seq_len(k)]
      top <- top[S[r, top] >= min_sim]
      if (length(top)) {
        parts[[length(parts) + 1L]] <- data.frame(from = nm[ii[r]], to = nm[top], weight = S[r, top],
                                                  stringsAsFactors = FALSE)
      }
    }
  }
  if (!length(parts)) return(empty_edges())
  combine_edges(do.call(rbind, parts))
}

# ---- shared ------------------------------------------------------------------

sort_rows <- function(d, by) {
  d <- d[do.call(order, c(unname(as.list(d[by])), method = "radix")), , drop = FALSE]
  rownames(d) <- NULL
  d
}

# size is the number of current primary members.
with_sizes <- function(subjects, members) {
  cur <- members$subject_id[members$is_primary %in% 1L & is.na(members$left)]
  subjects$size <- as.integer(table(factor(cur, levels = subjects$subject_id)))
  sort_rows(subjects, "subject_id")
}

# Primary members in common with each task view.
task_view_rows <- function(members, views) {
  m <- members[members$is_primary %in% 1L & is.na(members$left), c("subject_id", "package"), drop = FALSE]
  j <- merge(m, views, by = "package")
  if (!nrow(j)) return(typed(NULL, "subject_task_views"))
  out <- stats::aggregate(package ~ subject_id + view, j, length)
  names(out)[names(out) == "package"] <- "in_common"
  out <- typed(out, "subject_task_views")
  out[order(out$subject_id, -out$in_common, out$view, method = "radix"), , drop = FALSE]
}

# Every current package, with the day it was first seen.
seen_rows <- function(prev_seen, packages, today) {
  prev_seen <- typed(prev_seen, "seen_packages")
  first <- prev_seen$first_seen[match(packages, prev_seen$package)]
  sort_rows(data.frame(package = packages, first_seen = ifelse(is.na(first), today, first),
                       stringsAsFactors = FALSE), "package")
}

# ---- io and CLI --------------------------------------------------------------

USAGE <- "usage: Rscript scripts/update.R OUT_DIR --mode weekly|daily [--input PATH] [--previous PATH|none] [--llama PATH] [--model PATH] [--gpu] [--name-cap N]"

parse_cli <- function(args) {
  out <- list(out_dir = "out", mode = "weekly", input = NULL, previous = NULL,
              llama = "llama-cli", model = file.path("models", MODEL_FILE), gpu = FALSE, name_cap = NULL)
  if (length(args) && !startsWith(args[1], "--")) {
    out$out_dir <- args[1]
    args <- args[-1]
  }
  i <- 1L
  while (i <= length(args)) {
    if (args[i] == "--gpu") {
      out$gpu <- TRUE
      i <- i + 1L
      next
    }
    flag <- gsub("-", "_", sub("^--", "", args[i]), fixed = TRUE)
    if (!startsWith(args[i], "--") || !flag %in% c("mode", "input", "previous", "llama", "model", "name_cap") ||
        i == length(args)) stop(USAGE, call. = FALSE)
    out[[flag]] <- args[i + 1L]
    i <- i + 2L
  }
  if (!out$mode %in% c("weekly", "daily")) stop(USAGE, call. = FALSE)
  if (!is.null(out$name_cap)) {
    if (!grepl("^[0-9]+$", out$name_cap)) stop(USAGE, call. = FALSE)
    out$name_cap <- as.integer(out$name_cap)
  }
  out
}

# The default settings, with the CLI's overrides.
cli_settings <- function(args) {
  s <- default_settings()
  if (!is.null(args$name_cap)) s$name_cap <- args$name_cap
  s
}

gh_run <- function(args) {
  out <- suppressWarnings(system2("gh", args, stdout = TRUE, stderr = TRUE))
  status <- attr(out, "status")
  list(status = if (is.null(status)) 0L else as.integer(status), output = paste(out, collapse = "\n"))
}

# The real fetchers. A previous state that exists but cannot be fetched stops the run.
default_io <- function(args, gh = gh_run) {
  need <- function(path, what) {
    if (!file.exists(path) || is.na(file.size(path)) || file.size(path) == 0) {
      stop(what, " is missing or empty: ", path, call. = FALSE)
    }
    path
  }
  list(
    fetch_input = function(dir) {
      if (!is.null(args$input)) return(need(args$input, "input"))
      r <- gh(c("release", "download", "--repo", INPUT_REPO, "--pattern", INPUT_ASSET,
                "--dir", shQuote(dir), "--clobber"))
      if (r$status != 0L) stop("could not download ", INPUT_ASSET, " from ", INPUT_REPO, ": ", r$output, call. = FALSE)
      zst <- need(file.path(dir, INPUT_ASSET), "downloaded input")
      db <- file.path(dir, sub("\\.zst$", "", INPUT_ASSET))
      status <- system2("zstd", c("-d", "-q", "-f", shQuote(zst), "-o", shQuote(db)))
      if (!identical(as.integer(status), 0L)) stop("zstd could not unpack ", zst, call. = FALSE)
      unlink(zst)
      need(db, "unpacked input")
    },
    fetch_previous = function(dir) {
      if (identical(args$previous, "none")) return(NULL)
      if (!is.null(args$previous)) return(need(args$previous, "previous state"))
      v <- gh(c("release", "view", "current", "--repo", PUBLISH_REPO))
      if (v$status != 0L) {
        if (grepl("release not found", v$output, ignore.case = TRUE)) return(NULL)
        stop("could not look up the current release of ", PUBLISH_REPO, ": ", v$output, call. = FALSE)
      }
      r <- gh(c("release", "download", "current", "--repo", PUBLISH_REPO, "--pattern", DB_FILENAME,
                "--dir", shQuote(dir), "--clobber"))
      if (r$status != 0L) stop("could not download ", DB_FILENAME, " from the current release: ", r$output, call. = FALSE)
      need(file.path(dir, DB_FILENAME), "downloaded previous state")
    },
    # Checked once per weekly run before any naming call.
    can_name = function() {
      if (is.null(args$model) || !file.exists(args$model)) {
        message("model not found: ", if (is.null(args$model)) "(none)" else args$model)
        return(FALSE)
      }
      if (is.null(args$llama) || (!nzchar(Sys.which(args$llama)) && !file.exists(args$llama))) {
        message("llama-cli not found: ", if (is.null(args$llama)) "(none)" else args$llama)
        return(FALSE)
      }
      TRUE
    },
    name_cluster = function(system, user) {
      if (is.null(args$model) || !file.exists(args$model)) return(NULL)
      llama_name(system, user, args$llama, args$model,
                 file.path(.script_dir, "prompts", "name-schema.json"), gpu = isTRUE(args$gpu))
    },
    today = function() format(Sys.time(), "%Y-%m-%d", tz = "UTC"),
    log = function(msg) message(msg)
  )
}

if (sys.nframe() == 0L) {
  REPO_ROOT <- dirname(normalizePath(.script_dir))
  args <- parse_cli(commandArgs(trailingOnly = TRUE))
  run_update(default_io(args), args$out_dir, args$mode, settings = cli_settings(args))
}
