# scripts/classifications.R: read the LLM's classifications, apply them to clusters, and list subjects waiting for one.

CLASSIFICATION_KINDS <- c("keep", "split", "merge", "hold")

# JSON arrays of strings as character vectors.
json_chr <- function(x) {
  if (is.list(x) && all(vapply(x, function(e) is.character(e) && length(e) == 1L, NA))) {
    return(as.character(unlist(x, use.names = FALSE)))
  }
  x
}

json_int <- function(x) {
  if (is.numeric(x) && length(x) == 1L && !is.na(x) && x == round(x)) as.integer(x) else x
}

normalise_classification <- function(v) {
  if (!is.list(v)) return(v)
  for (f in c("members", "strays")) if (!is.null(v[[f]])) v[[f]] <- json_chr(v[[f]])
  for (f in c("subject_id", "merge_with")) if (!is.null(v[[f]])) v[[f]] <- json_int(v[[f]])
  if (is.list(v[["groups"]])) {
    v[["groups"]] <- lapply(v[["groups"]], function(g) {
      if (is.list(g) && !is.null(g[["packages"]])) g[["packages"]] <- json_chr(g[["packages"]])
      g
    })
  }
  v
}

# The classifications file as a list of entries; stops on any fault.
read_classifications <- function(path) {
  if (!file.exists(path)) stop("classifications file not found: ", path, call. = FALSE)
  x <- tryCatch(
    jsonlite::fromJSON(paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n"),
                       simplifyVector = FALSE),
    error = function(e) stop("classifications file is not valid JSON: ", path, " (", conditionMessage(e), ")",
                             call. = FALSE))
  if (!is.list(x) || !is.null(names(x))) stop("classifications file must hold a JSON array: ", path, call. = FALSE)
  x <- lapply(x, normalise_classification)
  for (i in seq_along(x)) {
    err <- validate_classification(x[[i]])
    if (length(err)) {
      stop(sprintf("classifications entry %d in %s: %s", i, path, paste(err, collapse = ", ")), call. = FALSE)
    }
  }
  ids <- vapply(x, function(v) as.integer(v[["subject_id"]]), 1L)
  dup <- unique(ids[duplicated(ids)])
  if (length(dup)) stop("classifications file repeats subject_id ", paste(dup, collapse = ", "), call. = FALSE)
  x
}

# Problems with one classification entry, empty when it is valid.
validate_classification <- function(v, known_ids = NULL) {
  if (!is.list(v) || is.null(names(v))) return("entry is not an object")
  str1 <- function(x) is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x)
  whole <- function(x) is.numeric(x) && length(x) == 1L && !is.na(x) && x == round(x)
  pkgs <- function(x) is.character(x) && length(x) > 0L && !anyNA(x) && all(nzchar(x))
  listed <- function(x) paste(sort(unique(x), method = "radix"), collapse = ", ")
  err <- character()
  id_ok <- whole(v[["subject_id"]])
  if (!id_ok) err <- c(err, "subject_id must be an integer")
  members <- v[["members"]]
  members_ok <- pkgs(members)
  if (!members_ok) err <- c(err, "members must list package names")
  if (members_ok && anyDuplicated(members)) err <- c(err, "members name a package twice")
  if (!str1(v[["fingerprint"]])) err <- c(err, "fingerprint must be a string")
  if (!str1(v[["model"]])) err <- c(err, "model must be a string")
  ja <- v[["judged_at"]]
  if (!str1(ja) || !grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", ja) || is.na(as.Date(ja, format = "%Y-%m-%d"))) {
    err <- c(err, "judged_at must be a date")
  }
  kind <- v[["classification"]]
  if (!str1(kind) || !kind %in% CLASSIFICATION_KINDS) return(c(err, "classification must be keep, split, merge or hold"))
  strays <- v[["strays"]]
  if (!is.null(strays) && !(is.character(strays) && !anyNA(strays))) {
    err <- c(err, "strays must list package names")
    strays <- NULL
  }
  placed <- character()
  if (kind == "keep") {
    if (!str1(v[["name"]])) err <- c(err, "keep needs a name")
    if (!str1(v[["description"]])) err <- c(err, "keep needs a description")
  }
  if (kind == "merge") {
    mw <- v[["merge_with"]]
    if (!whole(mw)) {
      err <- c(err, "merge_with must be an integer")
    } else {
      if (id_ok && mw == v[["subject_id"]]) err <- c(err, "merge_with must name another subject")
      if (!is.null(known_ids) && !mw %in% known_ids) err <- c(err, sprintf("merge_with %d is not a known subject", as.integer(mw)))
    }
  }
  if (kind == "split") {
    gs <- v[["groups"]]
    if (!is.list(gs) || !length(gs) || !is.null(names(gs))) {
      err <- c(err, "split needs a list of groups")
      gs <- list()
    }
    for (i in seq_along(gs)) {
      g <- gs[[i]]
      if (!is.list(g) || is.null(names(g))) {
        err <- c(err, sprintf("group %d is not an object", i))
        next
      }
      if (!str1(g[["name"]])) err <- c(err, sprintf("group %d needs a name", i))
      if (pkgs(g[["packages"]])) placed <- c(placed, g[["packages"]]) else err <- c(err, sprintf("group %d needs packages", i))
      d <- g[["description"]]
      if (!is.null(d) && !(length(d) == 1L && (is.na(d) || str1(d)))) {
        err <- c(err, sprintf("group %d description must be a string", i))
      }
    }
  }
  if (members_ok) {
    outside <- setdiff(c(placed, strays), members)
    if (length(outside)) err <- c(err, paste("packages not among members:", listed(outside)))
  }
  twice <- placed[duplicated(placed)]
  if (length(twice)) err <- c(err, paste("packages in two groups:", listed(twice)))
  both <- intersect(placed, strays)
  if (length(both)) err <- c(err, paste("packages in a group and in strays:", listed(both)))
  err
}

classification_scores <- function(members, classifications) {
  vapply(classifications, function(v) jaccard(members, v[["members"]]), 0)
}

# Index of the best classification at or above the threshold; ties go to the latest, then the lowest id.
pick_classification <- function(scores, classifications, threshold) {
  ok <- which(scores >= threshold - 1e-12 & scores > 0)
  if (!length(ok)) return(NA_integer_)
  when <- vapply(classifications[ok], function(v) as.character(v[["judged_at"]]), "")
  ids <- vapply(classifications[ok], function(v) as.integer(v[["subject_id"]]), 1L)
  ok[order(-scores[ok], when, ids, decreasing = c(FALSE, TRUE, FALSE), method = "radix")][1]
}

# The overlap each classification needs to apply. A split holds while its group still carries the subject it
# split, so its parts keep their ids until the next judging; every other kind stops at drift_jaccard.
classification_thresholds <- function(classifications, settings) {
  vapply(classifications, function(v) {
    if (identical(v[["classification"]], "split")) settings$match_jaccard else settings$drift_jaccard
  }, 0)
}

# The raw cluster holding most of a split's members, which the weekly run matches the split against.
split_cluster <- function(v, clusters) {
  if (is.null(clusters) || !nrow(clusters)) return(character())
  k <- clusters$cluster[clusters$package %in% v[["members"]]]
  if (!length(k)) return(character())
  home <- names(which.max(table(k)))
  sort(clusters$package[as.character(clusters$cluster) == home], method = "radix")
}

classification_for <- function(members, classifications, threshold) {
  if (!length(classifications)) return(NULL)
  i <- pick_classification(classification_scores(members, classifications), classifications, threshold)
  if (is.na(i)) NULL else classifications[[i]]
}

# Each group's classification index; a classification goes only to the group it matches best.
match_classifications <- function(groups, classifications, threshold) {
  sc <- lapply(groups, function(g) classification_scores(g$members, classifications))
  pick <- vapply(sc, pick_classification, 1L, classifications = classifications, threshold = threshold)
  keys <- vapply(groups, function(g) as.character(g$key), "")
  for (j in unique(pick[!is.na(pick) & duplicated(pick)])) {
    rows <- which(pick == j)
    s <- vapply(sc[rows], `[`, 0, j)
    best <- rows[order(-s, keys[rows], method = "radix")][1]
    pick[setdiff(rows, best)] <- NA_integer_
  }
  pick
}

# For each published subject, the index of its latest classification judged after its own classification_at,
# named by subject id. Such a classification applies once, by id, whatever the clusters did.
due_classifications <- function(classifications, subjects) {
  out <- stats::setNames(integer(), character())
  if (!length(classifications) || is.null(subjects) || !nrow(subjects)) return(out)
  vid <- vapply(classifications, function(v) as.integer(v[["subject_id"]]), 1L)
  when <- vapply(classifications, function(v) as.character(v[["judged_at"]]), "")
  pub <- subjects[subjects$status %in% c("named", "judged"), , drop = FALSE]
  for (r in seq_len(nrow(pub))) {
    at <- as.character(pub$classification_at[r])
    k <- which(vid == pub$subject_id[r] & (is.na(at) | when > at))
    if (length(k)) out[as.character(pub$subject_id[r])] <- k[order(when[k], k, decreasing = TRUE)][1]
  }
  out
}

# A hold from the automatic checks, which no classification lifts.
held_by_checks <- function(g) {
  r <- as.character(g$held_reason)
  identical(g$status, "held") && !isTRUE(startsWith(r, "name_")) && !identical(r, "classification")
}

judge_group <- function(g, v, name, description) {
  g$name <- name
  g$description <- description
  g$named_by <- v[["model"]]
  g$named_at <- v[["judged_at"]]
  g$classification_at <- v[["judged_at"]]
  if (!held_by_checks(g)) {
    g$status <- "judged"
    g$held_reason <- NA_character_
  }
  g
}

# Label propagation inside the group, seeded and fixed at the LLM's placements. Other linked members
# start in last week's part of the same name from this classification, so parts hold steady; a part
# with no placed package here, or a name that does not pick out one subject, gives no start. A part
# under the entry size stays while it still carries last week's published part.
split_group <- function(g, v, edges, settings, prev_members = list(), prev_subjects = NULL) {
  members <- g$members
  e <- edges[edges$from %in% members & edges$to %in% members, , drop = FALSE]
  gr <- igraph::graph_from_data_frame(e[, c("from", "to")], directed = FALSE,
                                      vertices = data.frame(name = members))
  cg <- v[["groups"]]
  seed <- setNames(rep(-1L, length(members)), members)
  for (j in seq_along(cg)) seed[members[members %in% cg[[j]][["packages"]]]] <- j
  fixed <- seed >= 0L
  if (!any(fixed)) return(list())
  init <- seed
  was <- rep(NA_character_, length(cg))
  if (!is.null(prev_subjects)) {
    names_cg <- vapply(cg, function(x) as.character(x[["name"]]), "")
    linked <- members %in% c(e$from, e$to)
    for (j in intersect(seq_along(cg), seed[fixed])) {
      id <- prev_subjects$subject_id[prev_subjects$name %in% names_cg[j] & !prev_subjects$status %in% "retired" &
                                       prev_subjects$classification_at %in% v[["judged_at"]]]
      if (length(id) != 1L || sum(names_cg == names_cg[j]) != 1L) next
      was[j] <- as.character(id)
      init[!fixed & linked & members %in% prev_members[[was[j]]]] <- j
    }
  }
  withr::local_seed(if (is.null(settings$seed)) 1L else settings$seed)
  lab <- as.integer(igraph::membership(igraph::cluster_label_prop(
    gr, weights = if (nrow(e)) e$weight else NULL, initial = unname(init), fixed = unname(fixed))))
  owner <- tapply(seed[fixed], lab[fixed], function(x) x[1])
  part <- unname(owner[as.character(lab)])
  parts <- list()
  for (j in seq_along(cg)) {
    pm <- members[which(part == j)]
    if (length(pm) < settings$min_subject_size &&
        (is.na(was[j]) || !prev_subjects$status[match(as.integer(was[j]), prev_subjects$subject_id)] %in% PUBLISHED ||
         !keeps_subject(pm, prev_members[[was[j]]], settings))) next
    d <- cg[[j]][["description"]]
    p <- g
    p$key <- paste0(g$key, "/", j)
    p$members <- pm
    p$redirect_from <- g$key
    parts[[length(parts) + 1L]] <- judge_group(p, v, cg[[j]][["name"]], if (is.null(d)) NA_character_ else d)
  }
  parts
}

# Strays join the strongest group outside their old one, at or above second_share.
place_strays <- function(groups, from, strays, edges, settings) {
  mem <- lapply(groups, `[[`, "members")
  grouped <- unlist(mem, use.names = FALSE)
  e <- edges[(edges$from %in% strays$package & edges$to %in% grouped) |
               (edges$to %in% strays$package & edges$from %in% grouped), , drop = FALSE]
  if (!nrow(e)) return(groups)
  asg <- data.frame(package = c(grouped, strays$package),
                    cluster = c(rep(seq_along(groups), lengths(mem)), rep(NA_integer_, nrow(strays))),
                    stringsAsFactors = FALSE)
  sh <- link_shares(e, asg)
  sh <- sh[sh$package %in% strays$package & sh$share >= settings$second_share - 1e-12, , drop = FALSE]
  for (r in seq_len(nrow(strays))) {
    s <- sh[sh$package == strays$package[r], , drop = FALSE]
    s <- s[!vapply(s$cluster, function(k) strays$origin[r] %in% from[[k]], NA), , drop = FALSE]
    if (!nrow(s)) next
    k <- s$cluster[order(-s$share, s$cluster, method = "radix")][1]
    groups[[k]]$members <- c(groups[[k]]$members, strays$package[r])
  }
  groups
}

# Apply each group's classification: strays leave, then splits, holds and keeps, then merges. `pick` gives each
# group's classification index directly, NA for none; without it groups match classifications by overlap.
apply_classifications <- function(groups, classifications, edges, prev_members, settings, prev_subjects = NULL,
                                  pick = NULL) {
  if (!length(groups) || !length(classifications)) return(groups)
  if (is.null(pick)) pick <- match_classifications(groups, classifications, classification_thresholds(classifications, settings))
  strays <- data.frame(package = character(), origin = character(), stringsAsFactors = FALSE)
  out <- list()
  from <- list()
  merges <- list()
  for (i in seq_along(groups)) {
    g <- groups[[i]]
    if (is.na(pick[i])) {
      out <- c(out, list(g))
      from <- c(from, list(g$key))
      next
    }
    v <- classifications[[pick[i]]]
    sv <- g$members[g$members %in% v[["strays"]]]
    if (length(sv)) {
      g$members <- setdiff(g$members, sv)
      strays <- rbind(strays, data.frame(package = sv, origin = g$key, stringsAsFactors = FALSE))
    }
    kind <- v[["classification"]]
    if (kind == "split") {
      # Parts are new subjects, not the steady one they came from.
      parts <- lapply(split_group(g, v, edges, settings, prev_members, prev_subjects), function(p) {
        p$steady <- NULL
        p$subject_id <- NULL
        p
      })
      out <- c(out, parts)
      from <- c(from, rep(list(g$key), length(parts)))
      next
    }
    if (kind == "keep") g <- judge_group(g, v, v[["name"]], v[["description"]])
    if (kind == "hold") {
      g$status <- "held"
      g$held_reason <- "classification"
      g$classification_at <- v[["judged_at"]]
    }
    out <- c(out, list(g))
    from <- c(from, list(g$key))
    if (kind == "merge") merges[[length(merges) + 1L]] <- list(at = length(out), v = v)
  }
  gone <- rep(FALSE, length(out))
  went <- seq_along(out)
  by_id <- order(vapply(merges, function(m) as.integer(m$v[["subject_id"]]), 1L), method = "radix")
  for (m in merges[by_id]) {
    into <- as.integer(m$v[["merge_with"]])
    # A steady target is found by its id, following any merge it went through this run.
    own <- which(vapply(out, function(g) isTRUE(g$steady) && identical(g$subject_id, into), NA))
    if (length(own)) {
      to <- own[1]
      while (gone[to]) to <- went[to]
      if (to == m$at) next
    } else {
      target <- prev_members[[as.character(into)]]
      cand <- setdiff(which(!gone), m$at)
      if (is.null(target) || !length(cand)) next
      sc <- vapply(out[cand], function(g) jaccard(target, g$members), 0)
      ok <- sc >= settings$match_jaccard - 1e-12 & sc > 0
      if (!any(ok)) next
      keys <- vapply(out[cand], function(g) as.character(g$key), "")
      to <- cand[ok][order(-sc[ok], keys[ok], method = "radix")][1]
    }
    out[[to]]$members <- union(out[[to]]$members, out[[m$at]]$members)
    from[[to]] <- union(from[[to]], from[[m$at]])
    gone[m$at] <- TRUE
    went[m$at] <- to
  }
  out <- out[!gone]
  from <- from[!gone]
  if (nrow(strays) && length(out)) out <- place_strays(out, from, strays, edges, settings)
  out <- lapply(out, function(g) {
    g$members <- sort(unique(g$members), method = "radix")
    g
  })
  # A steady subject stays even with no members left.
  Filter(function(g) length(g$members) > 0L || isTRUE(g$steady), out)
}

# Subjects waiting for a classification: new names, name holds, and classifications their members have drifted from.
# `basis` is the subject_name_basis table, the baseline for judged split parts. With `clusters` (weekly_clusters),
# a split whose cluster has drifted but still carries it waits too, as the subject it split.
waiting_subjects <- function(subjects, members, basis, classifications, settings, clusters = NULL) {
  empty <- data.frame(subject_id = integer(), slug = character(), name = character(),
                      why = character(), stringsAsFactors = FALSE)
  if (is.null(subjects) || !nrow(subjects)) return(empty)
  st <- as.character(subjects$status)
  hr <- as.character(subjects$held_reason)
  why <- rep(NA_character_, nrow(subjects))
  why[st %in% "named"] <- "new name"
  name_hold <- st %in% "held" & !is.na(hr) & startsWith(hr, "name_")
  why[name_hold] <- paste0("held: ", hr[name_hold])
  check <- which(st %in% "judged" | (st %in% "held" & hr %in% "classification"))
  if (length(check)) {
    vid <- vapply(classifications, function(v) as.integer(v[["subject_id"]]), 1L)
    when <- vapply(classifications, function(v) as.character(v[["judged_at"]]), "")
    latest <- order(when, decreasing = TRUE, method = "radix")
    into <- vapply(classifications, function(v) {
      if (identical(v[["classification"]], "merge")) as.integer(v[["merge_with"]]) else NA_integer_
    }, 1L)
    settled <- function(v) setdiff(v[["members"]], v[["strays"]])
    cur <- members[!is.na(members$is_primary) & members$is_primary == 1 & is.na(members$left), , drop = FALSE]
    for (i in check) {
      id <- subjects$subject_id[i]
      k <- latest[vid[latest] == id][1]
      if (!is.na(k)) {
        # Its own classification without strays, plus every group merged into it.
        base <- Reduce(union, lapply(classifications[which(into == id)], settled), settled(classifications[[k]]))
      } else if (st[i] %in% "judged") {
        # A split part has no classification of its own, so it drifts from the members it was named from.
        base <- if (is.null(basis)) character() else basis$package[basis$subject_id %in% id]
        if (!length(base)) next
      } else {
        next
      }
      now <- cur$package[cur$subject_id == id]
      if (jaccard(base, now) < settings$drift_jaccard - 1e-12) why[i] <- "drifted"
    }
  }
  if (!is.null(clusters) && nrow(clusters)) {
    for (v in Filter(function(v) identical(v[["classification"]], "split"), classifications)) {
      i <- match(as.integer(v[["subject_id"]]), subjects$subject_id)
      if (is.na(i) || !st[i] %in% "retired") next
      parts <- vapply(v[["groups"]], function(g) as.character(g[["name"]]), "")
      if (!any(!st %in% "retired" & subjects$classification_at %in% v[["judged_at"]] & subjects$name %in% parts)) next
      j <- jaccard(split_cluster(v, clusters), v[["members"]])
      if (j >= settings$match_jaccard - 1e-12 && j < settings$drift_jaccard - 1e-12) why[i] <- "split drifted"
    }
  }
  keep <- !is.na(why)
  out <- data.frame(subject_id = as.integer(subjects$subject_id[keep]), slug = as.character(subjects$slug[keep]),
                    name = as.character(subjects$name[keep]), why = why[keep], stringsAsFactors = FALSE)
  out <- out[order(out$subject_id, method = "radix"), , drop = FALSE]
  rownames(out) <- NULL
  out
}
