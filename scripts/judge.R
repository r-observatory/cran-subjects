#!/usr/bin/env Rscript
# scripts/judge.R: prepare the monthly judging queue and merge the answers into classifications/subjects.json.
#
# The /judge-subjects command answers the queue in a coding agent session. Nothing here calls a model.
.judge_file <- function() {
  for (i in rev(seq_len(sys.nframe()))) {
    of <- sys.frame(i)$ofile
    if (!is.null(of) && nzchar(of)) return(normalizePath(of))
  }
  a <- commandArgs(FALSE); f <- sub("^--file=", "", grep("^--file=", a, value = TRUE))
  if (length(f) == 1L && nzchar(f)) return(normalizePath(f)) else NA_character_
}
.judge_dir <- { tf <- .judge_file(); if (!is.na(tf)) dirname(tf) else "scripts" }
if (!exists("read_state", mode = "function")) {
  for (.f in c("config.R", "helpers.R", "inputs.R", "text.R", "graph.R", "identity.R", "naming.R",
               "classifications.R", "state.R")) {
    source(file.path(.judge_dir, .f))
  }
}

JUDGE_QUEUE <- "judge-queue.jsonl"
JUDGE_ANSWERS <- "judge-answers.jsonl"
JUDGE_INVALID <- "judge-invalid.jsonl"
JUDGE_IDS <- "judge-ids.json"

json_line <- function(x) as.character(jsonlite::toJSON(x, auto_unbox = TRUE, na = "null", null = "null"))

# SHA-256 of the sorted members joined by newlines.
members_fingerprint <- function(members) {
  f <- tempfile()
  on.exit(unlink(f), add = TRUE)
  writeBin(charToRaw(paste(sort(members, method = "radix"), collapse = "\n")), f)
  file_sha256(f)
}

safe_main_words <- function(name) if (is.null(name) || length(name) != 1L || is.na(name)) character() else main_words(name)

# Up to 5 other live subjects sharing a main word with this one, as "ID: NAME".
close_subjects <- function(subjects, id, words = NULL, top = 5L) {
  live <- subjects[!subjects$status %in% "retired" & !is.na(subjects$name) & subjects$subject_id != id, , drop = FALSE]
  mine <- safe_main_words(subjects$name[match(id, subjects$subject_id)])
  if (!length(mine) || !nrow(live)) return(character())
  w <- lapply(live$subject_id, function(i) if (is.null(words)) NULL else words[[as.character(i)]])
  for (k in seq_along(w)) if (is.null(w[[k]])) w[[k]] <- safe_main_words(live$name[k])
  overlap <- vapply(w, function(x) length(intersect(mine, x)), 1L)
  live <- live[overlap > 0L, , drop = FALSE]
  overlap <- overlap[overlap > 0L]
  if (!nrow(live)) return(character())
  o <- order(-overlap, live$subject_id, method = "radix")[seq_len(min(top, nrow(live)))]
  sprintf("%d: %s", live$subject_id[o], live$name[o])
}

# The naming prompt plus the current name and the groups with close names.
judge_prompt <- function(subject, members, inputs, edges, close_names, settings, gram_df = NULL) {
  titles <- stats::setNames(inputs$packages$title, inputs$packages$name)
  if (is.null(gram_df)) gram_df <- title_gram_df(titles)
  base <- name_prompt(members, inputs, edges, title_phrases(titles, members, gram_df), settings)
  nm <- subject$name
  paste0(base, sprintf("Current name: %s\nOther groups with close names: %s\n",
                       if (is.null(nm) || is.na(nm)) "(none)" else nm,
                       if (length(close_names)) paste(close_names, collapse = ", ") else "none"))
}

# Strays of the subject's latest classification that its raw cluster still holds. The weekly run matches a
# classification against the raw cluster, strays and all, so a new answer records them too.
carried_strays <- function(id, members, classifications, clusters) {
  mine <- Filter(function(v) identical(as.integer(v[["subject_id"]]), as.integer(id)), classifications)
  if (is.null(clusters) || !nrow(clusters) || !length(mine)) return(character())
  when <- vapply(mine, function(v) as.character(v[["judged_at"]]), "")
  v <- mine[[order(when, decreasing = TRUE, method = "radix")[1]]]
  k <- clusters$cluster[match(members, clusters$package)]
  k <- k[!is.na(k)]
  if (!length(k)) return(character())
  home <- as.integer(names(which.max(table(k))))
  sort(setdiff(intersect(as.character(unlist(v[["strays"]])), clusters$package[clusters$cluster == home]), members),
       method = "radix")
}

# The packages name_prompt showed, read back from its bullet lines.
listed_in_prompt <- function(prompt) {
  head <- sub("\n\nPhrases common in their titles.*$", "", prompt)
  lines <- strsplit(head, "\n", fixed = TRUE)[[1]]
  lines <- lines[startsWith(lines, "- ")]
  sub("^- ([^:]+): .*$", "\\1", lines)
}

judge_prepare <- function(state_path, inputs_path, out_dir, classifications_path, all = FALSE,
                          settings = default_settings()) {
  state <- read_state(state_path)
  if (is.null(state)) stop("judge needs the current state", call. = FALSE)
  inputs <- read_inputs(inputs_path)
  classifications <- read_classifications(classifications_path)
  subjects <- state$subjects
  members <- state$subject_members
  live <- subjects[!subjects$status %in% "retired", , drop = FALSE]
  todo <- if (all) live[order(live$subject_id), c("subject_id", "slug", "name"), drop = FALSE]
          else waiting_subjects(subjects, members, state$subject_name_basis, classifications, settings, state$weekly_clusters)
  open <- members[members$is_primary %in% 1L & is.na(members$left), , drop = FALSE]
  words <- stats::setNames(lapply(live$name, safe_main_words), as.character(live$subject_id))
  gram_df <- title_gram_df(stats::setNames(inputs$packages$title, inputs$packages$name))

  lines <- character()
  for (r in seq_len(nrow(todo))) {
    id <- todo$subject_id[r]
    # A drifted split is judged again as its whole cluster, so the answer is matched as the weekly run matches it.
    m <- if (isTRUE(todo$why[r] == "split drifted")) {
      split_cluster(Filter(function(v) identical(as.integer(v[["subject_id"]]), as.integer(id)), classifications)[[1]],
                    state$weekly_clusters)
    } else sort(open$package[open$subject_id == id], method = "radix")
    if (!length(m)) next
    edges <- dependency_edges(inputs$deps, m)
    nm <- subjects$name[match(id, subjects$subject_id)]
    prompt <- judge_prompt(list(subject_id = id, name = nm), m, inputs, edges,
                           close_subjects(subjects, id, words), settings, gram_df)
    lines <- c(lines, json_line(list(
      subject_id = id, slug = todo$slug[r], name = nm, members = I(m), fingerprint = members_fingerprint(m),
      carried = I(carried_strays(id, m, classifications, state$weekly_clusters)),
      listed = I(listed_in_prompt(prompt)), prompt = prompt)))
  }
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  writeLines(lines, file.path(out_dir, JUDGE_QUEUE), useBytes = TRUE)
  writeLines(json_line(I(as.integer(live$subject_id))), file.path(out_dir, JUDGE_IDS))
  for (f in c("judge-system.txt", "classification-schema.json")) {
    file.copy(file.path(.judge_dir, "prompts", f), file.path(out_dir, f), overwrite = TRUE)
  }
  invisible(file.path(out_dir, JUDGE_QUEUE))
}

read_json_lines <- function(path) {
  if (!file.exists(path)) stop("file not found: ", path, call. = FALSE)
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  lines[nzchar(trimws(lines))]
}

# Site text rules: no semicolon in a name or description, and neither starts with A or An.
text_errors <- function(x, what) {
  err <- character()
  opens_with_article <- function(s) tolower(sub("^\\s*([^[:space:]]*).*$", "\\1", s)) %in% c("a", "an")
  for (f in c("name", "description")) {
    s <- x[[f]]
    if (!is.character(s) || length(s) != 1L || is.na(s)) next
    if (grepl(";", s, fixed = TRUE)) err <- c(err, paste(what, f, "must not contain a semicolon"))
    if (opens_with_article(s)) err <- c(err, paste(what, f, "must not start with A or An"))
  }
  err
}

answer_errors <- function(e, q, ids) {
  err <- validate_classification(e, known_ids = ids)
  err <- c(err, text_errors(e, "the"))
  gs <- e[["groups"]]
  if (is.list(gs) && is.null(names(gs))) {
    for (i in seq_along(gs)) if (is.list(gs[[i]])) err <- c(err, text_errors(gs[[i]], sprintf("group %d", i)))
  }
  named <- c(unlist(lapply(if (is.list(gs) && is.null(names(gs))) gs else list(),
                           function(g) if (is.list(g)) g[["packages"]]), use.names = FALSE),
             if (is.character(e[["strays"]])) e[["strays"]])
  hidden <- setdiff(intersect(named, e[["members"]]), q$listed)
  if (length(hidden)) {
    err <- c(err, paste("packages not listed in the prompt:", paste(sort(hidden, method = "radix"), collapse = ", ")))
  }
  err
}

# Arrays stay arrays when written, even with one element.
classification_json <- function(v) {
  for (f in c("members", "strays")) if (!is.null(v[[f]])) v[[f]] <- I(as.character(v[[f]]))
  if (is.list(v[["groups"]])) {
    v[["groups"]] <- lapply(v[["groups"]], function(g) {
      g[["packages"]] <- I(as.character(g[["packages"]]))
      g
    })
  }
  v
}

judge_merge <- function(out_dir, classifications_path, model, today) {
  qlines <- read_json_lines(file.path(out_dir, JUDGE_QUEUE))
  queue <- lapply(qlines, jsonlite::fromJSON, simplifyVector = FALSE)
  queue <- lapply(queue, function(q) {
    q$members <- as.character(unlist(q$members))
    q$listed <- as.character(unlist(q$listed))
    q$carried <- as.character(unlist(q$carried))
    q
  })
  names(queue) <- vapply(queue, function(q) as.character(q$subject_id), "")
  ids <- as.integer(unlist(jsonlite::fromJSON(file.path(out_dir, JUDGE_IDS), simplifyVector = FALSE)))
  existing <- read_classifications(classifications_path)

  answers <- list()
  invalid <- list()
  for (line in read_json_lines(file.path(out_dir, JUDGE_ANSWERS))) {
    a <- tryCatch(jsonlite::fromJSON(line, simplifyVector = FALSE), error = function(e) NULL)
    id <- if (is.list(a) && !is.null(names(a))) a[["subject_id"]]
    if (!is.atomic(id) || length(id) != 1L || is.na(id)) {
      invalid[[length(invalid) + 1L]] <- list(subject_id = NA_integer_, errors = I("answer is not a JSON object with a single subject_id"),
                                              answer = line)
      next
    }
    answers[[as.character(a[["subject_id"]])]] <- normalise_classification(a)
  }

  valid <- list()
  for (key in names(answers)) {
    a <- answers[[key]]
    q <- queue[[key]]
    if (is.null(q)) {
      invalid[[length(invalid) + 1L]] <- list(subject_id = a[["subject_id"]], errors = I("subject_id is not in the queue"), answer = a)
      next
    }
    e <- list(subject_id = as.integer(q$subject_id), members = q$members, fingerprint = q$fingerprint,
              classification = a[["classification"]])
    for (f in c("name", "description", "merge_with", "groups", "strays")) if (!is.null(a[[f]])) e[[f]] <- a[[f]]
    e$judged_at <- today
    e$model <- model
    err <- answer_errors(e, q, ids)
    if (length(err)) {
      invalid[[length(invalid) + 1L]] <- list(subject_id = e$subject_id, errors = I(err), answer = a)
    } else {
      if (length(q$carried)) {
        e$members <- sort(union(e$members, q$carried), method = "radix")
        e$strays <- sort(union(as.character(unlist(e$strays)), q$carried), method = "radix")
        e$fingerprint <- members_fingerprint(e$members)
      }
      valid[[key]] <- e
    }
  }

  keep <- Filter(function(v) !as.integer(v[["subject_id"]]) %in% vapply(valid, function(e) e$subject_id, 1L), existing)
  all <- c(keep, unname(valid))
  all <- all[order(vapply(all, function(v) as.integer(v[["subject_id"]]), 1L), method = "radix")]
  jsonlite::write_json(lapply(all, classification_json), classifications_path, pretty = TRUE, auto_unbox = TRUE, null = "null")
  writeLines(vapply(invalid, json_line, ""), file.path(out_dir, JUDGE_INVALID), useBytes = TRUE)
  list(written = length(valid), invalid = length(invalid))
}

# ---- CLI ---------------------------------------------------------------------

JUDGE_USAGE <- paste("usage: Rscript scripts/judge.R prepare OUT_DIR [--state PATH] [--input PATH] [--all]",
                     "       Rscript scripts/judge.R merge OUT_DIR --model MODEL_ID", sep = "\n")

parse_judge_cli <- function(args) {
  if (length(args) < 2L || !args[1] %in% c("prepare", "merge") || startsWith(args[2], "--")) stop(JUDGE_USAGE, call. = FALSE)
  out <- list(command = args[1], out_dir = args[2], state = NULL, input = NULL, model = NULL, all = FALSE)
  rest <- args[-(1:2)]
  i <- 1L
  while (i <= length(rest)) {
    if (rest[i] == "--all" && out$command == "prepare") {
      out$all <- TRUE
      i <- i + 1L
      next
    }
    flag <- sub("^--", "", rest[i])
    ok <- if (out$command == "prepare") c("state", "input") else "model"
    if (!startsWith(rest[i], "--") || !flag %in% ok || i == length(rest)) stop(JUDGE_USAGE, call. = FALSE)
    out[[flag]] <- rest[i + 1L]
    i <- i + 2L
  }
  if (out$command == "merge" && (is.null(out$model) || !nzchar(out$model))) stop(JUDGE_USAGE, call. = FALSE)
  out
}

if (sys.nframe() == 0L) {
  REPO_ROOT <- dirname(normalizePath(.judge_dir))
  args <- parse_judge_cli(commandArgs(trailingOnly = TRUE))
  if (args$command == "prepare") {
    if (!exists("default_io", mode = "function")) source(file.path(.judge_dir, "update.R"))
    io <- default_io(list(input = args$input, previous = args$state))
    work <- tempfile("cran-subjects-judge-")
    dir.create(work, recursive = TRUE)
    state <- io$fetch_previous(work)
    if (is.null(state)) stop("no current state to judge: the current release was not found", call. = FALSE)
    inputs <- io$fetch_input(work)
    path <- judge_prepare(state, inputs, args$out_dir, repo_path(CLASSIFICATIONS_PATH), all = args$all)
    unlink(work, recursive = TRUE)
    n <- length(readLines(path, warn = FALSE))
    message(sprintf("%d subjects to judge, queue in %s", n, path))
  } else {
    res <- judge_merge(args$out_dir, repo_path(CLASSIFICATIONS_PATH), args$model, format(Sys.time(), "%Y-%m-%d", tz = "UTC"))
    message(sprintf("%d classifications written to %s, %d invalid", res$written, repo_path(CLASSIFICATIONS_PATH), res$invalid))
  }
}
