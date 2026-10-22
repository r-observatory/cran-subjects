# scripts/reasons.R: the plain fact behind each member's placement in a subject.

# Text for one reason. NA kinds give NA.
reason_text <- function(kind, value) {
  templates <- c(uses = "Uses %s", used_by = "Used by %s",
                 title = "Its title names %s", topic = "Repository topic: %s",
                 words = "Its description mentions %s")
  out <- rep(NA_character_, length(kind))
  ok <- !is.na(kind) & kind %in% names(templates)
  out[ok] <- sprintf(templates[kind[ok]], as.character(value)[ok])
  out
}

# The candidate with the highest score, ties broken by name bytewise.
pick_best <- function(candidates, score) {
  candidates <- unique(candidates)
  if (length(candidates) == 0L) return(NA_character_)
  candidates[order(-score[candidates], candidates, method = "radix")][1]
}

# One row per member, using the first rule that applies: uses, used by, title, topic, words.
# `words` is an optional named vector from package to description_words() value.
placement_reasons <- function(members, deps, titles, phrase, topics, words = NULL) {
  members <- as.character(members)
  if (is.null(deps)) deps <- data.frame(from = character(0), to = character(0), type = character(0))
  if (is.null(topics)) topics <- data.frame(package = character(0), topic = character(0))

  deps <- deps[deps$type %in% c("Depends", "Imports", "LinkingTo"), c("from", "to"), drop = FALSE]
  deps <- deps[deps$from %in% members & deps$to %in% members & deps$from != deps$to, , drop = FALSE]
  deps <- unique(data.frame(from = as.character(deps$from), to = as.character(deps$to)))

  users <- setNames(integer(length(members)), members)
  counts <- table(deps$to)
  users[names(counts)] <- as.integer(counts)

  tp <- data.frame(package = as.character(topics$package), topic = as.character(topics$topic))
  tp <- unique(tp[tp$package %in% members & !is.na(tp$topic) &
                 !(tolower(tp$topic) %in% GENERIC_TOPICS), , drop = FALSE])
  shared <- table(tp$topic)
  shared_count <- setNames(as.integer(shared), names(shared))
  shared_topics <- names(shared_count)[shared_count >= 3]

  lower_phrase <- if (is.na(phrase)) NA_character_ else tolower(phrase)
  lower_titles <- tolower(titles[members])

  kind <- rep(NA_character_, length(members))
  value <- rep(NA_character_, length(members))
  for (i in seq_along(members)) {
    m <- members[i]
    uses <- pick_best(deps$to[deps$from == m], users)
    used_by <- pick_best(deps$from[deps$to == m], users)
    topic <- pick_best(tp$topic[tp$package == m & tp$topic %in% shared_topics], shared_count)
    title_hit <- !is.na(lower_phrase) && !is.na(lower_titles[i]) &&
      grepl(lower_phrase, lower_titles[i], fixed = TRUE)
    if (!is.na(uses)) {
      kind[i] <- "uses"
      value[i] <- uses
    } else if (!is.na(used_by)) {
      kind[i] <- "used_by"
      value[i] <- used_by
    } else if (title_hit) {
      kind[i] <- "title"
      value[i] <- phrase
    } else if (!is.na(topic)) {
      kind[i] <- "topic"
      value[i] <- topic
    } else if (!is.null(words) && m %in% names(words) && !is.na(words[[m]])) {
      kind[i] <- "words"
      value[i] <- words[[m]]
    }
  }
  data.frame(package = members, reason_kind = kind, reason_value = value,
             stringsAsFactors = FALSE, row.names = NULL)
}

# Stems of each word: lower case, and for words longer than 3 letters also without a final "s" or "es",
# so "estimates" meets "estimate" and "errors" meets "error".
word_stems <- function(words) {
  lapply(tolower(words), function(w) if (nchar(w) > 3L) unique(c(w, sub("s$", "", w), sub("es$", "", w))) else w)
}

# TRUE when every word of term a shares a stem with some word of term b.
stems_within <- function(a, b) {
  all(vapply(a, function(sa) any(vapply(b, function(sb) any(sa %in% sb), NA)), NA))
}

# For each member, "A and B": its two strongest TF-IDF terms among the subject's top terms
# (summed over the primary members) that its own description uses, neither nesting in the other
# by word stems. Members with none are left out.
description_words <- function(X, primary, members, descriptions, stopwords, top = 20L) {
  none <- stats::setNames(character(), character())
  primary <- intersect(primary, rownames(X))
  members <- intersect(members, rownames(X))
  if (!length(primary) || !length(members) || !ncol(X)) return(none)
  w <- Matrix::colSums(X[primary, , drop = FALSE])
  w <- w[w > 0]
  if (!length(w)) return(none)
  terms <- names(w)[order(-w, names(w), method = "radix")][seq_len(min(top, length(w)))]
  sub <- as.matrix(X[members, terms, drop = FALSE])
  out <- vapply(seq_along(members), function(r) {
    v <- stats::setNames(as.numeric(sub[r, , drop = FALSE]), terms)
    v <- v[v > 0]
    if (length(v)) {
      d <- descriptions[members[r]]
      v <- v[names(v) %in% tokenize(clean_text(if (is.na(d)) "" else d), stopwords)]
    }
    if (!length(v)) return(NA_character_)
    # Take the strongest two, skipping a term whose stems nest in a picked one or hold it.
    picked <- character()
    for (t in names(v)[order(-v, names(v), method = "radix")]) {
      st <- word_stems(strsplit(t, " ", fixed = TRUE)[[1]])
      nests <- vapply(picked, function(p) {
        sp <- word_stems(strsplit(p, " ", fixed = TRUE)[[1]])
        stems_within(st, sp) || stems_within(sp, st)
      }, NA)
      if (!any(nests)) picked <- c(picked, t)
      if (length(picked) == 2L) break
    }
    paste(picked, collapse = " and ")
  }, "")
  names(out) <- members
  out[!is.na(out)]
}
