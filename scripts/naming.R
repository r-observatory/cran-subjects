# scripts/naming.R: name a subject with a local open model, and check names by rule.

NAME_STOP <- c("a", "an", "and", "the", "of", "for", "in", "on", "to", "with", "from", "by", "via",
               "using", "or", "its", "into", "at", "as", "r", "package", "tools", "tool", "functions",
               "interface", "based", "methods", "analysis", "data", "models", "model", "estimation",
               "is", "are")
NAME_JOINERS <- c("and", "for", "of", "with", "in", "the")

# Grams of 1 to 3 words from one title: names are lower-case keys, values surface forms.
title_grams <- function(title) {
  t <- gsub("['\"`]", "", if (is.na(title)) "" else title)
  w <- regmatches(t, gregexpr("[A-Za-z][A-Za-z0-9-]*", t))[[1]]
  out <- character()
  for (n in 1:3) {
    for (i in seq_len(max(0L, length(w) - n + 1L))) {
      g <- w[i:(i + n - 1L)]
      low <- tolower(g)
      if (low[1] %in% NAME_STOP || low[n] %in% NAME_STOP) next
      if (n == 1L && nchar(low[1]) < 4L) next
      key <- paste(low, collapse = " ")
      if (!key %in% names(out)) out[key] <- paste(g, collapse = " ")
    }
  }
  out
}

# Number of titles holding each gram.
title_gram_df <- function(titles) {
  keys <- unlist(lapply(titles, function(t) names(title_grams(t))), use.names = FALSE)
  if (!length(keys)) return(integer())
  tb <- table(keys)
  setNames(as.integer(tb), names(tb))
}

# Up to `top` phrases the members' titles share, as surface forms, best first.
title_phrases <- function(titles, members, df, top = 3L) {
  size <- length(members)
  pg <- lapply(titles[intersect(members, names(titles))], title_grams)
  keys <- unlist(lapply(pg, names), use.names = FALSE)
  if (!length(keys)) return(character())
  cnt <- table(keys)
  v <- setNames(as.integer(cnt), names(cnt))
  v <- v[v >= max(3, 0.06 * size)]
  if (!length(v)) return(character())
  dfk <- ifelse(is.na(df[names(v)]), v, df[names(v)])
  n_words <- lengths(strsplit(names(v), " ", fixed = TRUE))
  score <- (v / size) * (v / dfk) * (1 + 0.35 * (n_words - 1))
  o <- order(-score, names(v), decreasing = c(FALSE, TRUE), method = "radix")
  ranked <- names(v)[o]
  first <- ranked[1]
  longer <- ranked[vapply(ranked, function(k) k != first && grepl(first, k, fixed = TRUE), NA) &
                     v[ranked] >= 0.6 * v[[first]]]
  if (length(longer)) {
    lo <- order(-v[longer], longer, decreasing = c(FALSE, TRUE), method = "radix")
    ranked <- c(longer[lo][1], ranked)
  }
  picks <- character()
  for (k in ranked) {
    if (any(vapply(picks, function(p) grepl(p, k, fixed = TRUE) || grepl(k, p, fixed = TRUE), NA))) next
    picks <- c(picks, k)
    if (length(picks) == top) break
  }
  vapply(picks, function(k) {
    s <- unlist(lapply(pg, function(g) g[k]), use.names = FALSE)
    s <- s[!is.na(s)]
    tb <- table(s)
    names(tb)[order(-as.integer(tb), names(tb), method = "radix")][1]
  }, "", USE.NAMES = FALSE)
}

short_text <- function(s, n = 170L) {
  s <- trimws(gsub("\\s+", " ", gsub("<[^>]*>", "", s)))
  if (nchar(s) <= n) return(s)
  paste0(sub("\\s+\\S*$", "", substr(s, 1, n)), " ...")
}

name_prompt <- function(members, inputs, edges, phrases, settings) {
  size <- length(members)
  deg <- setNames(numeric(size), members)
  if (nrow(edges)) {
    e <- edges[edges$from %in% members & edges$to %in% members, , drop = FALSE]
    for (i in seq_len(nrow(e))) {
      deg[e$from[i]] <- deg[e$from[i]] + e$weight[i]
      deg[e$to[i]] <- deg[e$to[i]] + e$weight[i]
    }
  }
  ord <- order(-deg, names(deg), method = "radix")
  shown <- names(deg)[ord][seq_len(min(settings$name_members, size))]
  pk <- inputs$packages
  lines <- vapply(shown, function(n) {
    i <- match(n, pk$name)
    title <- if (is.na(i) || is.na(pk$title[i])) "" else pk$title[i]
    desc <- if (is.na(i) || is.na(pk$description[i])) "" else short_text(pk$description[i])
    if (nzchar(desc)) sprintf("- %s: %s. %s", n, title, desc) else sprintf("- %s: %s.", n, title)
  }, "", USE.NAMES = FALSE)
  v <- inputs$views
  common <- tapply(v$package %in% members, v$view, sum)
  common <- common[common > 0]
  ov <- if (length(common)) {
    o <- order(-as.integer(common), names(common), method = "radix")[seq_len(min(3L, length(common)))]
    paste(sprintf("%s (%d in common)", names(common)[o], as.integer(common)[o]), collapse = ", ")
  } else "none"
  ph <- if (length(phrases)) paste(phrases, collapse = " / ") else "(none)"
  paste0(sprintf("These %d R packages were placed together by a clustering of CRAN. Here are the %d most central, with their titles and descriptions.\n\n",
                 size, length(shown)),
         paste(lines, collapse = "\n"),
         sprintf("\n\nPhrases common in their titles: %s\nCRAN task views with the most overlap: %s\n", ph, ov))
}

# Ask the local model; NULL on any failure.
llama_name <- function(system_prompt, user_prompt, llama_bin, model_path, schema_path, gpu = FALSE) {
  sysf <- tempfile(fileext = ".txt")
  on.exit(unlink(sysf), add = TRUE)
  writeLines(system_prompt, sysf, useBytes = TRUE)
  args <- c("-m", model_path, "-sysf", sysf, "-p", user_prompt, "-st", "-jf", schema_path,
            "-rea", "off", "--temp", "0", "-n", "300", "-c", "8192", "--no-display-prompt",
            "--log-disable", "--no-jinja",
            if (gpu) c("-ngl", "99") else c("-ngl", "0", "-t", "4"))
  out <- tryCatch(
    suppressWarnings(system2(llama_bin, shQuote(args), stdout = TRUE, stderr = FALSE,
                             stdin = "/dev/null", timeout = 900)),
    error = function(e) NULL)
  if (is.null(out) || !is.null(attr(out, "status"))) return(NULL)
  txt <- paste(out, collapse = "\n")
  m <- regmatches(txt, regexpr("\\{.*\\}", txt))
  if (!length(m)) return(NULL)
  ans <- tryCatch(jsonlite::fromJSON(m, simplifyVector = FALSE), error = function(e) NULL)
  if (!is.list(ans) || !is.character(ans$name) || !is.character(ans$description)) return(NULL)
  list(name = ans$name, description = ans$description)
}

clean_name <- function(name, member_names) {
  s <- gsub("[\"“”`]", "", name)
  s <- trimws(gsub("^'+|'+$", "", trimws(s)))
  s <- trimws(sub("\\.+$", "", s))
  w <- strsplit(s, "\\s+")[[1]]
  w <- w[nzchar(w)]
  if (!length(w)) return("")
  keep <- vapply(w, function(x) {
    x %in% member_names || grepl("^[A-Z][^A-Za-z]*$", x) ||
      any(nchar(gsub("[^A-Z]", "", strsplit(x, "-", fixed = TRUE)[[1]])) >= 2L)
  }, NA)
  w[!keep] <- tolower(w[!keep])
  while (length(w) && tolower(w[length(w)]) %in% GENERIC_TAIL_WORDS) {
    w <- w[-length(w)]
    keep <- keep[-length(keep)]
  }
  if (!length(w)) return("")
  if (!keep[1]) {
    w[1] <- paste0(toupper(substr(w[1], 1, 1)), substring(w[1], 2))
  }
  paste(w, collapse = " ")
}

main_words <- function(name) {
  w <- strsplit(tolower(name), "[^a-z0-9]+")[[1]]
  w <- w[nzchar(w)]
  w[!w %in% c(GENERIC_NAME_WORDS, NAME_JOINERS)]
}

is_generic_name <- function(name) length(main_words(name)) == 0L

near_duplicate_names <- function(names) {
  mw <- lapply(names, main_words)
  vapply(seq_along(mw), function(i) {
    if (!length(mw[[i]])) return(FALSE)
    any(vapply(seq_along(mw)[-i], function(j) {
      all(mw[[i]] %in% mw[[j]]) || (length(mw[[j]]) > 0L && all(mw[[j]] %in% mw[[i]]))
    }, NA))
  }, NA)
}
