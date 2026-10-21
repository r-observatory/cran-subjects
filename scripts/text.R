# scripts/text.R: text links between packages from titles, descriptions and topics.

load_stopwords <- function(path = "scripts/stopwords.txt") {
  if (!file.exists(path) && file.exists(file.path("..", "..", path))) path <- file.path("..", "..", path)
  w <- trimws(readLines(path, warn = FALSE))
  unique(w[nzchar(w)])
}

clean_text <- function(x) {
  x <- ifelse(is.na(x), "", as.character(x))
  x <- gsub("<[^>]*>", " ", x, perl = TRUE)
  x <- gsub("(https?://|doi:|arxiv:)\\S+", " ", x, perl = TRUE, ignore.case = TRUE)
  x <- gsub("[\"'`]", " ", x, perl = TRUE)
  tolower(x)
}

topic_text <- function(topics) {
  topics <- trimws(tolower(topics[!is.na(topics)]))
  topics <- topics[nzchar(topics) & !(topics %in% GENERIC_TOPICS)]
  paste(sort(unique(gsub("-", " ", topics, fixed = TRUE))), collapse = " ")
}

build_documents <- function(packages, topics, title_weight) {
  by_pkg <- split(topics$topic, topics$package)
  docs <- vapply(seq_len(nrow(packages)), function(i) {
    title <- clean_text(packages$title[i])
    paste(c(rep(title, title_weight), clean_text(packages$description[i]),
            clean_text(topic_text(by_pkg[[packages$name[i]]]))), collapse = " ")
  }, character(1))
  setNames(docs, packages$name)
}

# Unigrams then bigrams of adjacent kept tokens.
tokenize <- function(doc, stopwords) {
  tok <- regmatches(tolower(doc), gregexpr("[a-z][a-z0-9]+", tolower(doc), perl = TRUE))[[1]]
  tok <- tok[!(tok %in% stopwords) & !(tok %in% WORDING_WORDS)]
  n <- length(tok)
  if (n < 2L) return(tok)
  c(tok, paste(tok[-n], tok[-1L]))
}

tfidf_matrix <- function(docs, stopwords, min_df, max_df) {
  n <- length(docs)
  terms <- lapply(unname(docs), tokenize, stopwords = stopwords)
  df <- table(unlist(lapply(terms, unique), use.names = FALSE))
  keep <- names(df)[df >= min_df & df <= max_df * n]
  keep <- sort(keep, method = "radix")
  rows <- rep(seq_len(n), lengths(terms))
  cols <- match(unlist(terms, use.names = FALSE), keep)
  ok <- !is.na(cols)
  X <- Matrix::sparseMatrix(i = rows[ok], j = cols[ok], x = 1, dims = c(n, length(keep)),
                            dimnames = list(names(docs), keep))
  X <- methods::as(X, "generalMatrix")
  if (length(X@x) > 0L) {
    X@x <- 1 + log(X@x)
    idf <- log((1 + n) / (1 + as.numeric(df[keep]))) + 1
    X <- X %*% Matrix::Diagonal(x = idf)
    len <- sqrt(Matrix::rowSums(X^2))
    len[len == 0] <- 1
    X <- Matrix::Diagonal(x = 1 / len) %*% X
  }
  dimnames(X) <- list(names(docs), keep)
  methods::as(X, "CsparseMatrix")
}

embed <- function(X, dim, seed) {
  set.seed(seed)
  s <- irlba::irlba(X, nv = min(dim, min(dim(X)) - 1L))
  Z <- s$u %*% diag(s$d, nrow = length(s$d))
  len <- sqrt(rowSums(Z^2))
  len[len == 0] <- 1
  Z <- Z / len
  rownames(Z) <- rownames(X)
  Z
}

knn_edges <- function(Z, k, min_sim, chunk = 2000L) {
  n <- nrow(Z)
  nm <- if (is.null(rownames(Z))) as.character(seq_len(n)) else rownames(Z)
  rank <- match(nm, sort(nm, method = "radix"))
  k <- min(k, n - 1L)
  ii <- jj <- integer(0)
  ww <- numeric(0)
  for (start in seq(1L, n, by = chunk)) {
    idx <- start:min(start + chunk - 1L, n)
    S <- Z[idx, , drop = FALSE] %*% t(Z)
    S[cbind(seq_along(idx), idx)] <- -Inf
    for (r in seq_along(idx)) {
      top <- order(S[r, ], decreasing = TRUE)[seq_len(k)]
      top <- top[S[r, top] >= min_sim]
      ii <- c(ii, rep(idx[r], length(top)))
      jj <- c(jj, top)
      ww <- c(ww, S[r, top])
    }
  }
  lo <- ifelse(rank[ii] < rank[jj], ii, jj)
  hi <- ifelse(rank[ii] < rank[jj], jj, ii)
  if (length(lo) == 0L) {
    return(data.frame(from = character(0), to = character(0), weight = numeric(0),
                      stringsAsFactors = FALSE))
  }
  key <- paste(lo, hi)
  first <- !duplicated(key)
  w <- rowsum(ww, key, reorder = FALSE)
  out <- data.frame(from = nm[lo[first]], to = nm[hi[first]],
                    weight = as.numeric(w[key[first], 1]), stringsAsFactors = FALSE)
  out[order(rank[match(out$from, nm)], rank[match(out$to, nm)]), ]
}
