# scripts/identity.R: Jaccard overlap, matching clusters to last run's subjects, and slugs.

# Jaccard overlap of two member vectors, 0 when both are empty.
jaccard <- function(a, b) {
  a <- unique(a)
  b <- unique(b)
  u <- length(union(a, b))
  if (u == 0L) return(0)
  length(intersect(a, b)) / u
}

# Whether a group under the entry size still carries a subject: it keeps min_keep_size packages and
# matches the subject's last members.
keeps_subject <- function(members, previous, settings) {
  length(members) >= settings$min_keep_size && jaccard(members, previous) >= settings$match_jaccard
}

# Greedy one to one match, best Jaccard first, ties by key then subject_id (bytewise).
match_to_previous <- function(group_members, prev_members, threshold) {
  keys <- names(group_members)
  ids <- names(prev_members)
  out <- data.frame(key = keys, subject_id = rep(NA_integer_, length(keys)),
                    jaccard = rep(NA_real_, length(keys)), stringsAsFactors = FALSE)
  pairs <- expand.grid(i = seq_along(keys), j = seq_along(ids), stringsAsFactors = FALSE)
  if (nrow(pairs) == 0L) return(out)
  pairs$score <- mapply(function(i, j) jaccard(group_members[[i]], prev_members[[j]]),
                        pairs$i, pairs$j)
  pairs <- pairs[pairs$score >= threshold & pairs$score > 0, , drop = FALSE]
  pairs <- pairs[order(-pairs$score, keys[pairs$i], ids[pairs$j], method = "radix"), ,
                 drop = FALSE]
  used_key <- rep(FALSE, length(keys))
  used_id <- rep(FALSE, length(ids))
  for (r in seq_len(nrow(pairs))) {
    i <- pairs$i[r]
    j <- pairs$j[r]
    if (used_key[i] || used_id[j]) next
    used_key[i] <- TRUE
    used_id[j] <- TRUE
    out$subject_id[i] <- as.integer(ids[j])
    out$jaccard[i] <- pairs$score[r]
  }
  out
}

# Lower-case hyphenated slug, with -2, -3 and so on when taken.
make_slug <- function(name, taken) {
  s <- gsub("^-+|-+$", "", gsub("[^a-z0-9]+", "-", tolower(name)))
  if (!nzchar(s)) s <- "subject"
  if (!s %in% taken) return(s)
  k <- 2L
  while (paste0(s, "-", k) %in% taken) k <- k + 1L
  paste0(s, "-", k)
}
