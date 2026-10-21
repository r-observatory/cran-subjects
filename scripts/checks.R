# scripts/checks.R: maintainer checks for candidate subjects.

# Trim, collapse whitespace and case-fold maintainer names.
normalise_maintainer <- function(x) {
  x <- tolower(gsub("\\s+", " ", trimws(x)))
  x[!nzchar(x)] <- NA_character_
  x
}

# TRUE when the members have enough distinct maintainers and no one holds too large a share.
maintainer_check <- function(members, maintainers, settings) {
  m <- normalise_maintainer(maintainers[members])
  m <- m[!is.na(m)]
  if (length(m) == 0L) return(FALSE)
  counts <- table(m)
  if (length(counts) < settings$min_maintainers) return(FALSE)
  max(counts) / length(m) <= settings$max_maintainer_share
}
