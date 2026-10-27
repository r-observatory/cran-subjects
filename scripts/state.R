# scripts/state.R: last run's tables, read fail-closed, and the retention check.

STATE_TABLES <- c("subjects", "subject_members", "subject_name_basis", "subject_task_views",
                  "collections", "collection_members", "collection_mentions",
                  "seen_packages", "pipeline_runs", "weekly_clusters", "subject_cluster_basis")
# States written before these tables existed lack them: the next weekly run clusters from scratch, and takes
# each subject's cluster basis from that run's clusters.
OPTIONAL_STATE_TABLES <- c("weekly_clusters", "subject_cluster_basis")

# Every state table as a data frame, NULL for a missing optional one. NULL path is a cold start;
# anything unreadable stops.
read_state <- function(path) {
  if (is.null(path)) return(NULL)
  if (!file.exists(path)) stop("state file not found: ", path, call. = FALSE)
  if (is.na(file.size(path)) || file.size(path) == 0) stop("state file is empty: ", path, call. = FALSE)
  con <- DBI::dbConnect(RSQLite::SQLite(), path, flags = RSQLite::SQLITE_RO)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  have <- DBI::dbListTables(con)
  missing <- setdiff(STATE_TABLES, c(have, OPTIONAL_STATE_TABLES))
  if (length(missing)) {
    stop("state file ", path, " is missing table ", paste(missing, collapse = ", "), call. = FALSE)
  }
  out <- lapply(STATE_TABLES, function(t) {
    if (t %in% have) DBI::dbGetQuery(con, sprintf('SELECT * FROM "%s"', t))
  })
  names(out) <- STATE_TABLES
  out
}

# Stops unless every previous subject and collection id survives and published slugs stay put.
check_retention <- function(prev, new) {
  if (is.null(prev)) return(invisible(TRUE))
  lost <- setdiff(prev$subjects$subject_id, new$subjects$subject_id)
  if (length(lost)) {
    stop("retention: subject_id ", paste(sort(lost), collapse = ", "), " missing from the new tables",
         call. = FALSE)
  }
  lost <- setdiff(prev$collections$collection_id, new$collections$collection_id)
  if (length(lost)) {
    stop("retention: collection_id ", paste(sort(lost), collapse = ", "), " missing from the new tables",
         call. = FALSE)
  }
  ps <- prev$subjects
  fixed <- as.character(ps$slug) != paste0("held-", ps$subject_id)
  now <- as.character(new$subjects$slug)[match(ps$subject_id[fixed], new$subjects$subject_id)]
  moved <- ps$subject_id[fixed][now != as.character(ps$slug[fixed])]
  if (length(moved)) {
    stop("retention: the published slug changed for subject_id ", paste(sort(moved), collapse = ", "),
         call. = FALSE)
  }
  invisible(TRUE)
}
