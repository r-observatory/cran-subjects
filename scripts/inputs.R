# scripts/inputs.R: reads the packages, dependencies, topics, owners and task views.

DEP_TYPES <- c(depends = "depends", imports = "imports",
               suggests = "suggests", linking_to = "linking_to")

# Package names from a DESCRIPTION dependency field, without versions or R.
parse_dep_field <- function(x) {
  if (length(x) == 0L || all(is.na(x))) return(character())
  x <- paste(x[!is.na(x)], collapse = ",")
  parts <- trimws(gsub("\\([^)]*\\)", "", strsplit(x, ",", fixed = TRUE)[[1]]))
  parts <- gsub("\\s+", "", parts)
  parts[nzchar(parts) & parts != "R"]
}

# Returns list(packages, deps, topics, owners, task_views) for current CRAN packages.
read_inputs <- function(db_path) {
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path, flags = RSQLite::SQLITE_RO)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  packages <- DBI::dbGetQuery(con, "
    SELECT p.*, c.loc_r AS loc_r
      FROM packages p
      LEFT JOIN cran_code_summary c ON c.package = p.name AND c.version = p.version
     WHERE COALESCE(p.is_archived, 0) = 0
     ORDER BY p.name")
  packages <- packages[!duplicated(packages$name), , drop = FALSE]

  rows <- lapply(names(DEP_TYPES), function(type) {
    to <- lapply(packages[[DEP_TYPES[[type]]]], parse_dep_field)
    n <- lengths(to)
    data.frame(from = rep(packages$name, n), to = unlist(to, use.names = FALSE),
               type = rep(type, sum(n)), stringsAsFactors = FALSE)
  })
  deps <- do.call(rbind, rows)

  vcs <- DBI::dbGetQuery(con, "
    SELECT package, topics FROM vcs_signals_summary
     WHERE origin = 'cran' AND topics IS NOT NULL AND topics <> ''")
  topic_list <- strsplit(vcs$topics, ",", fixed = TRUE)
  topics <- data.frame(package = rep(vcs$package, lengths(topic_list)),
                       topic = trimws(unlist(topic_list, use.names = FALSE)),
                       stringsAsFactors = FALSE)
  topics <- unique(topics[nzchar(topics$topic) & topics$package %in% packages$name, ])

  owners <- DBI::dbGetQuery(con, "
    SELECT l.package, l.repo_id, o.owner_login_current AS owner_login,
           o.owner_type, o.observed_on
      FROM repo_package_links l
      JOIN vcs_repo_owner o ON o.repo_id = l.repo_id
     WHERE l.repo_id LIKE 'github.com/%'
     ORDER BY l.package, l.repo_id, o.observed_on DESC")
  owners <- owners[!duplicated(owners[c("package", "repo_id")]) &
                     owners$package %in% packages$name, c("package", "repo_id", "owner_login", "owner_type")]
  rownames(owners) <- NULL

  task_views <- DBI::dbGetQuery(con, "
    SELECT view, package, core FROM cran_task_view_membership
     WHERE currently_member = 1")
  task_views <- task_views[task_views$package %in% packages$name, ]

  list(packages = packages, deps = deps, topics = topics,
       owners = owners, task_views = task_views)
}
