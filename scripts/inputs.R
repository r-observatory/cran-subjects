# scripts/inputs.R: reads the packages, dependencies, topics, owners and views.

DEP_FIELDS <- c(Depends = "depends", Imports = "imports",
                LinkingTo = "linking_to", Suggests = "suggests")

# Package names from a DESCRIPTION dependency field, without versions or R.
parse_dep_field <- function(x) {
  if (length(x) == 0L || all(is.na(x))) return(character())
  x <- paste(x[!is.na(x)], collapse = ",")
  parts <- trimws(gsub("\\([^)]*\\)", "", strsplit(x, ",", fixed = TRUE)[[1]]))
  parts <- gsub("\\s+", "", parts)
  parts[nzchar(parts) & parts != "R"]
}

# Returns list(packages, deps, topics, owners, views) for current CRAN packages.
read_inputs <- function(db_path) {
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path, flags = RSQLite::SQLITE_RO)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  base <- DBI::dbGetQuery(con, "
    SELECT name, title, description, maintainer, version, first_published,
           depends, imports, linking_to, suggests
      FROM packages
     WHERE COALESCE(is_archived, 0) = 0
     ORDER BY name")
  base <- base[!duplicated(base$name), , drop = FALSE]

  code <- DBI::dbGetQuery(con, "SELECT package, version, loc_r FROM cran_code_summary")
  # Row matching the version, else the newest row, else NA.
  code <- code[order(code$package, numeric_version(code$version, strict = FALSE),
                     na.last = FALSE), ]
  newest <- code[!duplicated(code$package, fromLast = TRUE), ]
  exact_key <- paste(code$package, code$version)
  loc_exact <- code$loc_r[match(paste(base$name, base$version), exact_key)]
  loc_newest <- newest$loc_r[match(base$name, newest$package)]
  base$loc_r <- ifelse(is.na(loc_exact), loc_newest, loc_exact)

  packages <- base[c("name", "title", "description", "maintainer",
                     "version", "first_published", "loc_r")]
  rownames(packages) <- NULL

  rows <- lapply(names(DEP_FIELDS), function(type) {
    to <- lapply(base[[DEP_FIELDS[[type]]]], parse_dep_field)
    n <- lengths(to)
    data.frame(from = rep(base$name, n), to = unlist(to, use.names = FALSE),
               type = rep(type, sum(n)), stringsAsFactors = FALSE)
  })
  deps <- do.call(rbind, rows)
  deps <- deps[deps$to %in% packages$name, , drop = FALSE]
  rownames(deps) <- NULL

  vcs <- DBI::dbGetQuery(con, "
    SELECT package, topics FROM vcs_signals_summary
     WHERE origin = 'cran' AND topics IS NOT NULL AND topics <> ''")
  topic_list <- strsplit(vcs$topics, ",", fixed = TRUE)
  topics <- data.frame(package = rep(vcs$package, lengths(topic_list)),
                       topic = tolower(trimws(unlist(topic_list, use.names = FALSE))),
                       stringsAsFactors = FALSE)
  topics <- unique(topics[nzchar(topics$topic) & topics$package %in% packages$name, ])
  rownames(topics) <- NULL

  owners <- DBI::dbGetQuery(con, "
    SELECT l.package, l.repo_id, o.owner_login_current AS owner,
           o.owner_type, o.observed_on
      FROM repo_package_links l
      JOIN vcs_repo_owner o ON o.repo_id = l.repo_id
     WHERE l.repo_id LIKE 'github.com/%'
     ORDER BY l.package, l.repo_id, o.observed_on DESC")
  owners <- owners[!duplicated(owners[c("package", "repo_id")]) &
                     owners$package %in% packages$name, ]
  owners <- unique(owners[c("package", "owner", "owner_type")])
  rownames(owners) <- NULL

  views <- DBI::dbGetQuery(con, "
    SELECT view, package FROM cran_task_view_membership WHERE currently_member = 1")
  views <- unique(views[views$package %in% packages$name, ])
  rownames(views) <- NULL

  list(packages = packages, deps = deps, topics = topics, owners = owners, views = views)
}
