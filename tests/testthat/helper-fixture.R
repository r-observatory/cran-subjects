# Writes a small store with the real table and column names.
make_fixture_store <- function(path, n_per_subject = 30L, extra = list()) {
  if (file.exists(path)) unlink(path)
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  ddl <- c(
    "CREATE TABLE packages (name TEXT PRIMARY KEY, version TEXT, title TEXT, description TEXT,
       maintainer TEXT, maintainer_email TEXT, license TEXT, depends TEXT, imports TEXT,
       suggests TEXT, linking_to TEXT, needs_compilation TEXT, published TEXT, cran_url TEXT,
       first_published TEXT, is_archived INTEGER, updated_at TEXT, rev_dep_count INTEGER,
       dep_count INTEGER, suggests_rev_dep_count INTEGER, indirect_rev_dep_count INTEGER)",
    "CREATE TABLE cran_task_view_membership (view TEXT, package TEXT, added_date TEXT,
       removed_date TEXT, currently_member INTEGER, core INTEGER)",
    "CREATE TABLE vcs_signals_summary (package TEXT, origin TEXT, repo_id TEXT, topics TEXT)",
    "CREATE TABLE repo_package_links (repo_id TEXT, package TEXT, origin TEXT,
       first_seen TEXT, last_seen TEXT)",
    "CREATE TABLE vcs_repo_owner (repo_id TEXT, node_id TEXT, owner_login_current TEXT,
       owner_type TEXT, owner_node_id TEXT, name_with_owner_current TEXT, observed_on TEXT)",
    "CREATE TABLE cran_code_summary (package TEXT, version TEXT, loc_r INTEGER)"
  )
  for (s in ddl) DBI::dbExecute(con, s)

  n <- as.integer(n_per_subject)
  idx <- seq_len(n)
  nn <- sprintf("%02d", idx)
  pk <- function(name, title, description = title, maintainer = "Other Maintainer",
                 depends = NA, imports = NA, suggests = NA, archived = 0L) {
    data.frame(name = name, version = "1.0.0", title = title, description = description,
               maintainer = maintainer, depends = depends, imports = imports,
               suggests = suggests, is_archived = archived, stringsAsFactors = FALSE)
  }

  surv <- pk(sprintf("surv%s", nn), sprintf("Survival Analysis With Method %s", nn),
             "Fits hazard models for censored data and Kaplan-Meier curves.",
             sprintf("Surv Author %d", (idx - 1L) %% 6L + 1L),
             imports = ifelse(idx == 1L, NA, "surv01"))
  pal <- pk(sprintf("pal%s", nn), sprintf("Colour Palettes Inspired By %s", nn),
            "Provides a palette of colour scales with a chosen hue.",
            sprintf("Pal Author %d", (idx - 1L) %% 6L + 1L),
            imports = ifelse(idx == 1L, NA, "pal01"))

  hw <- "Hadley Wickham"
  gc <- "Gabor Csardi"
  coll <- rbind(
    pk("tidyverse", "Easily Install and Load the 'Tidyverse'", maintainer = hw,
       depends = "R (>= 3.3)", imports = "cli, dplyr (>= 1.1.0), ggplot2, tidyr"),
    pk("dplyr", "A Grammar of Data Manipulation", maintainer = hw),
    pk("ggplot2", "Create Elegant Data Visualisations", maintainer = hw),
    pk("tidyr", "Tidy Messy Data", maintainer = hw),
    pk("cli", "Helpers for Developing Command Line Interfaces", maintainer = gc),
    pk("glue", "Interpreted String Literals", maintainer = gc),
    pk("janitor", "Simple Tools for Examining and Cleaning Dirty Data",
       "Cleans data and works with the 'tidyverse'.", "Sam Firke"),
    pk("tidytext", "Text Mining Using Tidy Tools", maintainer = "Julia Silge"),
    pk("bigloc", "Install and Load the Bigloc Suite", maintainer = "Big Maintainer",
       imports = "bigloc.core, bigloc.io"),
    pk("bigloc.core", "Core of the Bigloc Suite", maintainer = "Big Maintainer"),
    pk("bigloc.io", "Input and Output for the Bigloc Suite", maintainer = "Big Maintainer"),
    pk("librarian", "Install, Update, Load Packages from CRAN, 'GitHub', and 'Bioconductor'",
       maintainer = "Yi Fan", imports = "glue"),
    pk("dartRverse", "Install and Load the 'dartRverse' Suits of Packages",
       maintainer = "Bernd Gruber", imports = "cli",
       suggests = "dartR.base, dartR.sim, dartR.popgen"),
    pk("dartR.base", "Base Functions for dartR", maintainer = "Bernd Gruber"),
    pk("dartR.sim", "Simulation Functions for dartR", maintainer = "Bernd Gruber"),
    pk("dartR.popgen", "Population Genetics Functions for dartR", maintainer = "Bernd Gruber"),
    pk("pharmaverse", "Navigate 'Pharmaverse'", maintainer = "Pharma Maintainer",
       imports = "cli"),
    pk("admiral", "ADaM in R Asset Library", maintainer = "Pharma Maintainer",
       imports = "admiraldev"),
    pk("admiraldev", "Utility Functions for admiral", maintainer = "Pharma Maintainer"),
    pk("pharmaversesdtm", "SDTM Test Data for the Pharmaverse", maintainer = "Pharma Maintainer",
       imports = "admiral"),
    pk("foo1", "Foo One", maintainer = "Foo Maintainer", imports = "foo2"),
    pk("foo2", "Foo Two", maintainer = "Foo Maintainer", imports = "foo3"),
    pk("foo3", "Foo Three", maintainer = "Foo Maintainer", imports = "foo1"),
    pk("oldpkg", "Archived Package", archived = 1L)
  )
  pkgs <- rbind(surv, pal, coll)
  DBI::dbWriteTable(con, "packages", pkgs, append = TRUE)

  # Task views: surv01 to surv(n-1) are current, the last one was removed.
  tv <- data.frame(view = "Survival", package = surv$name,
                   added_date = "2020-01-01", removed_date = NA_character_,
                   currently_member = 1L, core = as.integer(idx == 1L))
  tv$removed_date[n] <- "2025-01-01"
  tv$currently_member[n] <- 0L
  DBI::dbWriteTable(con, "cran_task_view_membership", tv, append = TRUE)

  # Repos and owners.
  org <- function(pkgs, owner, type) {
    data.frame(package = pkgs, owner = owner, type = type, stringsAsFactors = FALSE)
  }
  repos <- rbind(
    org(c("tidyverse", "dplyr", "ggplot2", "tidyr", "cli", "glue"), "tidyverse", "Organization"),
    org(c("admiral", "admiraldev", "pharmaversesdtm"), "pharmaverse", "Organization"),
    org(c("foo1", "foo2", "foo3"), "fooverse", "User")
  )
  repos$repo_id <- sprintf("github.com/%s/%s", repos$owner, repos$package)
  DBI::dbWriteTable(con, "repo_package_links",
                    data.frame(repo_id = repos$repo_id, package = repos$package,
                               origin = "cran", first_seen = "2026-01-01",
                               last_seen = "2026-10-20"), append = TRUE)
  DBI::dbWriteTable(con, "vcs_repo_owner",
                    data.frame(repo_id = repos$repo_id,
                               node_id = paste0("R_", repos$package),
                               owner_login_current = repos$owner, owner_type = repos$type,
                               owner_node_id = paste0("O_", repos$owner),
                               name_with_owner_current = paste(repos$owner, repos$package, sep = "/"),
                               observed_on = "2026-10-20"), append = TRUE)

  # Topics.
  vs <- data.frame(package = c(repos$package, "tidytext"), origin = "cran",
                   repo_id = c(repos$repo_id, "github.com/juliasilge/tidytext"),
                   topics = c(rep(NA_character_, nrow(repos)), "tidyverse,text-mining"))
  DBI::dbWriteTable(con, "vcs_signals_summary", vs, append = TRUE)

  # Code size.
  loc <- data.frame(package = pkgs$name, version = pkgs$version, loc_r = 100L)
  loc$loc_r[loc$package == "tidyverse"] <- 181L
  loc$loc_r[loc$package == "bigloc"] <- 6690L
  loc$loc_r[loc$package == "dartRverse"] <- 300L
  loc$loc_r[loc$package == "pharmaverse"] <- 120L
  DBI::dbWriteTable(con, "cran_code_summary", loc, append = TRUE)

  for (tbl in names(extra)) DBI::dbWriteTable(con, tbl, extra[[tbl]], append = TRUE)
  invisible(path)
}
