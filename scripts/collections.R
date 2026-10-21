# scripts/collections.R: community collections among CRAN packages.

QUOTES <- "['\"‘’“”]"

# TRUE when the text says it installs or loads a set of packages.
is_collection_text <- function(title, description) {
  n <- max(length(title), length(description))
  txt <- paste(rep_len(ifelse(is.na(title), "", title), n),
               rep_len(ifelse(is.na(description), "", description), n))
  txt <- gsub("\\s+", " ", txt)
  hit <- rep(FALSE, n)
  for (p in INSTALL_PATTERNS) hit <- hit | grepl(p, txt, ignore.case = TRUE, perl = TRUE)
  hit
}

regex_escape <- function(x) gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", x)

empty_collections <- function() {
  data.frame(slug = character(), kind = character(), package = character(),
             github_owner = character(), title = character(), description = character(),
             maintainer = character(), stringsAsFactors = FALSE)
}
empty_members <- function() {
  data.frame(slug = character(), package = character(), tier = character(),
             attached = integer(), stringsAsFactors = FALSE)
}
empty_mentions <- function() {
  data.frame(slug = character(), package = character(), source = character(),
             stringsAsFactors = FALSE)
}

find_collections <- function(inputs, settings) {
  pk <- inputs$packages
  deps <- inputs$deps
  own <- inputs$owners
  own$key <- tolower(own$owner)
  org_own <- own[own$owner_type == "Organization", , drop = FALSE]
  maint <- stats::setNames(normalise_maintainer(pk$maintainer), pk$name)
  txt <- stats::setNames(gsub("\\s+", " ", paste(ifelse(is.na(pk$title), "", pk$title),
                                                 ifelse(is.na(pk$description), "", pk$description))),
                         pk$name)
  owners_of <- function(p) unique(own$key[own$package == p])

  kin_of <- function(coll, cands) {
    cands <- setdiff(unique(cands), coll)
    if (!length(cands)) return(character())
    stem <- sub("verse$", "", tolower(coll))
    ok <- rep(FALSE, length(cands))
    mc <- maint[[coll]]
    if (!is.na(mc)) ok <- ok | (!is.na(maint[cands]) & maint[cands] == mc)
    oc <- owners_of(coll)
    if (length(oc)) ok <- ok | cands %in% own$package[own$key %in% oc]
    for (i in seq_along(cands)) {
      if (ok[i]) next
      w <- paste0("(?<![A-Za-z0-9_.])", regex_escape(cands[i]), "(?![A-Za-z0-9_]|\\.[A-Za-z0-9])")
      ok[i] <- grepl(w, txt[[coll]], ignore.case = TRUE, perl = TRUE) ||
        (nzchar(stem) && startsWith(tolower(cands[i]), stem))
    }
    cands[ok]
  }

  cols <- list(); mems <- list()
  cand <- pk$name[is_collection_text(pk$title, pk$description) &
                    !is.na(pk$loc_r) & pk$loc_r <= settings$loc_r_max]
  kind1_owners <- character()
  for (nm in sort(cand)) {
    d <- deps[deps$from == nm, , drop = FALSE]
    di <- d[d$type %in% c("Depends", "Imports"), , drop = FALSE]
    members <- NULL
    if (length(kin_of(nm, di$to)) >= 2L) {
      pkgs <- unique(di$to)
      att <- as.integer(pkgs %in% di$to[di$type == "Depends"])
      members <- data.frame(package = pkgs, attached = att, stringsAsFactors = FALSE)
    } else {
      sk <- kin_of(nm, d$to[d$type == "Suggests"])
      if (length(sk) >= 2L) members <- data.frame(package = sk, attached = 0L, stringsAsFactors = FALSE)
    }
    if (is.null(members)) next
    members$tier <- "installed"
    members <- members[members$package != nm, , drop = FALSE]
    o <- unique(org_own$owner[org_own$package == nm])
    if (length(o)) {
      op <- setdiff(unique(org_own$package[org_own$owner %in% o]), c(nm, members$package))
      if (length(op)) {
        members <- rbind(members, data.frame(package = op, attached = 0L, tier = "organization",
                                             stringsAsFactors = FALSE))
      }
      kind1_owners <- c(kind1_owners, tolower(o))
    }
    ow <- unique(own$owner[own$package == nm])
    slug <- tolower(nm)
    i <- match(nm, pk$name)
    cols[[slug]] <- data.frame(slug = slug, kind = "package", package = nm,
                               github_owner = if (length(ow)) ow[1] else NA_character_,
                               title = pk$title[i], description = pk$description[i],
                               maintainer = pk$maintainer[i], stringsAsFactors = FALSE)
    mems[[slug]] <- data.frame(slug = slug, package = members$package, tier = members$tier,
                               attached = members$attached, stringsAsFactors = FALSE)
  }

  # Kind 2: organizations with "verse" in the login.
  logins <- unique(org_own$owner)
  logins <- logins[grepl("verse", logins, ignore.case = TRUE) & !tolower(logins) %in% kind1_owners]
  cran_lc <- tolower(pk$name)
  edge <- deps[deps$type %in% c("Depends", "Imports", "LinkingTo"), , drop = FALSE]
  for (lg in sort(logins)) {
    slug <- tolower(lg)
    if (slug %in% names(cols)) next
    ps <- unique(org_own$package[org_own$owner == lg])
    if (length(ps) < settings$org_min_packages) next
    if (!any(edge$from %in% ps & edge$to %in% ps)) next
    i <- match(slug, cran_lc)
    cp <- if (is.na(i)) NA_character_ else pk$name[i]
    cols[[slug]] <- data.frame(slug = slug, kind = "organization", package = cp, github_owner = lg,
                               title = if (is.na(i)) NA_character_ else pk$title[i],
                               description = if (is.na(i)) NA_character_ else pk$description[i],
                               maintainer = if (is.na(i)) NA_character_ else pk$maintainer[i],
                               stringsAsFactors = FALSE)
    mp <- setdiff(ps, cp)
    mems[[slug]] <- data.frame(slug = slug, package = mp, tier = "organization",
                               attached = 0L, stringsAsFactors = FALSE)
  }

  if (!length(cols)) {
    return(list(collections = empty_collections(), members = empty_members(),
                mentions = empty_mentions()))
  }
  collections <- do.call(rbind, cols)
  members <- do.call(rbind, mems)

  # Mentions: quoted name in the text, or an exact topic.
  tl <- ifelse(is.na(pk$title), "", pk$title)
  dl <- gsub("\\s+", " ", ifelse(is.na(pk$description), "", pk$description))
  mn <- list()
  for (k in seq_len(nrow(collections))) {
    slug <- collections$slug[k]
    own_pkg <- collections$package[k]
    pat <- paste0(QUOTES, regex_escape(slug), QUOTES)
    inT <- grepl(pat, tl, ignore.case = TRUE, perl = TRUE)
    inD <- grepl(pat, dl, ignore.case = TRUE, perl = TRUE)
    tp <- unique(inputs$topics$package[tolower(inputs$topics$topic) == slug])
    m <- rbind(
      data.frame(package = pk$name[inT], source = rep("title", sum(inT))),
      data.frame(package = pk$name[inD], source = rep("description", sum(inD))),
      data.frame(package = tp, source = rep("topic", length(tp)))
    )
    skip <- c(members$package[members$slug == slug], own_pkg)
    m <- m[!m$package %in% skip & m$package %in% pk$name, , drop = FALSE]
    if (nrow(m)) mn[[slug]] <- data.frame(slug = slug, package = m$package, source = m$source,
                                          stringsAsFactors = FALSE)
  }
  mentions <- if (length(mn)) do.call(rbind, mn) else empty_mentions()

  sort_by <- function(d, ...) {
    d <- d[order(..., method = "radix"), , drop = FALSE]
    rownames(d) <- NULL
    d
  }
  list(collections = sort_by(collections, collections$slug),
       members = sort_by(members, members$slug, members$package),
       mentions = sort_by(mentions, mentions$slug, mentions$package, mentions$source))
}

# Merge today's finds into the previous tables, keeping ids and gone rows.
carry_collections <- function(prev, found, today) {
  pc <- prev$collections
  if (is.null(pc) || !nrow(pc)) pc <- NULL
  pm <- prev$members
  if (is.null(pm) || !nrow(pm)) {
    pm <- data.frame(collection_id = integer(), package = character(), tier = character(),
                     attached = integer(), stringsAsFactors = FALSE)
  }
  pn <- prev$mentions
  if (is.null(pn) || !nrow(pn)) {
    pn <- data.frame(collection_id = integer(), package = character(), source = character(),
                     stringsAsFactors = FALSE)
  }
  fc <- found$collections
  ids <- stats::setNames(integer(), character())
  if (!is.null(pc)) ids <- stats::setNames(as.integer(pc$collection_id), pc$slug)
  new <- sort(setdiff(fc$slug, names(ids)), method = "radix")
  top <- if (length(ids)) max(ids) else 0L
  ids <- c(ids, stats::setNames(top + seq_along(new), new))

  cur <- fc
  cur$collection_id <- unname(ids[cur$slug])
  old_first <- if (is.null(pc)) character() else stats::setNames(as.character(pc$first_seen), pc$slug)
  cur$first_seen <- ifelse(cur$slug %in% names(old_first), old_first[cur$slug], today)
  cur$last_seen <- today
  cur$status <- "active"
  keep <- c("collection_id", "slug", "kind", "package", "github_owner", "title", "description",
            "maintainer", "first_seen", "last_seen", "status")
  cur <- cur[, keep]
  gone <- NULL
  if (!is.null(pc)) {
    gone <- pc[!pc$slug %in% fc$slug, , drop = FALSE]
    if (nrow(gone)) {
      gone$status <- "gone"
      gone <- gone[, keep]
    } else gone <- NULL
  }
  all_c <- rbind(cur, gone)
  all_c$collection_id <- as.integer(all_c$collection_id)
  all_c <- all_c[order(all_c$collection_id), , drop = FALSE]
  rownames(all_c) <- NULL

  gone_ids <- if (is.null(gone)) integer() else as.integer(gone$collection_id)
  fm <- found$members
  fm <- data.frame(collection_id = unname(ids[fm$slug]), package = fm$package, tier = fm$tier,
                   attached = as.integer(fm$attached), stringsAsFactors = FALSE)
  pm$attached <- as.integer(pm$attached)
  pm$collection_id <- as.integer(pm$collection_id)
  members <- rbind(fm, pm[pm$collection_id %in% gone_ids, names(fm), drop = FALSE])
  fn <- found$mentions
  fn <- data.frame(collection_id = unname(ids[fn$slug]), package = fn$package, source = fn$source,
                   stringsAsFactors = FALSE)
  pn$collection_id <- as.integer(pn$collection_id)
  mentions <- rbind(fn, pn[pn$collection_id %in% gone_ids, names(fn), drop = FALSE])
  members <- members[order(members$collection_id, members$package, method = "radix"), , drop = FALSE]
  mentions <- mentions[order(mentions$collection_id, mentions$package, mentions$source,
                             method = "radix"), , drop = FALSE]
  rownames(members) <- NULL
  rownames(mentions) <- NULL
  list(collections = all_c, members = members, mentions = mentions)
}
