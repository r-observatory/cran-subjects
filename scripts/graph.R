# scripts/graph.R: dependency links, clusters, second memberships and subject cores.

empty_edges <- function() data.frame(from = character(), to = character(), weight = numeric())

# Undirected pairs, from before to (bytewise), duplicate weights summed.
combine_edges <- function(...) {
  parts <- Filter(function(e) !is.null(e) && nrow(e) > 0L, list(...))
  if (length(parts) == 0L) return(empty_edges())
  e <- do.call(rbind, lapply(parts, function(p) p[, c("from", "to", "weight")]))
  e <- e[e$from != e$to, , drop = FALSE]
  fwd <- order_before(e$from, e$to)
  lo <- ifelse(fwd, e$from, e$to)
  hi <- ifelse(fwd, e$to, e$from)
  key <- paste(lo, hi, sep = "\r")
  w <- tapply(e$weight, key, sum)
  first <- match(names(w), key)
  out <- data.frame(from = lo[first], to = hi[first], weight = as.numeric(w),
                    stringsAsFactors = FALSE)
  out <- out[order(out$from, out$to, method = "radix"), , drop = FALSE]
  rownames(out) <- NULL
  out
}

# TRUE where a sorts strictly before b, bytewise.
order_before <- function(a, b) {
  r <- match(c(a, b), sort(unique(c(a, b)), method = "radix"))
  r[seq_along(a)] < r[length(a) + seq_along(b)]
}

# Depends, Imports and LinkingTo among universe packages, weight 1 / log2(2 + in-degree).
dependency_edges <- function(deps, universe) {
  d <- deps[deps$type %in% c("Depends", "Imports", "LinkingTo"), c("from", "to"), drop = FALSE]
  d <- unique(d[d$from %in% universe & d$to %in% universe & d$from != d$to, , drop = FALSE])
  if (nrow(d) == 0L) return(empty_edges())
  indeg <- table(d$to)
  d$weight <- 1 / log2(2 + as.numeric(indeg[d$to]))
  combine_edges(d)
}

# Leiden clusters over all packages; ids follow the sorted package order.
find_clusters <- function(edges, packages, resolution, seed, iterations) {
  packages <- sort(unique(packages), method = "radix")
  e <- edges[edges$from %in% packages & edges$to %in% packages, , drop = FALSE]
  g <- igraph::graph_from_data_frame(e[, c("from", "to")], directed = FALSE,
                                     vertices = data.frame(name = packages))
  igraph::E(g)$weight <- e$weight
  set.seed(seed)
  memb <- if (igraph::ecount(g) == 0L) seq_along(packages) else {
    igraph::membership(igraph::cluster_leiden(
      g, objective_function = "modularity", weights = igraph::E(g)$weight,
      resolution = resolution, n_iterations = iterations))
  }
  data.frame(package = packages, cluster = match(memb, unique(memb)), stringsAsFactors = FALSE)
}

# Share of each package's link weight that lands in each cluster.
link_shares <- function(edges, assignment) {
  cl <- setNames(assignment$cluster, assignment$package)
  e <- edges[edges$from %in% assignment$package & edges$to %in% assignment$package, , drop = FALSE]
  if (nrow(e) == 0L) return(data.frame(package = character(), cluster = integer(), share = numeric()))
  both <- data.frame(package = c(e$from, e$to), other = c(e$to, e$from),
                     weight = c(e$weight, e$weight), stringsAsFactors = FALSE)
  both$cluster <- as.integer(cl[both$other])
  s <- aggregate(weight ~ package + cluster, both, sum)
  s$share <- s$weight / ave(s$weight, s$package, FUN = sum)
  s <- s[order(s$package, s$cluster, method = "radix"), c("package", "cluster", "share")]
  rownames(s) <- NULL
  s
}

# Best other cluster per package with share >= min_share, for packages linked into their own cluster.
second_memberships <- function(shares, primary, min_share) {
  cl <- setNames(primary$cluster, primary$package)
  s <- shares
  own <- paste(s$package, s$cluster) %in% paste(primary$package, primary$cluster)
  linked <- unique(s$package[own])
  s <- s[!own & s$package %in% linked & s$share >= min_share - 1e-12, , drop = FALSE]
  s <- s[order(s$package, -s$share, s$cluster, method = "radix"), , drop = FALSE]
  s <- s[!duplicated(s$package), c("package", "cluster"), drop = FALSE]
  rownames(s) <- NULL
  s
}

# Members used by enough other members; the top few in a large subject.
subject_core <- function(members, deps, settings) {
  d <- deps[deps$type %in% c("Depends", "Imports", "LinkingTo") &
              deps$from %in% members & deps$to %in% members & deps$from != deps$to, , drop = FALSE]
  d <- unique(d[, c("from", "to")])
  users <- table(factor(d$to, levels = members))
  keep <- names(users)[users >= settings$core_min_users]
  if (length(members) > settings$core_large_subject && length(keep) > 0L) {
    n <- ceiling(settings$core_top_fraction * length(members))
    keep <- keep[order(-as.numeric(users[keep]), keep, method = "radix")]
    keep <- keep[seq_len(min(n, length(keep)))]
  }
  sort(keep, method = "radix")
}
