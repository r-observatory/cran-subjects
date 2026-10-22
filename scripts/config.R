# scripts/config.R: constants for the cran-subjects pipeline.

PUBLISH_REPO <- "r-observatory/cran-subjects"
DB_FILENAME <- "cran-subjects.db"
MANIFEST_FILENAME <- "manifest.json"
INPUT_REPO <- "r-observatory/data"
INPUT_ASSET <- "observatory.db.zst"
CLASSIFICATIONS_PATH <- "classifications/subjects.json"

# Repository files resolve against REPO_ROOT, which the command-line entry points set from the
# script location; unset, they resolve against the working directory.
REPO_ROOT <- NULL
repo_path <- function(...) if (is.null(REPO_ROOT)) file.path(...) else file.path(REPO_ROOT, ...)

MODEL_FILE <- "Qwen3-8B-Q4_K_M.gguf"
MODEL_URL <- "https://huggingface.co/Qwen/Qwen3-8B-GGUF/resolve/main/Qwen3-8B-Q4_K_M.gguf"
MODEL_SHA256 <- "d98cdcbd03e17ce47681435b5150e34c1417f50b5c0019dd560e4882c5745785"
MODEL_BYTES <- 5027783488
LLAMA_TAG <- "b11669"
LLAMA_ASSET <- "llama-b11669-bin-ubuntu-x64.tar.gz"
LLAMA_SHA256 <- "cf07c1c112d709bd67e522017471de57ed0ec8bea031d732c5422351c07f65ac"
TITLE_WEIGHT <- 2L; TFIDF_MIN_DF <- 3L; TFIDF_MAX_DF <- 0.2; SVD_DIM <- 256L
KNN_K <- 15L; KNN_MIN_SIM <- 0.30
LEIDEN_RESOLUTION <- 3; LEIDEN_SEED <- 1L; LEIDEN_ITERATIONS <- 20L
LEIDEN_START_ITERATIONS <- -1L  # from last week's clusters, passes run until quality stops rising
MIN_SUBJECT_SIZE <- 20L; MIN_MAINTAINERS <- 5L; MAX_MAINTAINER_SHARE <- 0.5
MIN_KEEP_SIZE <- 10L  # a published subject under MIN_SUBJECT_SIZE stays while it keeps this many and still matches
SECOND_SHARE <- 0.30; DAILY_PRIMARY_SHARE <- 0.40
CORE_MIN_USERS <- 3L; CORE_LARGE_SUBJECT <- 60L; CORE_TOP_FRACTION <- 0.05
MATCH_JACCARD <- 0.5; DRIFT_JACCARD <- 0.7
NAME_CAP <- 40L; NAME_MEMBERS <- 40L
NAME_BUDGET_S <- 9000  # 150 minutes of a run, after which no naming call starts
LOC_R_MAX <- 2000L; ORG_MIN_PACKAGES <- 3L
GENERIC_TOPICS <- c("r", "r-package", "rstats", "cran", "r-programming", "package",
                    "rpackage", "r-language", "cran-r", "hacktoberfest")
WORDING_WORDS <- c("interface", "interfaces", "client", "clients", "wrapper", "wrappers",
                   "access", "accessing", "tool", "tools", "toolkit", "utility", "utilities",
                   "helper", "helpers", "miscellaneous", "misc", "collection", "collections",
                   "processing", "modelling", "modeling", "index", "indices", "indexes",
                   "project", "projects")
GENERIC_TAIL_WORDS <- c("tools", "tool", "techniques", "methods", "functions", "operations")
GENERIC_NAME_WORDS <- c("utility", "utilities", "functions", "helpers", "miscellaneous",
                        "interfaces", "wrappers", "data", "analysis", "models", "tools", "methods")
INSTALL_PATTERNS <- c("install(s|ing)?\\b.{0,40}\\bload", "\\bload(s|ing)?\\b.{0,40}\\binstall",
                      "meta-?package", "umbrella package", "in one go",
                      "\\b(install|load|attach)\\w*\\b.{0,60}\\b(suite|set|family|collection) of (r )?packages",
                      "\\b(suite|set|family|collection) of (r )?packages\\b.{0,60}\\b(install|load|attach)")

default_settings <- function() {
  list(
    title_weight = TITLE_WEIGHT, min_df = TFIDF_MIN_DF, max_df = TFIDF_MAX_DF,
    svd_dim = SVD_DIM, knn_k = KNN_K, knn_min_sim = KNN_MIN_SIM,
    resolution = LEIDEN_RESOLUTION, seed = LEIDEN_SEED, iterations = LEIDEN_ITERATIONS,
    start_iterations = LEIDEN_START_ITERATIONS,
    min_subject_size = MIN_SUBJECT_SIZE, min_keep_size = MIN_KEEP_SIZE, min_maintainers = MIN_MAINTAINERS,
    max_maintainer_share = MAX_MAINTAINER_SHARE,
    second_share = SECOND_SHARE, daily_primary_share = DAILY_PRIMARY_SHARE,
    core_min_users = CORE_MIN_USERS, core_large_subject = CORE_LARGE_SUBJECT,
    core_top_fraction = CORE_TOP_FRACTION,
    match_jaccard = MATCH_JACCARD, drift_jaccard = DRIFT_JACCARD,
    name_cap = NAME_CAP, name_members = NAME_MEMBERS, name_budget_s = NAME_BUDGET_S,
    loc_r_max = LOC_R_MAX, org_min_packages = ORG_MIN_PACKAGES
  )
}
