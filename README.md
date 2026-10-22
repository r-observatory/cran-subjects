# cran-subjects

This pipeline finds subjects among CRAN packages for the R Observatory site. A subject is a group of packages that belong together, found from package text, repository topics and dependencies. The pipeline also finds community collections, such as the tidyverse, where a community has already declared its own set of packages. Subjects and collections are kept apart, task views are not changed, and Bioconductor packages keep their biocViews.

It reads the latest `observatory.db` from the `r-observatory/data` release. Each week it clusters current CRAN packages with TF-IDF, a truncated SVD, nearest neighbours and dependency links, starting from last week's clusters, keeps each subject's id and slug stable from run to run, and names new subjects with a free open model, Qwen3 8B, run by llama.cpp with both pinned by SHA-256. Each month an LLM classifies every subject as keep, split, merge or hold, and those classifications in `classifications/subjects.json` are applied on the next weekly run. Each day it places new packages into existing subjects and finds collections again. The data is written to a SQLite database and published to the `r-observatory/cran-subjects` GitHub repository for downstream consumers.

## Data Access

```bash
gh release download current \
  --repo r-observatory/cran-subjects \
  --pattern "cran-subjects.db"
```

```r
library(RSQLite)
con <- dbConnect(SQLite(), "cran-subjects.db")

# The ten largest published subjects
dbGetQuery(con, "
  SELECT slug, name, size
  FROM subjects
  WHERE status IN ('named', 'judged')
  ORDER BY size DESC
  LIMIT 10
")

# The members of a collection
dbGetQuery(con, "
  SELECT m.package, m.tier, m.attached
  FROM collection_members m
  JOIN collections c USING (collection_id)
  WHERE c.slug = 'tidyverse'
")
```

```python
import sqlite3

con = sqlite3.connect("cran-subjects.db")

# Collections and how many members each has
for row in con.execute("""
    SELECT c.slug, c.kind, COUNT(*) AS members
    FROM collections c
    JOIN collection_members m USING (collection_id)
    WHERE c.status = 'active'
    GROUP BY c.collection_id
    ORDER BY members DESC
"""):
    print(row)
```

## Output

`cran-subjects.db` and `manifest.json` are published on the rolling `current` release. The manifest carries `db_filename`, `db_bytes`, `db_sha256`, `tables` and `complete`. Each month's last state is also kept on a `snapshot-YYYY-MM` release, with a copy of that month's classifications as `classifications.json`, so any month can be restored.

- `subjects` - one row per subject: `subject_id`, `slug`, `name`, `description`, `status`, `held_reason`, `named_by`, `named_at`, `classification_at`, `first_seen`, `last_seen`, `redirect_to`, `size`. The status is `held`, `named`, `judged` or `retired`, and only `named` and `judged` rows are published. A slug never changes once published, and a retired subject keeps its row with `redirect_to` pointing at the subject that took most of its members.
- `subject_members` - one row per package in a subject: `subject_id`, `package`, `is_core`, `is_primary`, `reason_kind`, `reason_value`, `joined`, `left`. A package can sit in more than one subject, and `is_primary` marks its main one. `reason_kind` and `reason_value` hold the plain fact behind the placement. Former members are kept with a `left` date.
- `subject_name_basis` - the packages a subject's name was written from: `subject_id`, `package`.
- `subject_task_views` - the task views that overlap a subject: `subject_id`, `view`, `in_common`.
- `collections` - one row per collection: `collection_id`, `slug`, `kind`, `package`, `github_owner`, `title`, `description`, `maintainer`, `first_seen`, `last_seen`, `status`. The kind is `package` or `organization`, and the status is `active` or `gone`.
- `collection_members` - the packages that count as members: `collection_id`, `package`, `tier`, `attached`. The tier is `installed` for packages the collection package installs and `organization` for the other CRAN packages of its GitHub organization.
- `collection_mentions` - packages that name a collection without being members: `collection_id`, `package`, `source`.
- `seen_packages` - the first day each package was seen: `package`, `first_seen`.
- `pipeline_runs` - one row per run: `run_at`, `mode`, `n_packages`, `n_subjects_published`, `n_held`, `n_named_calls`, `n_collections`.
- `weekly_clusters` - the cluster each package fell in on the last weekly run, before any classification: `package`, `cluster`. The next weekly run starts its clustering from them.

## Running

```sh
Rscript scripts/update.R out/ --mode weekly   # weekly run, daily with --mode daily
Rscript scripts/judge.R prepare out/judge/    # monthly, from /judge-subjects
Rscript tests/testthat.R                      # unit tests
```

Each month a reminder issue lists the subjects waiting for a classification. Run `/judge-subjects` in this repository, then commit `classifications/subjects.json`.

## Feedback

Found a bug, a wrong number, or a missing package? Report it at [r-observatory/feedback](https://github.com/r-observatory/feedback/issues/new/choose). All feedback about R Observatory, the site, the data, and the pipelines, is tracked in one place.

## License

This repository is proprietary. Copyright (c) 2026 HJJB, LLC. All rights reserved; see [LICENSE](LICENSE).
