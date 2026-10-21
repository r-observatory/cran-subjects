# cran-subjects

This pipeline finds subjects among CRAN packages for the R Observatory site. A subject is a group of packages that belong together, found by the site itself from package text, repository topics and dependencies. The pipeline also finds community collections, such as the tidyverse, where a community has already declared its own set of packages. Everything is published as one SQLite database with a manifest on a rolling GitHub release.

Subjects and collections are kept apart. A subject is made by the site. A collection is declared by a community. Task views are not changed, and Bioconductor packages keep their biocViews.

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

# The core packages of one subject
dbGetQuery(con, "
  SELECT m.package, m.reason_kind, m.reason_value
  FROM subject_members m
  JOIN subjects s USING (subject_id)
  WHERE s.slug = 'survival-analysis' AND m.is_core = 1 AND m.left IS NULL
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

`cran-subjects.db` and `manifest.json` are published on the rolling `current` release. The manifest carries `db_filename`, `db_bytes`, `db_sha256`, `tables` and `complete`.

- `subjects` - one row per subject: `subject_id`, `slug`, `name`, `description`, `status`, `held_reason`, `named_by`, `named_at`, `classification_at`, `first_seen`, `last_seen`, `redirect_to`, `size`. The status is `held`, `named`, `judged` or `retired`. Only `named` and `judged` rows are meant for the pages. The slug is formed from the first name a subject is published under and never changes. A retired subject keeps its row, and `redirect_to` holds the subject that took most of its members when there is one.
- `subject_members` - one row per package in a subject: `subject_id`, `package`, `is_core`, `is_primary`, `reason_kind`, `reason_value`, `joined`, `left`. A package can sit in more than one subject, and `is_primary` marks its main one. `reason_kind` and `reason_value` hold the plain fact behind the placement, such as the strongest dependency inside the subject or a shared title phrase. Former members are kept with a `left` date.
- `subject_name_basis` - the packages a subject's name was written from: `subject_id`, `package`.
- `subject_task_views` - the task views that overlap a subject: `subject_id`, `view`, `in_common`.
- `collections` - one row per collection: `collection_id`, `slug`, `kind`, `package`, `github_owner`, `title`, `description`, `maintainer`, `first_seen`, `last_seen`, `status`. The kind is `package` or `organization`. The status is `active` or `gone`, and a gone collection keeps its row as history.
- `collection_members` - the packages that count as members: `collection_id`, `package`, `tier`, `attached`. The tier is `installed` for packages the collection package installs and `organization` for the other CRAN packages of its GitHub organization. `attached` is 1 for packages loaded through Depends.
- `collection_mentions` - packages that name a collection without being members: `collection_id`, `package`, `source`. The source is `description` or `topic`. Mentions are never counted as members.
- `seen_packages` - the first day each package was seen by the pipeline: `package`, `first_seen`.
- `pipeline_runs` - one row per run: `run_at`, `mode`, `n_packages`, `n_subjects_published`, `n_held`, `n_named_calls`, `n_collections`.

## How subjects are found

The input is the latest `observatory.db.zst` from the `r-observatory/data` release. Only current CRAN packages are used.

1. Each package contributes its title (counted twice), its description and its repository topics. Generic topics, URLs and DOIs are dropped, and so are the wording-only words that make clusters on their own, such as interface, client, wrapper and utility.
2. The text becomes TF-IDF vectors with one- and two-word terms, reduced by SVD to 256 dimensions.
3. Each package links to its 15 nearest packages by text. Each Depends, Imports or LinkingTo link adds weight, scaled down for packages everything depends on.
4. Leiden clustering with a fixed seed finds the clusters. The clustering uses no AI model.
5. A package also joins a second subject when a large share of its link weight goes there.
6. The core of a subject is the members most used by other members.

A cluster is published as a subject only when all of these hold:

- It has at least 20 packages.
- It has at least 5 distinct maintainers, and no one maintainer holds more than half of it.
- The model's name for it passes the naming rules.
- The latest classification on it is not hold.

Held packages count as in no subject.

Each new cluster is matched to last week's subjects by overlap. A matched subject keeps its id, slug, name and first-seen day. An unmatched cluster becomes a new subject. A subject that no cluster matches is retired, and its address is never reused. A subject is renamed only when its membership has drifted well past what it was when named.

## How subjects are named

A free open model, Qwen3 8B (Apache 2.0), names new subjects. It is run by llama.cpp b11669 with `--no-jinja`. The model file and the llama.cpp build are pinned by SHA-256 in `scripts/config.R`, and the hash is checked after every download. There is no API key or secret anywhere in this repository.

The weekly run names a subject when it is new or has drifted. It names at most 40 subjects per run, so a run stays well inside the job limit. The model sees up to 40 member titles, the distinctive title phrases and the overlapping task views, and answers with a name and a one-sentence description in JSON.

Code then applies the rules to the answer. Names go to sentence case with acronyms and package names kept. Generic tail words are removed. A name made only of generic words holds the subject back. Two subjects with near-duplicate names are both held back. If the model file cannot be fetched or the model gives no valid answer, new subjects stay held back and are retried next run. Naming never fails the run, and published subjects keep their names.

A subject named this way and passing every check has the status `named`.

## How subjects are judged

the LLM judges every subject once a month, from the owner's Mac, inside the coding agent and never through a one-shot model call. Its classification replaces the open model's name and description.

1. The reminder issue appears on the 1st of the month. It lists the subjects waiting for a classification.
2. Open the coding agent in this repository and run `/judge-subjects`.
3. The command runs `Rscript scripts/judge.R prepare out/judge/`, judges each waiting subject (with subagents in batches of 30 when there are many), then runs `Rscript scripts/judge.R merge out/judge/ --model <model id>`.
4. Commit `classifications/subjects.json`.

A classification is keep, merge, split or hold. The next weekly run applies it. Keep replaces the name and description and sets the status to `judged`. Merge moves the members into the other subject. Split re-clusters the subject around the LLM's groups and retires the old id with a redirect. Hold stops the subject from being published. Strays are removed and placed again by their next strongest links. A classification stays in force while the membership stays near the one it was judged on.

If last week's state or the classifications file cannot be read, the run fails instead of starting from nothing.

## How collections are found

Collections are found by rules, with no model. The daily run repeats them.

- **Collection package.** A current CRAN package counts when its title or description says it installs or loads a set of packages or calls itself a meta-package, its Depends and Imports name at least two packages of the set, and it has little R code of its own. Members are its Depends and Imports, with base R left out.
- **Organization community.** A GitHub owner of type Organization counts when it owns a collection package, or when its name contains "verse" and it holds at least 3 current CRAN packages that link to each other.
- **Members.** The packages a collection package installs, plus the other CRAN packages of the collection's own GitHub organization. A personal account's other packages are never members.
- **Mentions.** Packages whose description names the collection in quotes, or whose repository topic is its name, and that are not members.
- **Gone.** A collection whose package leaves CRAN, or whose organization no longer qualifies, is marked gone and keeps its row.

The daily run also places packages that are new since the last full run into existing subjects. It makes no new subjects, so membership does not flicker.

## Running

```sh
Rscript scripts/update.R out/            # weekly or daily run
Rscript scripts/judge.R prepare out/judge/   # monthly, from /judge-subjects
Rscript tests/testthat.R                 # unit tests
```

## Numbers

These figures come from the first full run on 21 Oct 2026, against the data release of 20 Oct 2026.

| Figure | Value |
|---|---:|
| Current CRAN packages | 25,406 |
| Subjects published (judged) | 175 |
| Subjects held | 7 |
| Subjects retired (by splits) | 54 |
| CRAN packages in at least one published subject | 24,133 |
| Member rows with no stated reason | 415 of 26,267 |
| Collections (package and organization) | 45 (25 and 20) |
| Collection mentions | 222 |

The first pass named 122 clusters with the open model in 1,086 seconds on an Apple M2 Max GPU. the LLM then judged all 124 subjects (64 keep, 53 split, 7 hold), and a second pass applied the classifications. A cold weekly pass without naming takes about 300 seconds on the same machine.

## Caveats

Subjects are the site's own grouping and can be wrong. A subject named by the open model can show for up to a month before the LLM judges it. Subjects cover current CRAN packages only. Packages in no subject are either too isolated to cluster or sit in a held cluster.

## Feedback

Found a bug, a wrong number, or a missing package? Report it at [r-observatory/feedback](https://github.com/r-observatory/feedback/issues/new/choose). All feedback about R Observatory, the site, the data, and the pipelines, is tracked in one place.

## License

This repository is proprietary. Copyright (c) 2026 HJJB, LLC. All rights reserved; see [LICENSE](LICENSE).
