---
description: Judge the cran-subjects groups waiting for a classification and write classifications/subjects.json
---

Judge the subjects that are waiting for a classification. Do these steps in order.

1. Run `Rscript scripts/judge.R prepare out/judge/`. Add `--all` when the owner asks for a full pass.
   - `--input /path/to/observatory.db` uses a local store instead of downloading the release asset and unpacking it to 4.65 GB.
   - `--state /path/to/cran-subjects.db` uses a local state database instead of the current release.
2. Read `out/judge/judge-system.txt` and `out/judge/classification-schema.json`.
3. Judge every entry in `out/judge/judge-queue.jsonl`. Each entry has a `prompt` to answer by the system rules.
   - With more than 30 entries, dispatch subagents with 30 entries each. Give each one the system text, the schema and its entries. Each returns one JSON line per entry, matching the schema with `subject_id` added.
   - With 30 entries or fewer, judge them in this session.
4. Write every answer line to `out/judge/judge-answers.jsonl`.
5. Run `Rscript scripts/judge.R merge out/judge/ --model llm`.
6. If `out/judge/judge-invalid.jsonl` has entries, answer those subjects again with their errors in view, append the new answers to `out/judge/judge-answers.jsonl`, and run the merge once more. Do this once only.
7. Report the counts by classification, and say that `classifications/subjects.json` is ready for the owner to commit. Never commit or push.
