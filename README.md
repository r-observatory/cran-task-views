# cran-task-views

This pipeline records CRAN task-view membership over time: which packages
belong to which task view, when they were added or removed, and whether they
are listed as core. CRAN task views themselves only reflect current
membership, so this fills that gap by replaying each view's edit history.

It enumerates the task-view repositories under the `cran-task-views` GitHub
org (the `ctv` toolkit repo itself is excluded, since it is not a task view)
and, for each view, walks the git history of its markdown source, parsing the
inline `r pkg(...)` code spans at every revision to get a package-to-core
snapshot. Snapshots are diffed pairwise into an append-only event log, which
is then replayed into a current membership table. The pipeline is
dependency-light: it uses `git` and `gh` plus base-R string parsing, with no
`ctv`/knitr/pandoc toolchain. The aggregated data is written to a SQLite
database and published to the `r-observatory/cran-task-views` GitHub
repository for downstream consumers.

## Output

`cran-task-views.db` (published on the rolling `current` release) contains:

- `cran_task_views` - one row per task view: `name`, `topic`, `maintainer`,
  `url`, `updated`.
- `cran_task_view_events` - the append-only membership event log: `view`,
  `package`, `event_type` (`added`, `removed`, `core_change`), `event_date`,
  `core`, `source`.
- `cran_task_view_membership` - current membership derived from the event
  log: `view`, `package`, `added_date`, `removed_date`, `currently_member`,
  `core`.

Task-view membership is factual data drawn from each view's own history;
every view remains attributed to its maintainer of record, as published by
CRAN.

## Running

```sh
Rscript scripts/update.R out/            # full rebuild, replays all revisions
Rscript scripts/update.R out/ --bootstrap  # full rebuild
Rscript tests/testthat.R                 # unit tests
```
