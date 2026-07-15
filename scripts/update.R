#!/usr/bin/env Rscript
# scripts/update.R: CRAN Task Views membership catalog builder.
#
# run_update(io, out_dir, force_full) takes an injectable io for offline testing;
# default_io() supplies the real git + markdown-parser fetchers (see below).
options(timeout = 600)
suppressPackageStartupMessages({ library(RSQLite) })

.this_file <- function() {
  for (i in rev(seq_len(sys.nframe()))) {
    of <- sys.frame(i)$ofile
    if (!is.null(of) && nzchar(of)) return(normalizePath(of))
  }
  a <- commandArgs(FALSE); f <- sub("^--file=", "", grep("^--file=", a, value = TRUE))
  if (length(f) == 1L && nzchar(f)) return(normalizePath(f)) else NA_character_
}
.script_dir <- { tf <- .this_file(); if (!is.na(tf)) dirname(tf) else "scripts" }
if (!exists("export_task_views", mode = "function")) {
  source(file.path(.script_dir, "config.R"))
  source(file.path(.script_dir, "helpers.R"))
}
if (!exists("parse_ctv_membership", mode = "function")) {
  source(file.path(.script_dir, "ctv_md.R"))
}

run_update <- function(io, out_dir, force_full = FALSE) {
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  views <- setdiff(io$available_views(), CTV_EXCLUDE)
  if (!isTRUE(force_full) && length(views) < VIEWS_FLOOR) {
    stop(sprintf("available.views() returned %d (< %d): presumed truncated fetch; aborting.",
                 length(views), VIEWS_FLOOR))
  }

  all_events <- list(); meta_rows <- list()
  for (v in views) {
    revs <- io$view_repo_revisions(v)              # oldest-first data.frame(sha, date)
    prev_snap <- integer(0)
    if (!is.null(revs) && nrow(revs) > 0L) for (r in seq_len(nrow(revs))) {
      snap <- io$read_ctv_at(v, revs$sha[r])       # named int vector package -> core
      ev <- diff_membership(setNames(list(prev_snap), v),
                            setNames(list(snap), v), revs$date[r], "git")
      if (nrow(ev) > 0L) all_events[[length(all_events) + 1L]] <- ev
      prev_snap <- snap
    }
    info <- io$view_info(v)
    meta_rows[[length(meta_rows) + 1L]] <- data.frame(name = v,
      topic = info$topic %||% v, maintainer = info$maintainer %||% NA_character_,
      url = info$url %||% NA_character_, updated = info$updated %||% NA_character_,
      stringsAsFactors = FALSE)
  }

  events_df <- if (length(all_events)) do.call(rbind, all_events)
               else diff_membership(list(), list(), NA_character_, "git")
  membership_df <- derive_membership_from_events(events_df)
  views_df <- if (length(meta_rows)) do.call(rbind, meta_rows) else
    data.frame(name=character(0), topic=character(0), maintainer=character(0),
               url=character(0), updated=character(0), stringsAsFactors = FALSE)

  export_task_views(file.path(out_dir, DB_FILENAME), views_df, events_df, membership_df)
  invisible(list(n_views = length(views), n_events = nrow(events_df)))
}

# ---------------------------------------------------------------------------
# default_io(): the real, side-effecting fetchers. These are deliberately
# dependency-light: they shell out to `git` and `gh` and parse each view's
# markdown source directly (scripts/ctv_md.R), with no `ctv`/knitr/pandoc
# toolchain. Each task view is its own public repo under the cran-task-views
# org, holding a single `<View>.md` source; membership is read from the inline
# `r pkg(...)` code spans in that file.
#
#   * run_update() always performs a full deterministic replay: for every view
#     it walks the complete revision list from view_repo_revisions and rebuilds
#     the event log and membership table from scratch. It never reads or diffs
#     against a previously published DB, so these fetchers return each view's
#     full revision history on every run; there is no incremental path to seed.
#   * The revision walk covers the GitHub markdown era (~2021-12 onward), which
#     is the intended tracking window; the pre-migration XML `.ctv` history that
#     predates the markdown source is out of scope.
#   * A CRAN archival never touches a view's `.md`, so a package that goes
#     archived but stays listed produces an unchanged snapshot and no spurious
#     "removed" event, without needing a separate active/archived flag here.
#
# The four closures share a single per-run clone cache via lexical scope, so
# each view repo is cloned at most once per run.
# ---------------------------------------------------------------------------
default_io <- function() {
  cache <- file.path(tempdir(), paste0("ctv-clones-", Sys.getpid()))
  dir.create(cache, showWarnings = FALSE, recursive = TRUE)

  ensure_clone <- function(v) {
    dest <- file.path(cache, v)
    if (!dir.exists(file.path(dest, ".git"))) {
      url <- sprintf("https://github.com/cran-task-views/%s.git", v)
      status <- system2("git", c("clone", "--quiet", "--no-tags",
                                 shQuote(url), shQuote(dest)),
                        stdout = FALSE, stderr = FALSE)
      if (!identical(as.integer(status), 0L)) {
        stop(sprintf("git clone failed for view '%s' (exit %s)", v, status))
      }
    }
    dest
  }
  git_show <- function(dest, ref) {
    system2("git", c("-C", shQuote(dest), "show", shQuote(ref)),
            stdout = TRUE, stderr = FALSE)
  }

  list(
    # Enumerate the view repos under the org (gh is authed; --paginate handles
    # the full listing). `ctv` and any non-view infra repo are dropped upstream
    # by run_update via CTV_EXCLUDE and by an empty revision list respectively.
    available_views = function() {
      out <- system2("gh", c("api", "--paginate", "orgs/cran-task-views/repos",
                             "--jq", shQuote(".[].name")),
                     stdout = TRUE, stderr = FALSE)
      out <- trimws(out)
      out[nzchar(out)]
    },
    # Oldest-first revisions that changed `<View>.md`. `date` is the committer
    # date in YYYY-MM-DD form (%cs), a lexicographically sortable text key.
    view_repo_revisions = function(v) {
      dest <- ensure_clone(v)
      log <- system2("git", c("-C", shQuote(dest), "log", "--reverse",
                              shQuote("--format=%H|%cs"), "--",
                              shQuote(paste0(v, ".md"))),
                     stdout = TRUE, stderr = FALSE)
      log <- log[nzchar(log)]
      if (length(log) == 0L) return(NULL)
      parts <- strsplit(log, "|", fixed = TRUE)
      data.frame(
        sha  = vapply(parts, function(p) p[[1]], character(1)),
        date = vapply(parts, function(p) substr(p[[2]], 1L, 10L), character(1)),
        stringsAsFactors = FALSE)
    },
    # Membership snapshot at a revision: named integer vector package -> core.
    read_ctv_at = function(v, sha) {
      dest <- ensure_clone(v)
      txt <- git_show(dest, paste0(sha, ":", v, ".md"))
      if (length(txt) == 0L) return(integer(0))
      parse_ctv_membership(paste(txt, collapse = "\n"))
    },
    # HEAD metadata parsed from the `<View>.md` YAML header.
    view_info = function(v) {
      dest <- ensure_clone(v)
      txt <- git_show(dest, paste0("HEAD:", v, ".md"))
      if (length(txt) == 0L) {
        return(list(topic = NA_character_, maintainer = NA_character_,
                    url = sprintf("https://github.com/cran-task-views/%s/", v),
                    updated = NA_character_))
      }
      parse_ctv_header(paste(txt, collapse = "\n"), view = v)
    }
  )
}

if (sys.nframe() == 0L) {
  args    <- commandArgs(trailingOnly = TRUE)
  out_dir <- if (length(args) >= 1L && !startsWith(args[1L], "--")) args[1L] else "out"
  force   <- "--bootstrap" %in% args
  run_update(default_io(), out_dir, force)
}
