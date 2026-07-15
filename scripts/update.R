#!/usr/bin/env Rscript
# scripts/update.R: CRAN Task Views membership catalog builder.
#
# run_update(io, out_dir, force_full) takes an injectable io for offline testing;
# default_io() supplies the real ctv-reader + git fetchers (marked FINALIZE below).
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
# default_io(): real fetchers. FINALIZE-AT-BUILD-TIME. These cannot be
# exercised offline in this plan and MUST be completed against the real `ctv`
# package and a local git clone of each view repo:
#   * read_ctv_at MUST route through ctv::read.ctv() (which applies the upstream
#     "core if any mention is core" rule, ctv-md.R:67-71) rather than re-parsing
#     pkg() tokens by hand, and MUST use `packagelist` UNION the view's
#     archived-but-listed packages, carrying CRAN active/archived status as a
#     SEPARATE flag (never as membership) so a CRAN archival does not emit a
#     spurious "removed".
#   * run_update() always performs a full deterministic replay: for every view
#     it walks the complete revision list from view_repo_revisions and rebuilds
#     the event log and membership table from scratch. It never reads or diffs
#     against a previously published DB, so default_io's fetchers must return
#     each view's full revision history on every run; there is no incremental
#     or forward-only path to seed here.
# ---------------------------------------------------------------------------
default_io <- function() {
  list(
    available_views     = function() ctv::available.views()$name,        # FINALIZE
    view_repo_revisions = function(v) stop("FINALIZE: git log of the ", v, " view repo"),
    read_ctv_at         = function(v, sha) stop("FINALIZE: ctv::read.ctv at revision ", sha),
    view_info           = function(v) stop("FINALIZE: ctv view metadata for ", v)
  )
}

if (sys.nframe() == 0L) {
  args    <- commandArgs(trailingOnly = TRUE)
  out_dir <- if (length(args) >= 1L && !startsWith(args[1L], "--")) args[1L] else "out"
  force   <- "--bootstrap" %in% args
  run_update(default_io(), out_dir, force)
}
