#!/usr/bin/env Rscript
# scripts/svn_era.R: task-view history from before the move to GitHub.
#
# Until 2021/22 every task view was a `.ctv` XML file in the ctv project's SVN
# on R-Forge. R-Forge is being retired, so its history is kept as a git bundle
# on this repo's `rforge-svn-archive` release. That history never changes, so
# it is extracted once, here, into two committed files under data/:
#
#   svn-era-events.tsv  view, package, event_type, event_date, core, source="svn"
#   svn-era-views.tsv   one row per view seen in that era, with its last header,
#                       last path, sha and edit date, the date its file was deleted, and
#                       `retired` for views that never reached GitHub
#
# update.R reads those files on every run. Run the extraction with
#   Rscript scripts/svn_era.R extract <path to ctv-rforge-svn-history.bundle>
# which checks the bundle's sha256 before reading it.

SVN_ARCHIVE_SHA256 <- "56581cd4b2ac399981dcef8cd07324f3d781ca38a5e56647bddd04fc5cfa381b"
# Old file names folded into the view they became.
SVN_RENAMES <- c(ExtremeValueTheory = "ExtremeValue", NumericalMath = "NumericalMathematics",
                 Robustness = "Robust", hydrology = "Hydrology", gR = "GraphicalModels")
# The last day of the SVN history. Genetics never moved to GitHub and was still
# on CRAN's task view index on 2022-04-01, so it is retired as of this day.
SVN_ERA_END <- "2022-04-03"
# Where a view file has lived, most recent layout first.
SVN_PATH_ORDER <- c("^pkg/inst/ctv/", "^inst/ctv/", "^pkg/inst/", "^[^/]+$")
SVN_EVENT_COLS <- c("view", "package", "event_type", "event_date", "core", "source")
SVN_VIEW_COLS  <- c("name", "topic", "maintainer", "updated", "last_path", "last_sha",
                    "last_date", "deleted", "retired")
RETIRED_URL <- "https://github.com/r-observatory/cran-task-views/blob/main/data/retired-views/%s.ctv"

svn_view_name <- function(basename) {
  v <- sub("\\.ctv$", "", basename)
  ifelse(v %in% names(SVN_RENAMES), unname(SVN_RENAMES[v]), v)
}

#' Map the `.ctv` paths of one commit to one path per view.
#'
#' A view can briefly have two files: during a directory move, a stray copy, or
#' a rename. The newer layout wins, then the file already bearing the view's
#' current name.
svn_pick_paths <- function(paths) {
  paths <- paths[grepl("\\.ctv$", paths)]
  if (length(paths) == 0L) return(character(0))
  base  <- basename(paths)
  view  <- svn_view_name(base)
  layer <- vapply(paths, function(p) which(vapply(SVN_PATH_ORDER, grepl, logical(1), x = p))[1],
                  integer(1))
  layer[is.na(layer)] <- length(SVN_PATH_ORDER) + 1L
  ord <- order(view, layer, sub("\\.ctv$", "", base) != view)
  keep <- ord[!duplicated(view[ord])]
  stats::setNames(paths[keep], view[keep])
}

#' Replay the SVN history into membership events and per-view details.
#'
#' commits: oldest-first data.frame(sha, date). read_tree(sha) returns a named
#' character vector of `.ctv` path -> blob id; read_blob(id) returns the text.
#' Only a changed blob is read and diffed, so a move or a rename with the same
#' content emits nothing. A view whose file disappears gets a `deleted` date
#' and no removals; if it comes back, it is diffed against its last list.
#' Events carry a day, not a time, so each day is read once, at its last
#' commit: an edit and its revert on the same day leave no trace.
svn_replay <- function(commits, read_tree, read_blob) {
  commits <- commits[!duplicated(commits$date, fromLast = TRUE), , drop = FALSE]
  snap <- list(); blob <- list(); present <- list(); info <- list()
  events <- list()
  for (i in seq_len(nrow(commits))) {
    sha <- commits$sha[i]; date <- commits$date[i]
    tree <- read_tree(sha)
    paths <- svn_pick_paths(names(tree))
    for (v in names(paths)) {
      b <- tree[[paths[[v]]]]
      if (isTRUE(present[[v]]) && identical(blob[[v]], b)) {
        info[[v]]$last_path <- paths[[v]]
        next
      }
      txt <- read_blob(b)
      s <- parse_ctv_xml_membership(txt)
      ev <- diff_membership(stats::setNames(list(snap[[v]] %||% integer(0)), v),
                            stats::setNames(list(s), v), date, "svn")
      if (nrow(ev) > 0L) events[[length(events) + 1L]] <- ev
      h <- parse_ctv_xml_header(txt)
      snap[[v]] <- s; blob[[v]] <- b; present[[v]] <- TRUE
      info[[v]] <- list(topic = h$topic, maintainer = h$maintainer, updated = h$updated,
                        last_path = paths[[v]], last_sha = sha, last_date = date,
                        deleted = NA_character_)
    }
    for (v in setdiff(names(present)[unlist(present)], names(paths))) {
      present[[v]] <- FALSE
      info[[v]]$deleted <- date
    }
  }
  ev_df <- if (length(events)) do.call(rbind, events) else diff_membership(list(), list(), NA, "svn")
  views <- do.call(rbind, lapply(sort(names(info)), function(v) data.frame(
    name = v, topic = info[[v]]$topic, maintainer = info[[v]]$maintainer,
    updated = info[[v]]$updated, last_path = info[[v]]$last_path,
    last_sha = info[[v]]$last_sha, last_date = info[[v]]$last_date,
    deleted = info[[v]]$deleted, stringsAsFactors = FALSE)))
  rownames(ev_df) <- NULL
  list(events = ev_df[SVN_EVENT_COLS], views = views)
}

#' Mark the views that never reached GitHub as retired.
#'
#' Decided once, at extraction, so a short GitHub listing on some later run can
#' never retire a live view. A deleted file is retired on its deletion day; a
#' file still there when the SVN history ends is retired on SVN_ERA_END.
svn_classify_retired <- function(views, github_views, era_end = SVN_ERA_END) {
  gone <- !(views$name %in% github_views)
  views$retired <- ifelse(gone, ifelse(is.na(views$deleted), era_end, views$deleted), NA_character_)
  views
}

.tsv_clean <- function(df) {
  for (col in names(df)) if (is.character(df[[col]])) df[[col]] <- gsub("[\t\r\n]+", " ", df[[col]])
  df
}

write_svn_era <- function(era, dir) {
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  w <- function(df, file) utils::write.table(.tsv_clean(df), file.path(dir, file), sep = "\t",
    quote = FALSE, row.names = FALSE, na = "", fileEncoding = "UTF-8")
  w(era$events[SVN_EVENT_COLS], "svn-era-events.tsv")
  w(era$views[SVN_VIEW_COLS], "svn-era-views.tsv")
  invisible(dir)
}

read_svn_era <- function(dir) {
  r <- function(file) utils::read.delim(file.path(dir, file), sep = "\t", quote = "",
    colClasses = "character", na.strings = "", encoding = "UTF-8", check.names = FALSE)
  events <- r("svn-era-events.tsv")
  events$core <- as.integer(events$core)
  list(events = events[SVN_EVENT_COLS], views = r("svn-era-views.tsv")[SVN_VIEW_COLS])
}

#' Members at the end of some events, as package -> core.
svn_snapshot <- function(events) {
  if (nrow(events) == 0L) return(integer(0))
  m <- derive_membership_from_events(events)
  m <- m[m$currently_member == 1L, , drop = FALSE]
  stats::setNames(as.integer(m$core), m$package)
}

#' A retired view's members leave on its retirement day.
svn_retirement_events <- function(view, members, date) {
  if (length(members) == 0L) return(diff_membership(list(), list(), date, "retired"))
  data.frame(view = view, package = names(members), event_type = "removed",
             event_date = date, core = as.integer(members), source = "retired",
             stringsAsFactors = FALSE)
}

# ---------------------------------------------------------------------------
# The one-time extraction from the archived bundle.
# ---------------------------------------------------------------------------

.utf8 <- function(x) {
  bad <- !validUTF8(x)
  x[bad] <- iconv(x[bad], "latin1", "UTF-8")
  x
}

extract_svn_era <- function(bundle, data_dir, github_views) {
  got <- file_sha256(bundle)
  if (!identical(got, SVN_ARCHIVE_SHA256)) {
    stop(sprintf("bundle sha256 %s does not match the archived %s", got, SVN_ARCHIVE_SHA256))
  }
  repo <- file.path(tempdir(), "ctv-svn-era")
  unlink(repo, recursive = TRUE)
  if (system2("git", c("clone", "--quiet", shQuote(bundle), shQuote(repo))) != 0L) {
    stop("could not clone the bundle")
  }
  git <- function(...) system2("git", c("-C", shQuote(repo), ...), stdout = TRUE)
  log <- git("log", "--first-parent", "--reverse", shQuote("--format=%H|%cs"), "master", "--",
             shQuote("*.ctv"))
  parts <- strsplit(log[nzchar(log)], "|", fixed = TRUE)
  commits <- data.frame(sha = vapply(parts, `[[`, "", 1L),
                        date = vapply(parts, `[[`, "", 2L), stringsAsFactors = FALSE)
  read_tree <- function(sha) {
    lines <- git("ls-tree", "-r", sha)
    lines <- lines[grepl("\\.ctv$", lines)]
    if (length(lines) == 0L) return(character(0))
    stats::setNames(sub("^\\S+\\s+\\S+\\s+(\\S+)\\t.*$", "\\1", lines), sub("^[^\t]*\t", "", lines))
  }
  read_blob <- function(id) paste(.utf8(git("cat-file", "-p", id)), collapse = "\n")
  era <- svn_replay(commits, read_tree, read_blob)
  era$views <- svn_classify_retired(era$views, github_views)
  write_svn_era(era, data_dir)
  # A retired view's final file is kept here, so its link never depends on R-Forge.
  ret <- era$views[!is.na(era$views$retired), , drop = FALSE]
  dir.create(file.path(data_dir, "retired-views"), showWarnings = FALSE)
  for (i in seq_len(nrow(ret))) {
    txt <- .utf8(git("show", paste0(ret$last_sha[i], ":", ret$last_path[i])))
    writeLines(txt, file.path(data_dir, "retired-views", paste0(ret$name[i], ".ctv")), useBytes = TRUE)
  }
  invisible(era)
}

if (sys.nframe() == 0L) {
  .dir <- dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))))
  source(file.path(.dir, "config.R")); source(file.path(.dir, "helpers.R"))
  source(file.path(.dir, "ctv_xml.R"))
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) != 2L || args[1] != "extract") stop("usage: svn_era.R extract <bundle>")
  gh <- system2("gh", c("api", "--paginate", "orgs/cran-task-views/repos", "--jq", shQuote(".[].name")),
                stdout = TRUE)
  github_views <- setdiff(trimws(gh[nzchar(gh)]), CTV_EXCLUDE)
  if (length(github_views) < VIEWS_FLOOR) stop("GitHub listing looks truncated")
  era <- extract_svn_era(args[2], file.path(dirname(.dir), "data"), github_views)
  cat(sprintf("%d events, %d views (%d retired)\n", nrow(era$events), nrow(era$views),
              sum(!is.na(era$views$retired))))
}
