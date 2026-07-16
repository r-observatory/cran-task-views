# scripts/helpers.R: pure helpers for the cran-task-views pipeline.

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

#' Compute the lowercase hex SHA-256 of a file's exact on-disk bytes.
#'
#' Uses whatever the runner already provides, in preference order:
#'   1. digest  package        (if installed)
#'   2. openssl package        (if installed)
#'   3. sha256sum (coreutils)  - present on the ubuntu-latest CI runner
#'   4. shasum -a 256 (BSD)    - macOS/local fallback
#' No heavy dependency is declared: on CI (which installs only RSQLite, DBI,
#' jsonlite, testthat, withr) the coreutils `sha256sum` path is used. If a
#' sibling pipeline already declares `digest`, that path wins automatically.
file_sha256 <- function(path) {
  if (requireNamespace("digest", quietly = TRUE)) {
    return(tolower(digest::digest(file = path, algo = "sha256")))
  }
  if (requireNamespace("openssl", quietly = TRUE)) {
    con <- file(path, open = "rb")
    on.exit(close(con), add = TRUE)
    return(tolower(as.character(openssl::sha256(con))))
  }
  sha_tool <- Sys.which("sha256sum")
  if (nzchar(sha_tool)) {
    out <- system2(sha_tool, shQuote(path), stdout = TRUE)
    return(tolower(sub("\\s.*$", "", out[1])))
  }
  shasum_tool <- Sys.which("shasum")
  if (nzchar(shasum_tool)) {
    out <- system2(shasum_tool, c("-a", "256", shQuote(path)), stdout = TRUE)
    return(tolower(sub("\\s.*$", "", out[1])))
  }
  stop("No SHA-256 backend found (need one of: digest, openssl, sha256sum, shasum)")
}

#' Build the integrity / completeness core describing a finalized SQLite file.
#'
#' Returns a named list of TOP-LEVEL manifest fields computed from the exact
#' on-disk bytes of `db_path` (call this only after the file is finalized and
#' its DB connection closed):
#'   * db_filename - basename of the file
#'   * db_bytes    - byte size of the file as a double. Deliberately NOT cast
#'                   to integer: R's integer range is 32-bit and overflows to
#'                   NA (serialized as the string "NA") for files >= ~2 GiB.
#'   * db_sha256   - lowercase hex sha256 of the file's exact bytes
#'   * tables      - named list mapping each user table to its row count
#'   * complete    - passed through by the caller. complete = the DB holds the
#'                   full, non-partial dataset (full-not-partial), NOT freshness:
#'                   freshness is tracked separately via generated_at and the
#'                   db_sha256 fingerprint. A pipeline with a genuine
#'                   partial/bootstrap state would DERIVE this (e.g. remaining ==
#'                   0) instead of hardcoding it; this pipeline always performs a
#'                   full deterministic replay, so the caller passes TRUE.
#' Lets a downstream merge content-verify the asset it pulls and confirm the
#' expected tables/rows are present.
summary_integrity_core <- function(db_path, complete = TRUE) {
  stopifnot(file.exists(db_path))

  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  tables <- tryCatch({
    tbl_names <- DBI::dbGetQuery(con, "
      SELECT name FROM sqlite_master
       WHERE type = 'table' AND name NOT LIKE 'sqlite_%'
       ORDER BY name")$name

    stats::setNames(
      lapply(tbl_names, function(t) {
        DBI::dbGetQuery(con, sprintf('SELECT count(*) AS n FROM "%s"', t))$n
      }),
      tbl_names
    )
  }, finally = DBI::dbDisconnect(con))

  # db_bytes/db_sha256 read the raw on-disk file only after the connection
  # above is closed, so no open handle or journal file skews the hash/size.
  list(
    db_filename = basename(db_path),
    db_bytes    = file.size(db_path),
    db_sha256   = file_sha256(db_path),
    tables      = tables,
    complete    = complete
  )
}

#' Write the release manifest.json describing the finalized primary DB.
#'
#' Top-level fields: generated_at plus the integrity/completeness core produced
#' by summary_integrity_core(). `core` is merged as TOP-LEVEL fields (not nested)
#' so a downstream merge can read db_filename/db_bytes/db_sha256/tables/complete
#' directly. generated_at records freshness independently of `complete`.
write_manifest <- function(path, core,
                           generated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ",
                                                 tz = "UTC")) {
  obj <- c(list(generated_at = generated_at), core)
  json <- jsonlite::toJSON(obj, auto_unbox = TRUE, pretty = TRUE, null = "null")
  writeLines(json, path)
  invisible(path)
}

#' Export the assembled task-view tables to a fresh SQLite database.
export_task_views <- function(path, views_df, events_df, membership_df) {
  if (file.exists(path)) unlink(path)
  con <- RSQLite::dbConnect(RSQLite::SQLite(), path)
  on.exit(RSQLite::dbDisconnect(con), add = TRUE)

  RSQLite::dbExecute(con, "
    CREATE TABLE cran_task_views (
      name       TEXT PRIMARY KEY,
      topic      TEXT,
      maintainer TEXT,
      url        TEXT,
      updated    TEXT
    )")
  RSQLite::dbExecute(con, "
    CREATE TABLE cran_task_view_events (
      view       TEXT NOT NULL,
      package    TEXT NOT NULL,
      event_type TEXT NOT NULL,
      event_date TEXT,
      core       INTEGER,
      source     TEXT
    )")
  RSQLite::dbExecute(con,
    "CREATE UNIQUE INDEX ux_ctv_events ON cran_task_view_events(view, package, event_type, event_date)")
  RSQLite::dbExecute(con,
    "CREATE INDEX idx_ctv_events_view ON cran_task_view_events(view)")
  RSQLite::dbExecute(con, "
    CREATE TABLE cran_task_view_membership (
      view             TEXT NOT NULL,
      package          TEXT NOT NULL,
      added_date       TEXT,
      removed_date     TEXT,
      currently_member INTEGER,
      core             INTEGER,
      PRIMARY KEY (view, package)
    )")
  RSQLite::dbExecute(con,
    "CREATE INDEX idx_ctv_membership_pkg ON cran_task_view_membership(package)")

  RSQLite::dbWriteTable(con, "cran_task_views", views_df, append = TRUE)
  # Enforce the idempotency key before writing so a re-run of the same event is a no-op.
  ev <- events_df[!duplicated(events_df[c("view", "package", "event_type", "event_date")]), , drop = FALSE]
  RSQLite::dbWriteTable(con, "cran_task_view_events", ev, append = TRUE)
  RSQLite::dbWriteTable(con, "cran_task_view_membership", membership_df, append = TRUE)
  RSQLite::dbExecute(con, "VACUUM")
  invisible(NULL)
}

#' Diff two membership snapshots into add/remove/core_change events.
#' prev, curr: named lists view -> named integer vector (package -> core 0/1).
diff_membership <- function(prev, curr, event_date, source) {
  empty <- data.frame(view = character(0), package = character(0), event_type = character(0),
                      event_date = character(0), core = integer(0), source = character(0),
                      stringsAsFactors = FALSE)
  ev <- list()
  for (v in union(names(prev), names(curr))) {
    p <- prev[[v]] %||% integer(0)
    c <- curr[[v]] %||% integer(0)
    for (pkg in setdiff(names(c), names(p)))
      ev[[length(ev) + 1L]] <- data.frame(view = v, package = pkg, event_type = "added",
        event_date = event_date, core = as.integer(c[[pkg]]), source = source, stringsAsFactors = FALSE)
    for (pkg in setdiff(names(p), names(c)))
      ev[[length(ev) + 1L]] <- data.frame(view = v, package = pkg, event_type = "removed",
        event_date = event_date, core = as.integer(p[[pkg]]), source = source, stringsAsFactors = FALSE)
    for (pkg in intersect(names(p), names(c)))
      if (!identical(as.integer(p[[pkg]]), as.integer(c[[pkg]])))
        ev[[length(ev) + 1L]] <- data.frame(view = v, package = pkg, event_type = "core_change",
          event_date = event_date, core = as.integer(c[[pkg]]), source = source, stringsAsFactors = FALSE)
  }
  if (length(ev) == 0L) return(empty)
  do.call(rbind, ev)
}

#' Derive current membership from the append-only event log.
derive_membership_from_events <- function(events_df) {
  cols  <- c("view", "package", "added_date", "removed_date", "currently_member", "core")
  empty <- data.frame(view = character(0), package = character(0), added_date = character(0),
                      removed_date = character(0), currently_member = integer(0), core = integer(0),
                      stringsAsFactors = FALSE)
  if (nrow(events_df) == 0L) return(empty)
  # Emission order is the true chronological mainline order: revisions are walked
  # oldest-first and their events are appended in order, so row order already
  # encodes commit order. Carry it as a per-event sequence index and replay each
  # package's events in that order. Sorting on the sequence (not a fixed
  # added<core_change<removed type rank) is what lets a same-day remove-then-readd
  # resolve to member instead of the removed always folding last. The .seq column
  # is internal and never reaches the exported schema.
  events_df$.seq <- seq_len(nrow(events_df))
  key <- paste(events_df$view, events_df$package, sep = "\r")
  ord <- order(key, events_df$.seq)
  events_df <- events_df[ord, , drop = FALSE]; key <- key[ord]
  rows <- list()
  for (k in unique(key)) {
    grp <- events_df[key == k, , drop = FALSE]
    member <- FALSE; stint_start <- NA_character_; removed <- NA_character_; core <- 0L
    for (i in seq_len(nrow(grp))) {
      et <- grp$event_type[i]; ed <- grp$event_date[i]
      if (et == "added") {
        if (!member) { member <- TRUE; stint_start <- ed; removed <- NA_character_ }
        if (!is.na(grp$core[i])) core <- as.integer(grp$core[i])
      } else if (et == "removed") {
        if (member) { member <- FALSE; removed <- ed }
      } else if (et == "core_change") {
        if (!is.na(grp$core[i])) core <- as.integer(grp$core[i])
      }
    }
    rows[[length(rows) + 1L]] <- data.frame(view = grp$view[1], package = grp$package[1],
      added_date = stint_start, removed_date = if (member) NA_character_ else removed,
      currently_member = if (member) 1L else 0L, core = core, stringsAsFactors = FALSE)
  }
  out <- do.call(rbind, rows); rownames(out) <- NULL
  out[order(out$view, out$package), cols, drop = FALSE]
}
