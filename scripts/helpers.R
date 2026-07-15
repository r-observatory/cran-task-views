# scripts/helpers.R: pure helpers for the cran-task-views pipeline.

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

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
  key <- paste(events_df$view, events_df$package, sep = "\r")
  # Stable replay order: by date, then added before core_change before removed on a shared date.
  type_rank <- match(events_df$event_type, c("added", "core_change", "removed"))
  ord <- order(key, events_df$event_date, type_rank)
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
