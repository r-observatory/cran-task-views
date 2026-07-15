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
