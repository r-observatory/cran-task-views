test_that("run_update replays revisions into events, membership, and view metadata", {
  fake_io <- list(
    available_views = function() c("Spatial", "ctv"),   # ctv must be excluded
    view_repo_revisions = function(v) data.frame(
      sha = c("aaa", "bbb"), date = c("2016-11-01", "2020-05-01"), stringsAsFactors = FALSE),
    read_ctv_at = function(v, sha) if (sha == "aaa") c(sp = 1L) else c(sp = 1L, sf = 0L),
    view_info = function(v) list(topic = "Spatial Data", maintainer = "R. Bivand",
      url = "https://github.com/cran-task-views/Spatial", updated = "2020-05-01"))
  out <- tempfile(); dir.create(out)
  on.exit(unlink(out, recursive = TRUE), add = TRUE)
  res <- run_update(fake_io, out, force_full = TRUE)   # force_full skips the VIEWS_FLOOR gate
  expect_equal(res$n_views, 1L)                          # ctv excluded
  con <- RSQLite::dbConnect(RSQLite::SQLite(), file.path(out, "cran-task-views.db"))
  on.exit(RSQLite::dbDisconnect(con), add = TRUE)
  # sp added at first rev; sf added at second rev; both current members.
  mem <- RSQLite::dbGetQuery(con, "SELECT package, added_date, currently_member FROM cran_task_view_membership ORDER BY package")
  expect_equal(mem$package, c("sf", "sp"))
  expect_equal(mem$added_date, c("2020-05-01", "2016-11-01"))
  expect_true(all(mem$currently_member == 1L))
  expect_equal(RSQLite::dbGetQuery(con, "SELECT topic FROM cran_task_views")$topic, "Spatial Data")
})

test_that("run_update aborts on a truncated available.views() fetch", {
  io <- list(available_views = function() c("Spatial"),
             view_repo_revisions = function(v) NULL, read_ctv_at = function(v, s) NULL,
             view_info = function(v) NULL)
  expect_error(run_update(io, tempfile(), force_full = FALSE), "truncated")
})

test_that("the first forward snapshot is diffed against the backfill's final membership: no phantom event at the boundary", {
  # aaa/bbb are the backfill; ccc simulates the first snapshot taken after backfill
  # (a "forward" run). sp and sf are unchanged from bbb to ccc and must NOT emit any
  # add/remove event at ccc's date; only the genuinely new package (terra) should.
  fake_io <- list(
    available_views = function() c("Spatial"),
    view_repo_revisions = function(v) data.frame(
      sha = c("aaa", "bbb", "ccc"),
      date = c("2016-11-01", "2020-05-01", "2021-01-01"), stringsAsFactors = FALSE),
    read_ctv_at = function(v, sha) switch(sha,
      aaa = c(sp = 1L),
      bbb = c(sp = 1L, sf = 0L),
      ccc = c(sp = 1L, sf = 0L, terra = 0L)),
    view_info = function(v) list(topic = "Spatial Data", maintainer = "R. Bivand",
      url = "https://github.com/cran-task-views/Spatial", updated = "2021-01-01"))
  out <- tempfile(); dir.create(out)
  on.exit(unlink(out, recursive = TRUE), add = TRUE)
  res <- run_update(fake_io, out, force_full = TRUE)
  con <- RSQLite::dbConnect(RSQLite::SQLite(), file.path(out, "cran-task-views.db"))
  on.exit(RSQLite::dbDisconnect(con), add = TRUE)

  boundary_events <- RSQLite::dbGetQuery(con,
    "SELECT package, event_type FROM cran_task_view_events WHERE event_date = '2021-01-01'")
  expect_equal(boundary_events$package, "terra")
  expect_equal(boundary_events$event_type, "added")

  mem <- RSQLite::dbGetQuery(con,
    "SELECT package, currently_member FROM cran_task_view_membership ORDER BY package")
  expect_equal(mem$package, c("sf", "sp", "terra"))
  expect_true(all(mem$currently_member == 1L))
})

test_that("a package that goes CRAN-archived but stays listed in the view is not emitted as removed", {
  # pkgA is CRAN-archived between the two revisions but ctv's packagelist still
  # carries it (archived-but-listed union), so read_ctv_at's snapshot is unchanged
  # for pkgA. CRAN active/archived status is a separate concern from view
  # membership and must never manufacture a "removed" event here.
  fake_io <- list(
    available_views = function() c("Spatial"),
    view_repo_revisions = function(v) data.frame(
      sha = c("aaa", "bbb"), date = c("2016-11-01", "2020-05-01"), stringsAsFactors = FALSE),
    read_ctv_at = function(v, sha) c(pkgA = 0L, pkgB = 1L),  # unchanged across the archival
    view_info = function(v) list(topic = "Spatial Data", maintainer = "R. Bivand",
      url = "https://github.com/cran-task-views/Spatial", updated = "2020-05-01"))
  out <- tempfile(); dir.create(out)
  on.exit(unlink(out, recursive = TRUE), add = TRUE)
  run_update(fake_io, out, force_full = TRUE)
  con <- RSQLite::dbConnect(RSQLite::SQLite(), file.path(out, "cran-task-views.db"))
  on.exit(RSQLite::dbDisconnect(con), add = TRUE)

  removed <- RSQLite::dbGetQuery(con,
    "SELECT * FROM cran_task_view_events WHERE package = 'pkgA' AND event_type = 'removed'")
  expect_equal(nrow(removed), 0L)

  mem <- RSQLite::dbGetQuery(con,
    "SELECT currently_member FROM cran_task_view_membership WHERE package = 'pkgA'")
  expect_equal(mem$currently_member, 1L)
})
