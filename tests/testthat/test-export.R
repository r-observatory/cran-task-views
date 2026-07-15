test_that("export_task_views writes the three tables with the documented shape", {
  tmp <- tempfile(fileext = ".db")
  views <- data.frame(name = "Spatial", topic = "Spatial Data",
    maintainer = "Roger Bivand", url = "https://github.com/cran-task-views/Spatial",
    updated = "2026-07-01", stringsAsFactors = FALSE)
  events <- data.frame(view = "Spatial", package = "sf", event_type = "added",
    event_date = "2016-11-01", core = 1L, source = "git", stringsAsFactors = FALSE)
  membership <- data.frame(view = "Spatial", package = "sf", added_date = "2016-11-01",
    removed_date = NA_character_, currently_member = 1L, core = 1L, stringsAsFactors = FALSE)
  export_task_views(tmp, views, events, membership)
  con <- RSQLite::dbConnect(RSQLite::SQLite(), tmp)
  on.exit({ RSQLite::dbDisconnect(con); unlink(tmp) }, add = TRUE)
  tbls <- RSQLite::dbListTables(con)
  expect_true(all(c("cran_task_views","cran_task_view_events","cran_task_view_membership") %in% tbls))
  expect_equal(RSQLite::dbGetQuery(con, "SELECT topic FROM cran_task_views WHERE name='Spatial'")$topic, "Spatial Data")
  expect_equal(RSQLite::dbGetQuery(con, "SELECT currently_member FROM cran_task_view_membership WHERE package='sf'")$currently_member, 1L)
})

test_that("the events idempotency key de-duplicates a re-run of the same event", {
  tmp <- tempfile(fileext = ".db")
  ev <- data.frame(view = rep("Spatial", 2), package = rep("sf", 2), event_type = rep("added", 2),
    event_date = rep("2016-11-01", 2), core = c(1L, 1L), source = rep("git", 2), stringsAsFactors = FALSE)
  export_task_views(tmp,
    data.frame(name="Spatial", topic="s", maintainer="m", url="u", updated="d", stringsAsFactors=FALSE),
    ev,
    data.frame(view="Spatial", package="sf", added_date="2016-11-01", removed_date=NA_character_,
               currently_member=1L, core=1L, stringsAsFactors=FALSE))
  con <- RSQLite::dbConnect(RSQLite::SQLite(), tmp)
  on.exit({ RSQLite::dbDisconnect(con); unlink(tmp) }, add = TRUE)
  expect_equal(RSQLite::dbGetQuery(con, "SELECT COUNT(*) n FROM cran_task_view_events")$n, 1L)
})
