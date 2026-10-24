ctv_xml <- function(..., core = character(0), topic = "T") {
  pk <- c(...)
  rows <- vapply(pk, function(p)
    if (p %in% core) sprintf('<pkg priority="core">%s</pkg>', p) else sprintf("<pkg>%s</pkg>", p),
    character(1))
  sprintf("<CRANTaskView><topic>%s</topic><maintainer>M</maintainer><packagelist>%s</packagelist></CRANTaskView>",
          topic, paste(rows, collapse = ""))
}

# A fake history: each commit is a tree of path -> blob id, and blobs are texts.
fake_history <- function(trees, blobs, dates) {
  list(
    commits = data.frame(sha = names(trees), date = dates, stringsAsFactors = FALSE),
    read_tree = function(sha) trees[[sha]],
    read_blob = function(id) blobs[[id]])
}

test_that("old view names fold into the current ones", {
  expect_equal(svn_view_name("gR.ctv"), "GraphicalModels")
  expect_equal(svn_view_name("ExtremeValueTheory.ctv"), "ExtremeValue")
  expect_equal(svn_view_name("NumericalMath.ctv"), "NumericalMathematics")
  expect_equal(svn_view_name("Robustness.ctv"), "Robust")
  expect_equal(svn_view_name("hydrology.ctv"), "Hydrology")
  expect_equal(svn_view_name("Spatial.ctv"), "Spatial")
})

test_that("each view resolves to one path, preferring pkg/inst/ctv and the current name", {
  p <- svn_pick_paths(c("pkg/inst/ctv/Spatial.ctv", "inst/ctv/Spatial.ctv",
                        "pkg/inst/ClinicalTrials.ctv", "pkg/inst/ctv/ClinicalTrials.ctv",
                        "pkg/inst/ctv/gR.ctv", "pkg/inst/ctv/GraphicalModels.ctv",
                        "Finance.ctv", "pkg/DESCRIPTION"))
  expect_equal(p[["Spatial"]], "pkg/inst/ctv/Spatial.ctv")
  expect_equal(p[["ClinicalTrials"]], "pkg/inst/ctv/ClinicalTrials.ctv")
  expect_equal(p[["GraphicalModels"]], "pkg/inst/ctv/GraphicalModels.ctv")
  expect_equal(p[["Finance"]], "Finance.ctv")
  expect_equal(sort(names(p)), c("ClinicalTrials", "Finance", "GraphicalModels", "Spatial"))
})

test_that("replay emits additions on first sight and real changes after, but not for a move or a rename", {
  h <- fake_history(
    trees = list(
      c1 = c("inst/ctv/gR.ctv" = "b1"),
      c2 = c("pkg/inst/ctv/gR.ctv" = "b1"),                 # moved, same content
      c3 = c("pkg/inst/ctv/gR.ctv" = "b2"),                 # gRbase added
      c4 = c("pkg/inst/ctv/GraphicalModels.ctv" = "b2")),   # renamed, same content
    blobs = list(b1 = ctv_xml("ggm", core = "ggm"), b2 = ctv_xml("ggm", "gRbase", core = "ggm")),
    dates = c("2005-04-25", "2007-06-08", "2010-01-01", "2021-12-18"))
  r <- svn_replay(h$commits, h$read_tree, h$read_blob)
  ev <- r$events
  expect_true(all(ev$view == "GraphicalModels"))
  expect_equal(paste(ev$package, ev$event_type, ev$event_date),
               c("ggm added 2005-04-25", "gRbase added 2010-01-01"))
  expect_true(all(ev$source == "svn"))
  expect_equal(ev$core[ev$package == "ggm"], 1L)
  v <- r$views[r$views$name == "GraphicalModels", ]
  expect_equal(v$last_path, "pkg/inst/ctv/GraphicalModels.ctv")
  expect_true(is.na(v$deleted))
})

test_that("several commits on one day count once, as the file stood at the end of the day", {
  # Survival on 2009-05-28: a new list, a revert to the old one, then a newer list.
  h <- fake_history(
    trees = list(c0 = c("pkg/inst/ctv/Survival.ctv" = "old"),
                 c1 = c("pkg/inst/ctv/Survival.ctv" = "new"),
                 c2 = c("pkg/inst/ctv/Survival.ctv" = "old"),
                 c3 = c("pkg/inst/ctv/Survival.ctv" = "newer")),
    blobs = list(old = ctv_xml("survival", "Design"), new = ctv_xml("survival", "rms", "flexsurv"),
                 newer = ctv_xml("survival", "rms")),
    dates = c("2008-05-29", "2009-05-28", "2009-05-28", "2009-05-28"))
  ev <- svn_replay(h$commits, h$read_tree, h$read_blob)$events
  day <- ev[ev$event_date == "2009-05-28", ]
  expect_equal(sort(paste(day$package, day$event_type)), c("Design removed", "rms added"))
})

test_that("a deleted file records its date and emits no removals", {
  h <- fake_history(
    trees = list(c1 = c("pkg/inst/ctv/Graphics.ctv" = "b1"), c2 = character(0)),
    blobs = list(b1 = ctv_xml("lattice", "ggplot2")),
    dates = c("2007-06-08", "2021-12-16"))
  r <- svn_replay(h$commits, h$read_tree, h$read_blob)
  expect_equal(sum(r$events$event_type == "removed"), 0L)
  expect_equal(r$views$deleted[r$views$name == "Graphics"], "2021-12-16")
})

test_that("retired views are those with no GitHub repo, dated by deletion or the end of the era", {
  views <- data.frame(name = c("Graphics", "Genetics", "Spatial"),
                      deleted = c("2021-12-16", NA, NA), stringsAsFactors = FALSE)
  out <- svn_classify_retired(views, github_views = c("Spatial"), era_end = "2022-04-03")
  expect_equal(out$retired, c("2021-12-16", "2022-04-03", NA))
})

test_that("the era files survive a write and a read unchanged", {
  dir <- tempfile(); dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  ev <- data.frame(view = "Spatial", package = "sp", event_type = "added",
                   event_date = "2005-05-20", core = 1L, source = "svn", stringsAsFactors = FALSE)
  views <- data.frame(name = "Graphics", topic = "Graphic Displays & Dynamic Graphics",
                      maintainer = "Nicholas Lewin-Koh", updated = NA_character_,
                      last_path = "pkg/inst/ctv/Graphics.ctv", last_sha = "abc",
                      deleted = "2021-12-16", retired = "2021-12-16", stringsAsFactors = FALSE)
  write_svn_era(list(events = ev, views = views), dir)
  back <- read_svn_era(dir)
  expect_equal(back$events, ev)
  expect_equal(back$views, views)
})

test_that("run_update joins the eras at each view's first GitHub commit and keeps retired views", {
  svn <- list(
    events = data.frame(
      view = c("Spatial", "Spatial", "Spatial", "Graphics", "Graphics"),
      package = c("sp", "maptools", "sf", "lattice", "ggplot2"),
      event_type = c("added", "added", "added", "added", "added"),
      event_date = c("2005-05-20", "2005-05-20", "2022-03-01", "2007-06-08", "2009-01-01"),
      core = c(1L, 0L, 0L, 0L, 0L), source = "svn", stringsAsFactors = FALSE),
    views = data.frame(name = c("Spatial", "Graphics"), topic = c("Spatial", "Graphics"),
      maintainer = c("R. Bivand", "N. Lewin-Koh"), updated = c("2022-03-01", "2021-11-01"),
      last_path = c("pkg/inst/ctv/Spatial.ctv", "pkg/inst/ctv/Graphics.ctv"),
      last_sha = c("s1", "g1"), deleted = c(NA, "2021-12-16"),
      retired = c(NA, "2021-12-16"), stringsAsFactors = FALSE))
  fake_io <- list(
    available_views = function() c("Spatial"),
    view_repo_revisions = function(v) data.frame(sha = c("m1", "m2"),
      date = c("2021-12-28", "2022-06-01"), stringsAsFactors = FALSE),
    # m1 is the converted list (sp, maptools, as the XML had before the cutover);
    # sf only arrives on GitHub at m2. The XML sf of 2022-03-01 is after the cutover.
    read_ctv_at = function(v, sha) if (sha == "m1") c(maptools = 0L, sp = 1L)
                                  else c(maptools = 0L, sf = 0L, sp = 1L),
    view_info = function(v) list(topic = "Spatial Data", maintainer = "R. Bivand",
      url = "https://github.com/cran-task-views/Spatial/", updated = "2022-06-01"),
    svn_era = function() svn)
  out <- tempfile(); dir.create(out)
  on.exit(unlink(out, recursive = TRUE), add = TRUE)
  run_update(fake_io, out, force_full = TRUE)
  con <- RSQLite::dbConnect(RSQLite::SQLite(), file.path(out, "cran-task-views.db"))
  on.exit(RSQLite::dbDisconnect(con), add = TRUE)

  sp <- RSQLite::dbGetQuery(con, "SELECT package, event_type, event_date, source
    FROM cran_task_view_events WHERE view = 'Spatial' ORDER BY event_date, package")
  expect_equal(paste(sp$package, sp$event_date, sp$source),
               c("maptools 2005-05-20 svn", "sp 2005-05-20 svn", "sf 2022-06-01 git"))
  mem <- RSQLite::dbGetQuery(con, "SELECT package, added_date FROM cran_task_view_membership
    WHERE view = 'Spatial' ORDER BY package")
  expect_equal(mem$added_date, c("2005-05-20", "2022-06-01", "2005-05-20"))

  v <- RSQLite::dbGetQuery(con, "SELECT name, retired, url FROM cran_task_views ORDER BY name")
  expect_equal(v$name, c("Graphics", "Spatial"))
  expect_equal(v$retired, c("2021-12-16", NA))
  expect_match(v$url[1], "data/retired-views/Graphics.ctv$")

  g <- RSQLite::dbGetQuery(con, "SELECT package, event_type, event_date, source
    FROM cran_task_view_events WHERE view = 'Graphics' AND source = 'retired' ORDER BY package")
  expect_equal(paste(g$package, g$event_type, g$event_date),
               c("ggplot2 removed 2021-12-16", "lattice removed 2021-12-16"))
  gm <- RSQLite::dbGetQuery(con, "SELECT package, currently_member, removed_date
    FROM cran_task_view_membership WHERE view = 'Graphics' ORDER BY package")
  expect_true(all(gm$currently_member == 0L))
  expect_true(all(gm$removed_date == "2021-12-16"))
})

test_that("a view missing from a short GitHub listing is skipped, never retired", {
  svn <- list(
    events = data.frame(view = "Spatial", package = "sp", event_type = "added",
      event_date = "2005-05-20", core = 1L, source = "svn", stringsAsFactors = FALSE),
    views = data.frame(name = "Spatial", topic = "Spatial", maintainer = "R. Bivand",
      updated = NA_character_, last_path = "pkg/inst/ctv/Spatial.ctv", last_sha = "s1",
      deleted = NA_character_, retired = NA_character_, stringsAsFactors = FALSE))
  fake_io <- list(available_views = function() character(0),
    view_repo_revisions = function(v) NULL, read_ctv_at = function(v, s) integer(0),
    view_info = function(v) list(), svn_era = function() svn)
  out <- tempfile(); dir.create(out)
  on.exit(unlink(out, recursive = TRUE), add = TRUE)
  run_update(fake_io, out, force_full = TRUE)
  con <- RSQLite::dbConnect(RSQLite::SQLite(), file.path(out, "cran-task-views.db"))
  on.exit(RSQLite::dbDisconnect(con), add = TRUE)
  expect_equal(RSQLite::dbGetQuery(con, "SELECT COUNT(*) n FROM cran_task_views")$n, 0L)
  expect_equal(RSQLite::dbGetQuery(con, "SELECT COUNT(*) n FROM cran_task_view_events")$n, 0L)
})
