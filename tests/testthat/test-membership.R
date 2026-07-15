test_that("diff_membership emits added, removed, and core_change against a prior snapshot", {
  prev <- list(Spatial = c(sf = 1L, sp = 0L))
  curr <- list(Spatial = c(sf = 1L, terra = 0L))   # sp removed, terra added, sf unchanged
  ev <- diff_membership(prev, curr, "2020-01-01", "git")
  expect_setequal(paste(ev$package, ev$event_type),
                  c("terra added", "sp removed"))

  prev2 <- list(Spatial = c(sf = 0L))
  curr2 <- list(Spatial = c(sf = 1L))              # sf promoted to core
  ev2 <- diff_membership(prev2, curr2, "2021-01-01", "git")
  expect_equal(ev2$event_type, "core_change")
  expect_equal(ev2$core, 1L)
})

test_that("derive_membership uses the readd date for a remove-then-readd stint", {
  events <- data.frame(
    view       = rep("Spatial", 3),
    package    = rep("rgeos", 3),
    event_type = c("added", "removed", "added"),
    event_date = c("2015-01-01", "2019-06-01", "2022-03-01"),
    core       = c(0L, 0L, 0L),
    source     = rep("git", 3), stringsAsFactors = FALSE)
  m <- derive_membership_from_events(events)
  expect_equal(m$currently_member, 1L)
  expect_equal(m$added_date, "2022-03-01")   # current stint start, not 2015
  expect_true(is.na(m$removed_date))
})

test_that("derive_membership resolves a same-date remove-then-readd to member by emission order", {
  # A merge or a same-day edit can emit a removed and a re-added event sharing a
  # calendar date. Emission order (removed then re-added) must win, so the
  # package is a current member; a fixed added<removed type rank would wrongly
  # fold the removed last and mark it a non-member.
  events <- data.frame(
    view       = rep("Spatial", 3),
    package    = rep("caret", 3),
    event_type = c("added", "removed", "added"),
    event_date = c("2018-01-01", "2021-05-01", "2021-05-01"),  # remove + readd share a date
    core       = c(0L, 0L, 0L),
    source     = rep("git", 3), stringsAsFactors = FALSE)
  m <- derive_membership_from_events(events)
  expect_equal(m$currently_member, 1L)
  expect_equal(m$added_date, "2021-05-01")   # the readd stint, not the original 2018 add
  expect_true(is.na(m$removed_date))
})

test_that("derive_membership records removal for a package that left and stayed out", {
  events <- data.frame(view = "Spatial", package = "maptools",
    event_type = c("added", "removed"), event_date = c("2010-01-01", "2023-10-16"),
    core = c(0L, 0L), source = "git", stringsAsFactors = FALSE)
  m <- derive_membership_from_events(events)
  expect_equal(m$currently_member, 0L)
  expect_equal(m$removed_date, "2023-10-16")
  expect_equal(m$added_date, "2010-01-01")
})
