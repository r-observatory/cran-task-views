# Integrity / completeness core for the primary published DB (cran-task-views.db).

# Build a tiny, real task-view DB on disk using the canonical schema
# (export_task_views) so the core is computed against genuine SQLite bytes.
build_ctv_db <- function(n_events = 2L) {
  tmp <- tempfile(fileext = ".db")
  views <- data.frame(
    name = "Spatial", topic = "Spatial Data", maintainer = "R. Bivand",
    url = "https://github.com/cran-task-views/Spatial", updated = "2026-07-01",
    stringsAsFactors = FALSE)
  events <- data.frame(
    view = "Spatial", package = paste0("pkg", seq_len(n_events)),
    event_type = "added", event_date = "2016-11-01",
    core = rep(1L, n_events), source = "git", stringsAsFactors = FALSE)
  membership <- data.frame(
    view = "Spatial", package = paste0("pkg", seq_len(n_events)),
    added_date = "2016-11-01", removed_date = NA_character_,
    currently_member = 1L, core = 1L, stringsAsFactors = FALSE)
  export_task_views(tmp, views, events, membership)
  tmp
}

test_that("summary_integrity_core reports filename, bytes, sha256, tables, complete", {
  db <- build_ctv_db(2L)
  on.exit(unlink(db))

  core <- summary_integrity_core(db, complete = TRUE)

  expect_equal(core$db_filename, basename(db))
  # db_bytes is a double (not cast to integer) so files >= ~2 GiB do not
  # overflow to NA; compare against the uncast file.size() directly.
  expect_type(core$db_bytes, "double")
  expect_equal(core$db_bytes, file.size(db))
  # sha256 is lowercase 64-char hex of the exact file bytes
  expect_match(core$db_sha256, "^[0-9a-f]{64}$")
  # tables maps every user table to its row count (no sqlite_% internals)
  expect_equal(
    core$tables,
    list(cran_task_view_events = 2L, cran_task_view_membership = 2L,
         cran_task_views = 1L))
  expect_true(core$complete)
})

test_that("summary_integrity_core sha256 matches an independent digest of the bytes", {
  # Compute the expected hash via an external CLI tool, independent of
  # file_sha256()'s own preferred backend (digest/openssl), so this test
  # genuinely cross-checks the code path instead of re-running the same
  # library. Skip only if neither tool is on PATH (both are expected on CI).
  sha256sum_bin <- Sys.which("sha256sum")
  shasum_bin    <- Sys.which("shasum")
  if (!nzchar(sha256sum_bin) && !nzchar(shasum_bin)) {
    skip("neither sha256sum nor shasum is on PATH")
  }

  db <- build_ctv_db(3L)
  on.exit(unlink(db))

  core <- summary_integrity_core(db)

  if (nzchar(sha256sum_bin)) {
    out <- system2(sha256sum_bin, shQuote(db), stdout = TRUE)
  } else {
    out <- system2(shasum_bin, c("-a", "256", shQuote(db)), stdout = TRUE)
  }
  independent <- tolower(sub("\\s.*$", "", out[1]))

  expect_equal(core$db_sha256, independent)
})

test_that("write_manifest emits generated_at plus the integrity core as top-level fields", {
  db <- build_ctv_db(4L)
  on.exit(unlink(db), add = TRUE)
  core <- summary_integrity_core(db, complete = TRUE)

  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp), add = TRUE)

  write_manifest(tmp, core)

  parsed <- jsonlite::fromJSON(tmp)
  # freshness field present and non-empty
  expect_true(nzchar(parsed$generated_at))
  # integrity/completeness core lives at the TOP level, not nested
  expect_equal(parsed$db_filename, basename(db))
  expect_equal(parsed$db_bytes, file.size(db))
  expect_match(parsed$db_sha256, "^[0-9a-f]{64}$")
  expect_equal(parsed$tables$cran_task_views, 1L)
  expect_equal(parsed$tables$cran_task_view_events, 4L)
  expect_true(parsed$complete)
})

test_that("run_update writes a manifest.json whose core matches the built DB", {
  fake_io <- list(
    available_views = function() c("Spatial", "ctv"),
    view_repo_revisions = function(v) data.frame(
      sha = c("aaa", "bbb"), date = c("2016-11-01", "2020-05-01"),
      stringsAsFactors = FALSE),
    read_ctv_at = function(v, sha) if (sha == "aaa") c(sp = 1L) else c(sp = 1L, sf = 0L),
    view_info = function(v) list(topic = "Spatial Data", maintainer = "R. Bivand",
      url = "https://github.com/cran-task-views/Spatial", updated = "2020-05-01"))
  out <- tempfile(); dir.create(out)
  on.exit(unlink(out, recursive = TRUE), add = TRUE)

  run_update(fake_io, out, force_full = TRUE)

  manifest_path <- file.path(out, "manifest.json")
  db_path       <- file.path(out, "cran-task-views.db")
  expect_true(file.exists(manifest_path))

  parsed <- jsonlite::fromJSON(manifest_path)
  expect_equal(parsed$db_filename, "cran-task-views.db")
  # sha256 in the manifest is the hash of the exact DB bytes on disk
  expect_equal(parsed$db_sha256, file_sha256(db_path))
  expect_equal(parsed$db_bytes, file.size(db_path))
  expect_true(parsed$complete)
  # every user table of the built DB is enumerated
  expect_setequal(
    names(parsed$tables),
    c("cran_task_views", "cran_task_view_events", "cran_task_view_membership"))
})
