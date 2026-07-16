test_that("the update workflow installs the SQLite deps and publishes to the current release", {
  candidates <- c(
    file.path(getwd(), ".github", "workflows", "update.yml"),
    file.path(getwd(), "..", "..", ".github", "workflows", "update.yml")
  )
  path <- candidates[file.exists(candidates)][1]
  yml <- readLines(path)
  # The pipeline is dependency-light: it uses git + gh + base R, never ctv,
  # knitr, rmarkdown, or a pandoc toolchain.
  expect_false(any(grepl("ctv|knitr|rmarkdown|pandoc", yml)))
  expect_true(any(grepl("any::RSQLite", yml)))
  expect_true(any(grepl("gh release upload current", yml)))
  expect_true(any(grepl("Rscript scripts/update.R", yml)))
  # The integrity manifest is attached to the release alongside the DB.
  expect_true(any(grepl("out/manifest.json", yml)))
})
