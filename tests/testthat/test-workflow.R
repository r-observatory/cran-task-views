test_that("the update workflow installs ctv and publishes to the current release", {
  candidates <- c(
    file.path(getwd(), ".github", "workflows", "update.yml"),
    file.path(getwd(), "..", "..", ".github", "workflows", "update.yml")
  )
  path <- candidates[file.exists(candidates)][1]
  yml <- readLines(path)
  expect_true(any(grepl("any::ctv", yml)))
  expect_true(any(grepl("gh release upload current", yml)))
  expect_true(any(grepl("Rscript scripts/update.R", yml)))
})
