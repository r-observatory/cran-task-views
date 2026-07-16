# scripts/config.R: constants for the cran-task-views pipeline.
PUBLISH_REPO   <- "r-observatory/cran-task-views"
DB_FILENAME    <- "cran-task-views.db"
# Release manifest describing the finalized primary DB (integrity/completeness
# core + generated_at) so a downstream merge can content-verify what it pulls.
MANIFEST_FILENAME <- "manifest.json"
# The `ctv` repo in available.views() is the toolkit, not a task view.
CTV_EXCLUDE  <- c("ctv")
# Fetch-sanity floor: available.views() returning fewer than this is a truncated
# fetch; abort rather than publish a shrunken catalog. CRAN has published well
# over this many task views for years, so a count below it signals a broken
# or partial fetch rather than a real drop in views.
VIEWS_FLOOR  <- 20L
