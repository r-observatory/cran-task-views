# Pure, network-free tests for the task-view markdown parser (scripts/ctv_md.R).

test_that("parse_ctv_membership captures core and non-core members", {
  txt <- paste(
    "---",
    "name: Demo",
    "topic: Demo View",
    "---",
    "",
    'Base package `r pkg("nnet", priority = "core")` is core, while',
    '`r pkg("RSNNS")` is an ordinary member.',
    sep = "\n")
  m <- parse_ctv_membership(txt)
  expect_equal(sort(names(m)), c("nnet", "RSNNS"))
  expect_equal(m[["nnet"]], 1L)
  expect_equal(m[["RSNNS"]], 0L)
  expect_type(m, "integer")
})

test_that("parse_ctv_membership accepts both quote styles", {
  txt <- paste(
    "---", "name: Demo", "---", "",
    "See `r pkg(\"dq\")` and `r pkg('sq')` here.",
    sep = "\n")
  m <- parse_ctv_membership(txt)
  expect_equal(sort(names(m)), c("dq", "sq"))
  expect_true(all(m == 0L))
})

test_that("parse_ctv_membership tolerates odd whitespace and quote style in priority", {
  txt <- paste(
    "---", "name: Demo", "---", "",
    'A `r pkg( \"a\" , priority = \"core\" )` core member,',
    "B `r pkg('b',priority='core')` also core,",
    'C `r pkg("c",   priority =   "core")` core too.',
    sep = "\n")
  m <- parse_ctv_membership(txt)
  expect_equal(m[["a"]], 1L)
  expect_equal(m[["b"]], 1L)
  expect_equal(m[["c"]], 1L)
})

test_that("a package mentioned twice is core if any mention is core", {
  txt <- paste(
    "---", "name: Demo", "---", "",
    'First `r pkg("dup")` as ordinary, later',
    '`r pkg("dup", priority = "core")` as core.',
    sep = "\n")
  m <- parse_ctv_membership(txt)
  expect_equal(length(m), 1L)
  expect_equal(m[["dup"]], 1L)
})

test_that("sibling helpers view/bioc/github/rforge/doi are not counted as members", {
  txt <- paste(
    "---", "name: Demo", "---", "",
    'Related `r view("Finance")`, Bioc `r bioc("vbmp")`,',
    'dev `r github("user/repo")`, `r rforge("countreg")`,',
    'cited `r doi("10.1000/xyz")`, but `r pkg("realpkg")` is a member.',
    sep = "\n")
  m <- parse_ctv_membership(txt)
  expect_equal(names(m), "realpkg")
  expect_equal(unname(m), 0L)
})

test_that("a mid-word suffix like mypkg() is not mistaken for pkg()", {
  txt <- paste(
    "---", "name: Demo", "---", "",
    'Prose mentioning mypkg() and other.pkg() should be ignored;',
    'only `r pkg("keep")` counts.',
    sep = "\n")
  m <- parse_ctv_membership(txt)
  expect_equal(names(m), "keep")
})

test_that("an empty body yields integer(0)", {
  txt <- paste("---", "name: Demo", "topic: Nothing here", "---", "", "", sep = "\n")
  m <- parse_ctv_membership(txt)
  expect_type(m, "integer")
  expect_length(m, 0L)
})

test_that("a body with no pkg() calls yields integer(0)", {
  txt <- paste("---", "name: Demo", "---", "",
               "Just prose, no inline package spans at all.", sep = "\n")
  expect_length(parse_ctv_membership(txt), 0L)
})

test_that("parse_ctv_header extracts topic, maintainer, url, and updated", {
  txt <- paste(
    "---",
    "name: MachineLearning",
    "topic: Machine Learning & Statistical Learning",
    "maintainer: Torsten Hothorn, Hannah Frick",
    "email: Torsten.Hothorn@R-project.org",
    "version: 2026-06-05",
    "source: https://github.com/cran-task-views/MachineLearning/",
    "---",
    "", "Body `r pkg(\"nnet\")`.",
    sep = "\n")
  info <- parse_ctv_header(txt, view = "MachineLearning")
  expect_equal(info$topic, "Machine Learning & Statistical Learning")
  expect_equal(info$maintainer, "Torsten Hothorn, Hannah Frick")
  expect_equal(info$url, "https://github.com/cran-task-views/MachineLearning/")
  expect_equal(info$updated, "2026-06-05")
})

test_that("parse_ctv_header falls back to the org URL when source is missing", {
  txt <- paste("---", "name: Demo", "topic: Demo View", "---", "", "Body.", sep = "\n")
  info <- parse_ctv_header(txt, view = "Demo")
  expect_equal(info$url, "https://github.com/cran-task-views/Demo/")
  expect_true(is.na(info$maintainer))
  expect_true(is.na(info$updated))
})

test_that("parsing handles CRLF line endings", {
  txt <- paste0("---\r\nname: Demo\r\ntopic: CRLF View\r\n---\r\n\r\n",
                "`r pkg(\"crlf\", priority = \"core\")` core member.\r\n")
  m <- parse_ctv_membership(txt)
  expect_equal(m[["crlf"]], 1L)
  info <- parse_ctv_header(txt, view = "Demo")
  expect_equal(info$topic, "CRLF View")
})
