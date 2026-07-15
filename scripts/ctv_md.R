# scripts/ctv_md.R: pure parser for CRAN task-view markdown sources.
#
# Each task view is a single markdown file (`<View>.md`) with a YAML front
# matter header followed by an R-Markdown body. Membership is expressed with
# inline R code spans of the form `r pkg("Name")` (or `priority = "core"` for a
# core member). Sibling helpers such as view(), bioc(), github(), rforge(),
# gcode(), ohat() and doi() are references, not membership, and are ignored.
#
# These functions take the raw file text (a single string) and are entirely
# pure: no git, no network, no `ctv` package. That keeps them trivially
# testable with string fixtures and keeps the pipeline dependency-light.

# Split the raw file into its YAML header lines and its markdown body string.
# Only the leading `---` ... `---` block is treated as front matter, so a `---`
# horizontal rule later in the body is left untouched.
.ctv_split_frontmatter <- function(text) {
  text <- gsub("\r\n", "\n", text, fixed = TRUE)
  text <- gsub("\r", "\n", text, fixed = TRUE)
  lines <- strsplit(text, "\n", fixed = TRUE)[[1]]
  i <- 1L
  while (i <= length(lines) && !nzchar(trimws(lines[i]))) i <- i + 1L
  if (i <= length(lines) && grepl("^---\\s*$", lines[i])) {
    j <- i + 1L
    while (j <= length(lines) && !grepl("^---\\s*$", lines[j])) j <- j + 1L
    if (j <= length(lines)) {
      header <- if (j - 1L >= i + 1L) lines[(i + 1L):(j - 1L)] else character(0)
      body   <- if (j < length(lines)) lines[(j + 1L):length(lines)] else character(0)
      return(list(header = header, body = paste(body, collapse = "\n")))
    }
  }
  list(header = character(0), body = paste(lines, collapse = "\n"))
}

#' Parse task-view membership from a `<View>.md` source string.
#'
#' Returns a named integer vector mapping package name -> core flag, where the
#' value is exactly 1L (core) or 0L (non-core). An empty view yields
#' integer(0). A package mentioned more than once is core if ANY mention marks
#' it core.
parse_ctv_membership <- function(text) {
  body <- .ctv_split_frontmatter(text)$body
  if (!nzchar(trimws(body))) return(integer(0))
  # Membership is expressed ONLY inside a single-line inline R code span:
  # a backtick, `r`, a horizontal whitespace, code with no backtick or newline,
  # then a closing backtick, e.g. `r pkg("Name")`. Extract those spans first.
  # A delimiter-less pkg("X") dropped into prose renders literally on CRAN (not a
  # member), and a fenced ```r block spans multiple lines; requiring the closing
  # backtick with no interior backtick/newline excludes both.
  spans <- regmatches(body,
    gregexpr("`r[ \\t][^`\\n]*`", body, perl = TRUE))[[1]]
  if (length(spans) == 0L) return(integer(0))
  # Within each inline span, match pkg( ... ) calls only. The lookbehind rejects
  # a word/dot before "pkg" so mypkg(...) and other.pkg(...) do not match, and
  # sibling helpers such as bioc()/github()/view() never contain the token "pkg".
  # Package arguments hold no nested parentheses, so [^()]* captures a whole call.
  calls <- unlist(regmatches(spans,
    gregexpr("(?<![A-Za-z0-9._])pkg\\s*\\(([^()]*)\\)", spans, perl = TRUE)))
  if (length(calls) == 0L) return(integer(0))

  names_vec <- character(0)
  core_vec  <- integer(0)
  for (call in calls) {
    args <- sub("\\)\\s*$", "", sub("^[^(]*\\(", "", call))
    nm <- regmatches(args, regexpr("[\"'][^\"']+[\"']", args, perl = TRUE))
    if (length(nm) == 0L || !nzchar(nm)) next
    nm <- sub("^[\"']", "", sub("[\"']$", "", nm))
    is_core <- grepl("priority\\s*=\\s*[\"']core[\"']", args, perl = TRUE)
    names_vec <- c(names_vec, nm)
    core_vec  <- c(core_vec, if (is_core) 1L else 0L)
  }
  if (length(names_vec) == 0L) return(integer(0))
  agg <- tapply(core_vec, names_vec, max)      # core = OR across mentions of a name
  out <- as.integer(agg)
  names(out) <- names(agg)
  out
}

#' Parse the metadata header of a `<View>.md` source string.
#'
#' Returns list(topic, maintainer, url, updated) of character scalars. `url`
#' comes from the `source:` field, falling back to the canonical org URL for
#' the view; `updated` comes from `version:` (an ISO date). Missing fields are
#' NA_character_ (topic/maintainer/updated), which downstream code guards.
parse_ctv_header <- function(text, view = NA_character_) {
  header <- .ctv_split_frontmatter(text)$header
  field <- function(name) {
    rx <- sprintf("^\\s*%s\\s*:\\s*(.*)$", name)
    hit <- grep(rx, header, perl = TRUE, value = TRUE)
    if (length(hit) == 0L) return(NA_character_)
    val <- trimws(sub(rx, "\\1", hit[1], perl = TRUE))
    val <- sub("^[\"']", "", sub("[\"']$", "", val))   # unwrap optional quotes
    if (nzchar(val)) val else NA_character_
  }
  url <- field("source")
  if (is.na(url) && !is.na(view)) {
    url <- sprintf("https://github.com/cran-task-views/%s/", view)
  }
  list(
    topic      = field("topic"),
    maintainer = field("maintainer"),
    url        = url,
    updated    = field("version")
  )
}
