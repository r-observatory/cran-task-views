# scripts/ctv_xml.R: pure parser for the XML task-view sources of the R-Forge era.
#
# Before the 2021/22 move to GitHub, each task view was a `<View>.ctv` XML file
# in the ctv project's SVN on R-Forge. Membership is the <packagelist> block:
# one <pkg> per package, with priority="core" marking a core package. A <pkg>
# inside the <info> prose is a mention, as bioc() or github() are in the
# markdown era, and is ignored. Base R string parsing only, like ctv_md.R.

.xml_unescape <- function(x) {
  x <- gsub("&lt;", "<", x, fixed = TRUE)
  x <- gsub("&gt;", ">", x, fixed = TRUE)
  x <- gsub("&quot;", "\"", x, fixed = TRUE)
  x <- gsub("&apos;", "'", x, fixed = TRUE)
  gsub("&amp;", "&", x, fixed = TRUE)
}

#' Parse membership from a `.ctv` XML source string.
#'
#' Returns a named integer vector package -> core (1L or 0L), integer(0) when
#' the file lists nothing. A package listed twice is core if either listing is.
parse_ctv_xml_membership <- function(text) {
  block <- regmatches(text, regexpr("(?s)<packagelist>.*?</packagelist>", text, perl = TRUE))
  if (length(block) == 0L) return(integer(0))
  tags <- regmatches(block, gregexpr("(?s)<pkg\\b[^>]*>[^<]*</pkg>", block, perl = TRUE))[[1]]
  if (length(tags) == 0L) return(integer(0))
  nm <- trimws(.xml_unescape(sub("(?s)^<pkg\\b[^>]*>([^<]*)</pkg>$", "\\1", tags, perl = TRUE)))
  core <- as.integer(grepl("priority\\s*=\\s*[\"']core[\"']", tags, perl = TRUE))
  keep <- nzchar(nm)
  if (!any(keep)) return(integer(0))
  agg <- tapply(core[keep], nm[keep], max)
  out <- as.integer(agg)
  names(out) <- names(agg)
  out
}

#' Parse the header of a `.ctv` XML source string.
#'
#' Returns list(topic, maintainer, updated); a missing field is NA_character_.
#' The 2005 files have no <version>, and the maintainer's email attribute is
#' dropped.
parse_ctv_xml_header <- function(text) {
  field <- function(tag) {
    rx <- sprintf("(?s)<%s\\b[^>]*>(.*?)</%s>", tag, tag)
    hit <- regmatches(text, regexpr(rx, text, perl = TRUE))
    if (length(hit) == 0L) return(NA_character_)
    val <- sub(rx, "\\1", hit, perl = TRUE)
    val <- trimws(gsub("\\s+", " ", .xml_unescape(gsub("<[^>]*>", "", val))))
    if (nzchar(val)) val else NA_character_
  }
  list(topic = field("topic"), maintainer = field("maintainer"), updated = field("version"))
}
