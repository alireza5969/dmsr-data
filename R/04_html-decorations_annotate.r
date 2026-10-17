# =============================================================================
# annotate_html.R
# Highlight / recolour keywords in a LOCAL html file by editing TEXT NODES only.
#
# Packages: xml2, stringr, dplyr, tibble
#
# Policy summary (details in the roxygen block):
#   * only text nodes are inspected - tag names, attributes, hrefs, src, script,
#     style, <head>, <title>, <textarea>, <select>/<option>, ... are never touched
#   * a match must lie inside ONE text node ("syntax <i>highlighting</i>" is two
#     text nodes, so the phrase "syntax highlighting" is NOT matched across the
#     <i> boundary; such matches are skipped rather than restructuring markup)
#   * overlaps: longest match wins; ties -> earlier keyword in the table; then
#     earlier position. Accepted matches never overlap.
#   * idempotent: text already inside an element carrying data-kwa-mark is skipped
#   * every text node that receives >= 1 match is replaced by
#       <span data-kwa-wrap="1">plain<mark data-kwa-mark="1" ...>match</mark>plain</span>
#     (or by the bare <mark>/<span> when the whole text node is the match)
#   * integrity guard: the concatenated text of the document, every attribute of
#     every pre-existing element, and the multiset of pre-existing element names
#     must be IDENTICAL before/after, otherwise nothing is written.
#
# NOTE: authored without the ability to execute R; see tests_and_demo.R.
# =============================================================================

library(xml2)
library(stringr)
library(dplyr)
library(tibble)

.KWA_SKIP_TAGS <- c("script", "style", "noscript", "template", "textarea", "title",
                    "head", "select", "option", "datalist", "iframe", "object", "svg")

.CSS_COLORS <- c(
  "aliceblue","antiquewhite","aqua","aquamarine","azure","beige","bisque","black",
  "blanchedalmond","blue","blueviolet","brown","burlywood","cadetblue","chartreuse",
  "chocolate","coral","cornflowerblue","cornsilk","crimson","cyan","darkblue","darkcyan",
  "darkgoldenrod","darkgray","darkgreen","darkgrey","darkkhaki","darkmagenta",
  "darkolivegreen","darkorange","darkorchid","darkred","darksalmon","darkseagreen",
  "darkslateblue","darkslategray","darkslategrey","darkturquoise","darkviolet","deeppink",
  "deepskyblue","dimgray","dimgrey","dodgerblue","firebrick","floralwhite","forestgreen",
  "fuchsia","gainsboro","ghostwhite","gold","goldenrod","gray","green","greenyellow","grey",
  "honeydew","hotpink","indianred","indigo","ivory","khaki","lavender","lavenderblush",
  "lawngreen","lemonchiffon","lightblue","lightcoral","lightcyan","lightgoldenrodyellow",
  "lightgray","lightgreen","lightgrey","lightpink","lightsalmon","lightseagreen",
  "lightskyblue","lightslategray","lightslategrey","lightsteelblue","lightyellow","lime",
  "limegreen","linen","magenta","maroon","mediumaquamarine","mediumblue","mediumorchid",
  "mediumpurple","mediumseagreen","mediumslateblue","mediumspringgreen","mediumturquoise",
  "mediumvioletred","midnightblue","mintcream","mistyrose","moccasin","navajowhite","navy",
  "oldlace","olive","olivedrab","orange","orangered","orchid","palegoldenrod","palegreen",
  "paleturquoise","palevioletred","papayawhip","peachpuff","peru","pink","plum","powderblue",
  "purple","rebeccapurple","red","rosybrown","royalblue","saddlebrown","salmon","sandybrown",
  "seagreen","seashell","sienna","silver","skyblue","slateblue","slategray","slategrey",
  "snow","springgreen","steelblue","tan","teal","thistle","tomato","turquoise","violet",
  "wheat","white","whitesmoke","yellow","yellowgreen","transparent","currentcolor")

# CSS named colour, #hex, or rgb()/rgba()/hsl()/hsla() whose argument list can only
# contain digits, letters, spaces and . % + , / - ; no ';', ':', quotes or
# nested parentheses, so a validated value cannot break out of style="...".
.valid_css_color <- function(x) {
  x <- tolower(stringr::str_trim(as.character(x)))
  named <- x %in% .CSS_COLORS
  hex <- stringr::str_detect(x, "^#(?:[0-9a-f]{3,4}|[0-9a-f]{6}|[0-9a-f]{8})$")
  fn  <- stringr::str_detect(x, "^(?:rgb|rgba|hsl|hsla)\\(\\s*[0-9a-z.%+,/\\s-]*\\)$")
  dplyr::coalesce(named | hex | fn, FALSE)
}

.kwa_re_escape <- function(x) {
  x <- stringr::str_replace_all(x, "[.^$|?*+()\\[\\]{}\\\\-]", "\\\\\\0")
  stringr::str_replace_all(x, "\\s+", "\\\\s+")      # any whitespace run matches
}
.kwa_xml_esc  <- function(x) {
  x <- stringr::str_replace_all(x, "&", "&amp;")
  x <- stringr::str_replace_all(x, "<", "&lt;")
  stringr::str_replace_all(x, ">", "&gt;")
}
.kwa_attr_esc <- function(x) stringr::str_replace_all(.kwa_xml_esc(x), "\"", "&quot;")

# Returns list(table = tibble(keyword, color, pattern, input_order), warnings = chr)
.kwa_validate_keywords <- function(keywords, default_color, regex, ignore_case, whole_word) {
  warns <- character()
  if (!inherits(keywords, "data.frame"))
    stop("`keywords` must be a data frame / tibble with columns `keyword` and `color`.", call. = FALSE)
  miss <- setdiff(c("keyword", "color"), names(keywords))
  if (length(miss)) stop("`keywords` is missing column(s): ", paste(miss, collapse = ", "), call. = FALSE)

  kw <- tibble::tibble(input_order = seq_len(nrow(keywords)),
                       keyword = stringr::str_trim(as.character(keywords$keyword)),
                       color = stringr::str_trim(as.character(keywords$color)))
  empty <- is.na(kw$keyword) | !nzchar(kw$keyword)
  if (any(empty)) warns <- c(warns, sprintf("%d row(s) with missing/empty keyword dropped", sum(empty)))
  kw <- kw[!empty, , drop = FALSE]

  no_col <- is.na(kw$color) | !nzchar(kw$color)
  if (any(no_col)) {
    warns <- c(warns, sprintf("%d keyword(s) without colour use default '%s': %s",
                              sum(no_col), default_color, paste(kw$keyword[no_col], collapse = ", ")))
    kw$color[no_col] <- default_color
  }
  bad <- !.valid_css_color(kw$color)
  if (any(bad)) {
    warns <- c(warns, sprintf("keyword(s) with invalid CSS colour dropped: %s",
                              paste0(kw$keyword[bad], " [", kw$color[bad], "]", collapse = ", ")))
    kw <- kw[!bad, , drop = FALSE]
  }
  kw$color <- tolower(kw$color)

  key <- if (ignore_case) tolower(kw$keyword) else kw$keyword
  dup <- duplicated(key)
  if (any(dup)) {
    first_col <- kw$color[match(key[dup], key)]
    conflict <- first_col != kw$color[dup]
    warns <- c(warns, sprintf("%d duplicate keyword row(s) dropped (first occurrence kept)%s",
                              sum(dup), if (any(conflict)) "; some had a DIFFERENT colour" else ""))
    kw <- kw[!dup, , drop = FALSE]
  }

  kw$pattern <- if (regex) kw$keyword else .kwa_re_escape(kw$keyword)
  if (whole_word) kw$pattern <- paste0("(?<![\\p{L}\\p{N}_])(?:", kw$pattern, ")(?![\\p{L}\\p{N}_])")

  ok <- vapply(kw$pattern, function(p) {
    tryCatch({ stringr::str_detect("", stringr::regex(p, ignore_case = ignore_case)); TRUE },
             error = function(e) FALSE)
  }, logical(1))
  if (any(!ok)) {
    warns <- c(warns, sprintf("invalid regular expression dropped: %s", paste(kw$keyword[!ok], collapse = ", ")))
    kw <- kw[ok, , drop = FALSE]
  }
  if (nrow(kw) == 0L) stop("No valid keywords left after validation.", call. = FALSE)
  list(table = kw, warnings = warns)
}

# Longest match first; ties -> earlier keyword (table order) -> earlier start.
# Accepted matches never overlap.
.kwa_select_matches <- function(txt, pats, ignore_case) {
  parts <- vector("list", length(pats))
  for (j in seq_along(pats)) {
    loc <- stringr::str_locate_all(txt, stringr::regex(pats[j], ignore_case = ignore_case))[[1]]
    if (nrow(loc)) {
      loc <- loc[loc[, "end"] >= loc[, "start"], , drop = FALSE]      # drop empty matches
      if (nrow(loc)) parts[[j]] <- data.frame(start = loc[, "start"], end = loc[, "end"], kw = j)
    }
  }
  cd <- do.call(rbind, parts)
  if (is.null(cd) || nrow(cd) == 0L) return(NULL)
  cd <- cd[order(-(cd$end - cd$start), cd$kw, cd$start), , drop = FALSE]
  keep <- logical(nrow(cd)); acc_s <- integer(); acc_e <- integer()
  for (r in seq_len(nrow(cd))) {
    if (!any(cd$start[r] <= acc_e & cd$end[r] >= acc_s)) {
      keep[r] <- TRUE; acc_s <- c(acc_s, cd$start[r]); acc_e <- c(acc_e, cd$end[r])
    }
  }
  cd <- cd[keep, , drop = FALSE]
  cd[order(cd$start), , drop = FALSE]
}

.kwa_snapshot <- function(doc) {
  own <- "//*[not(@data-kwa-mark) and not(@data-kwa-wrap)]"
  a <- xml2::xml_find_all(doc, paste0(own, "/@*"))
  list(text  = xml2::xml_text(doc),
       elems = xml2::xml_name(xml2::xml_find_all(doc, own)),
       attrs = paste(xml2::xml_name(a), xml2::xml_text(a), sep = "="))
}

.kwa_write <- function(doc, target, overwrite_ok, backup, expect_marks) {
  tmp <- tempfile(pattern = ".kwa_tmp_", tmpdir = dirname(target), fileext = ".html")
  on.exit(if (file.exists(tmp)) unlink(tmp), add = TRUE)
  xml2::write_html(doc, tmp, options = "as_html")          # no "format": do not re-indent
  if (!file.exists(tmp) || file.size(tmp) == 0L) stop("serialisation produced an empty file")
  chk <- xml2::read_html(tmp, options = c("RECOVER", "NOERROR", "NOWARNING", "HUGE"))
  n_back <- length(xml2::xml_find_all(chk, "//*[@data-kwa-mark]"))
  if (n_back != expect_marks)
    stop(sprintf("re-parse check failed: %d annotations expected, %d found", expect_marks, n_back))
  if (file.exists(target)) {
    if (!overwrite_ok) stop("`output` exists; set overwrite = TRUE", call. = FALSE)
    if (backup) {
      bak <- paste0(target, ".bak")
      if (file.exists(bak)) bak <- paste0(target, ".", format(Sys.time(), "%Y%m%d%H%M%S"), ".bak")
      if (!file.copy(target, bak)) stop("could not create backup file")
    }
  }
  ok <- suppressWarnings(file.rename(tmp, target))          # atomic on the same volume
  if (!ok) { ok <- file.copy(tmp, target, overwrite = TRUE); unlink(tmp) }
  if (!ok) stop("could not move the new file into place; the original is unchanged")
  invisible(target)
}

#' Highlight or recolour keywords in a local HTML file (text nodes only)
#'
#' @param input     path of a LOCAL html file (URLs are rejected).
#' @param keywords  data frame with columns `keyword` and `color` (CSS colour:
#'   named colour, #hex, rgb()/rgba()/hsl()/hsla()). Rows with missing keyword or
#'   invalid colour are dropped with a warning; missing colour -> `default_color`;
#'   duplicate keywords (case-insensitively when ignore_case) keep the FIRST row.
#' @param mode      "highlight" -> <mark> with background-color;
#'                  "color"     -> <span> with foreground color.
#' @param output    optional path to write the result to (refuses to overwrite an
#'   existing file unless overwrite = TRUE). With output = NULL and
#'   in_place = FALSE nothing is written; the modified document is returned.
#' @param in_place  TRUE = replace `input` (via temp file + rename; original kept
#'   if anything fails; backup = TRUE keeps a .bak copy).
#' @param ignore_case,whole_word,regex  matching options. Keywords are literal
#'   strings unless regex = TRUE (ICU regular expressions). In literal mode any
#'   whitespace run in a keyword matches any whitespace run in the text.
#' @param skip_tags tag names whose descendants are never annotated.
#' @return invisible list: output, counts (tibble per keyword), n_text_nodes_modified,
#'   n_annotations, warnings, checks, doc (only when nothing was written).
#' Serialiser notes: output is produced by libxml2's HTML writer. Expect whitespace
#'   between tags, attribute quoting, entity forms (&nbsp; vs U+00A0) and
#'   self-closing syntax to be normalised; the document's text, elements and
#'   attribute values are verified unchanged (see `checks`).
annotate_html <- function(input, keywords, mode = c("highlight", "color"),
                          output = NULL, in_place = FALSE, overwrite = FALSE,
                          backup = FALSE, ignore_case = TRUE, whole_word = FALSE,
                          regex = FALSE, skip_tags = .KWA_SKIP_TAGS,
                          default_color = NULL, quiet = TRUE) {
  mode <- match.arg(mode)
  if (is.null(default_color)) default_color <- if (mode == "highlight") "yellow" else "red"
  if (!.valid_css_color(default_color)) stop("`default_color` is not a valid CSS colour", call. = FALSE)

  # ---- input / output validation (before touching anything) ----
  if (!is.character(input) || length(input) != 1L || is.na(input))
    stop("`input` must be a single file path", call. = FALSE)
  if (grepl("^[a-z][a-z0-9+.-]*://", input, ignore.case = TRUE))
    stop("URLs are not supported: `input` must be a LOCAL html file", call. = FALSE)
  if (!file.exists(input) || dir.exists(input)) stop("File not found: ", input, call. = FALSE)
  if (in_place && !is.null(output) && normalizePath(output, mustWork = FALSE) != normalizePath(input))
    stop("Use either in_place = TRUE or `output`, not both", call. = FALSE)
  target <- if (in_place) input else output
  if (!is.null(target) && !in_place && file.exists(target) && !overwrite)
    stop("`output` already exists (", target, "); use overwrite = TRUE", call. = FALSE)
  if (!is.null(target) && !dir.exists(dirname(target)))
    stop("Directory of the target does not exist: ", dirname(target), call. = FALSE)
  skip_tags <- tolower(skip_tags)
  if (!all(grepl("^[a-z][a-z0-9-]*$", skip_tags))) stop("invalid `skip_tags`", call. = FALSE)

  kv <- .kwa_validate_keywords(keywords, default_color, regex, ignore_case, whole_word)
  kw <- kv$table; warns <- kv$warnings

  doc <- tryCatch(xml2::read_html(input, options = c("RECOVER", "NOERROR", "NOWARNING", "HUGE")),
                  error = function(e) stop("Could not parse '", input, "': ", conditionMessage(e), call. = FALSE))
  if (inherits(xml2::xml_find_first(doc, "/*"), "xml_missing"))
    stop("Parsed document is empty: ", input, call. = FALSE)

  before <- .kwa_snapshot(doc)

  xp <- sprintf("//text()[not(%s)][not(ancestor::*[@data-kwa-mark])][not(ancestor::*[@hidden])][normalize-space(.) != '']",
                paste(sprintf("ancestor::%s", skip_tags), collapse = " or "))
  nodes <- xml2::xml_find_all(doc, xp)
  txt <- xml2::xml_text(nodes)
  combined <- stringr::regex(paste0("(?:", paste(kw$pattern, collapse = "|"), ")"), ignore_case = ignore_case)
  hit <- which(dplyr::coalesce(stringr::str_detect(txt, combined), FALSE))

  counts <- integer(nrow(kw)); n_nodes <- 0L; n_marks <- 0L
  for (i in hit) {
    t <- txt[i]
    if (stringr::str_detect(t, "[\\x00-\\x08\\x0B\\x0C\\x0E-\\x1F]")) {
      warns <- c(warns, "a text node with control characters was skipped"); next
    }
    sel <- .kwa_select_matches(t, kw$pattern, ignore_case)
    if (is.null(sel)) next
    pieces <- character(); pos <- 1L
    for (r in seq_len(nrow(sel))) {
      s <- sel$start[r]; e <- sel$end[r]; j <- sel$kw[r]
      if (s > pos) pieces <- c(pieces, .kwa_xml_esc(stringr::str_sub(t, pos, s - 1L)))
      shown <- .kwa_xml_esc(stringr::str_sub(t, s, e))
      attrs <- sprintf("class=\"kwa-mark\" data-kwa-mark=\"1\" data-kwa-keyword=\"%s\" style=\"%s: %s\"",
                       .kwa_attr_esc(kw$keyword[j]),
                       if (mode == "highlight") "background-color" else "color", kw$color[j])
      pieces <- c(pieces, sprintf("<%s %s>%s</%s>", if (mode == "highlight") "mark" else "span", attrs, shown,
                                  if (mode == "highlight") "mark" else "span"))
      pos <- e + 1L
    }
    if (pos <= nchar(t)) pieces <- c(pieces, .kwa_xml_esc(stringr::str_sub(t, pos)))
    frag <- if (length(pieces) == 1L) pieces else paste0("<span data-kwa-wrap=\"1\">", paste(pieces, collapse = ""), "</span>")
    ok <- tryCatch({
      new <- xml2::xml_root(xml2::read_xml(frag, options = "HUGE"))
      if (!identical(xml2::xml_text(new), t)) stop("replacement text differs from original")
      xml2::xml_add_sibling(nodes[[i]], new, .where = "before", .copy = TRUE)
      xml2::xml_remove(nodes[[i]])
      TRUE
    }, error = function(e) { warns <<- c(warns, paste("text node skipped:", conditionMessage(e))); FALSE })
    if (ok) {
      n_nodes <- n_nodes + 1L; n_marks <- n_marks + nrow(sel)
      counts <- counts + tabulate(sel$kw, nbins = nrow(kw))
    }
  }

  after <- .kwa_snapshot(doc)
  checks <- list(text_identical = identical(before$text, after$text),
                 elements_identical = identical(before$elems, after$elems),
                 attributes_identical = identical(before$attrs, after$attrs))
  if (!all(unlist(checks)))
    stop("Integrity check failed (", paste(names(checks)[!unlist(checks)], collapse = ", "),
         "); nothing was written.", call. = FALSE)

  written <- NULL
  if (!is.null(target)) written <- .kwa_write(doc, target, overwrite_ok = in_place || overwrite,
                                             backup = backup,
                                             expect_marks = length(xml2::xml_find_all(doc, "//*[@data-kwa-mark]")))
  if (!quiet) message(sprintf("%d annotation(s) in %d text node(s)", n_marks, n_nodes))

  invisible(structure(list(
    output = written,
    counts = tibble::tibble(keyword = kw$keyword, color = kw$color, occurrences = counts),
    n_text_nodes_modified = n_nodes, n_annotations = n_marks,
    warnings = warns, checks = checks, doc = if (is.null(written)) doc else NULL),
    class = "annotate_html_result"))
}
