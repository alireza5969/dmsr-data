# =============================================================================
# extract_article.R
# Robust extraction of structured data from PMC "jats-preview" HTML
# (HTML rendered from JATS XML, with jats-preview.css).
#
# Main entry points:
#   extract_article(html, base_url = NULL)   -> one-row tibble
#   extract_articles(paths, base_url = NULL) -> many rows (failure-safe)
#
# Packages: xml2, rvest (only for read_html re-export, optional), stringr,
#           stringi (installed with stringr), dplyr, tibble, purrr
#
# NOTE: written without the ability to execute R in the authoring environment.
# XPath selectors / regex logic were validated against the 12 sample files with
# an independent Python/lxml mirror (same libxml2 engine). Run
# tests_and_demo.R locally to confirm.
# =============================================================================

library(xml2)
library(stringr)
library(dplyr)
library(tibble)
library(purrr)

# ------------------------------------------------------------------ helpers --

.or <- function(x, y) {
  if (is.null(x) || length(x) == 0L || (length(x) == 1L && is.na(x))) y else x
}

# XPath 1.0 class-token test (exact token, not substring)
.cls <- function(token) {
  sprintf("contains(concat(' ', normalize-space(@class), ' '), ' %s ')", token)
}

.norm_ws <- function(x) {
  x <- stringr::str_replace_all(x, "[\\s\\u200b\\ufeff]+", " ")
  stringr::str_trim(x)
}

# Generated warnings such as "{ label needed for corresp[@id='...'] }"
.clean_text <- function(x) {
  x <- stringr::str_remove_all(x, "\\{ label needed for [^}]*\\}")
  .norm_ws(x)
}

.node_text <- function(node) {
  if (inherits(node, "xml_missing") || length(node) == 0L) return(NA_character_)
  .clean_text(xml2::xml_text(node))
}

# ---- diagnostics ------------------------------------------------------------
# status: "absent" (legitimately not in document), "uncertain", "error"
# (extraction code failed). Fields with no entry are "ok".

.dg <- function(dg, field, status, message = NA_character_) {
  dg$rows[[length(dg$rows) + 1L]] <- list(field = field, status = status,
                                          message = message)
  invisible(NULL)
}

.guard <- function(dg, field, default, expr) {
  tryCatch(expr, error = function(e) {
    .dg(dg, field, "error", conditionMessage(e))
    default
  })
}

.DG_FIELDS <- c("document", "metadata", "title", "received_date", "abstract",
                "references", "body", "supplementary")

.dg_table <- function(dg) {
  rows <- dg$rows
  seen <- if (length(rows)) vapply(rows, function(r) r$field, character(1)) else character()
  out <- tibble::tibble(
    field   = vapply(rows, function(r) r$field, character(1)),
    status  = vapply(rows, function(r) r$status, character(1)),
    message = vapply(rows, function(r) as.character(r$message), character(1))
  )
  ok <- setdiff(.DG_FIELDS, seen)
  dplyr::bind_rows(
    out,
    tibble::tibble(field = ok, status = rep("ok", length(ok)),
                   message = rep(NA_character_, length(ok)))
  )
}

# ---- typed empty templates (keep nested columns structurally identical) ------

.t_meta <- function() tibble::tibble(label = character(), label_l = character(),
                                     value = character())
.t_refs <- function() tibble::tibble(
  reference_index = integer(), reference_number = character(),
  reference_id = character(), reference_doi = character(),
  reference_doi_source = character(), reference_pmid = character(),
  reference_pmcid = character(), reference_other_ids = character(),
  reference_id_parse = character(), reference_text = character())
.t_abs_sections <- function() tibble::tibble(
  section_index = integer(), heading = character(), level = integer(),
  paragraph_index = integer(), text = character(), is_note = logical())
.t_abstracts <- function() tibble::tibble(
  abstract_index = integer(), label = character(), kind = character(),
  language = character(), language_confidence = character(),
  language_basis = character(), is_selected = logical(), text = character())
.t_sections <- function() tibble::tibble(
  section_index = integer(), section_title = character(),
  section_label = character(), section_level = integer(),
  parent_section_index = integer(), root_section_index = integer(),
  section_path = character(), section_type = character(),
  root_section_type = character(), section_type_source = character(),
  n_paragraphs = integer(), n_words = integer())
.t_body <- function() tibble::tibble(
  paragraph_id = integer(), section_index = integer(), section_title = character(),
  section_label = character(), section_level = integer(),
  parent_section_index = integer(), parent_section = character(),
  section_path = character(), root_section_index = integer(),
  root_section_title = character(), section_type = character(),
  root_section_type = character(), section_type_source = character(),
  paragraph_index = integer(), block_type = character(),
  paragraph_text = character(), n_words = integer(), in_word_count = logical())
.t_supp <- function() tibble::tibble(
  material_name = character(), material_url = character(),
  material_url_raw = character(), material_type = character(),
  link_text = character(), material_label = character(),
  material_description = character(), material_section = character(),
  source = character())
.t_alt_titles <- function() tibble::tibble(type = character(), title = character())

# ---- word counting -----------------------------------------------------------
# A "word" = whitespace-delimited token containing >= 1 letter or digit.
# Hyphenated words ("meta-analysis") = 1; "p<0.05" = 1; pure symbols ("=", "-")
# = 0; "and/or" = 1. Scripts written without spaces (Chinese, Japanese, Thai)
# are under-counted (one run of characters = one token).
.count_words <- function(x) {
  n <- stringr::str_count(as.character(x), "\\S*[\\p{L}\\p{N}]\\S*")
  as.integer(dplyr::coalesce(n, 0L))
}

# ---- dates -------------------------------------------------------------------

.MONTHS <- c(jan = 1L, feb = 2L, mar = 3L, apr = 4L, may = 5L, jun = 6L,
             jul = 7L, aug = 8L, sep = 9L, oct = 10L, nov = 11L, dec = 12L)
.MONTH_RE <- paste0("\\b(january|february|march|april|may|june|july|august|",
                    "september|october|november|december|jan|feb|mar|apr|jun|",
                    "jul|aug|sept|sep|oct|nov|dec)\\b")

# Returns days since epoch (numeric) or NA. Only accepts ISO dates or dates with
# an English month name AND a day AND a year. "April 2023" -> NA (no day).
.parse_one_date <- function(s) {
  if (is.na(s) || !nzchar(s)) return(NA_real_)
  s <- tolower(.norm_ws(s))
  y <- m <- d <- NA_integer_
  iso <- stringr::str_match(s, "^(\\d{4})[-/.](\\d{1,2})[-/.](\\d{1,2})$")
  if (!is.na(iso[1, 1])) {
    y <- as.integer(iso[1, 2]); m <- as.integer(iso[1, 3]); d <- as.integer(iso[1, 4])
  } else {
    mons <- unique(stringr::str_extract_all(s, .MONTH_RE)[[1]])
    yrs  <- unique(stringr::str_extract_all(s, "\\b(?:1[89]|20)\\d{2}\\b")[[1]])
    days <- unique(stringr::str_extract_all(s, "\\b\\d{1,2}(?=(?:st|nd|rd|th)?\\b)")[[1]])
    if (length(mons) != 1L || length(yrs) != 1L || length(days) != 1L) return(NA_real_)
    m <- unname(.MONTHS[stringr::str_sub(mons, 1L, 3L)])
    y <- as.integer(yrs); d <- as.integer(days)
  }
  if (is.na(y) || is.na(m) || is.na(d)) return(NA_real_)
  dt <- as.Date(sprintf("%04d-%02d-%02d", y, m, d), format = "%Y-%m-%d")
  if (is.na(dt)) NA_real_ else as.numeric(dt)
}

.parse_date_en <- function(x) {
  if (length(x) == 0L) return(as.Date(character()))
  as.Date(unname(vapply(x, .parse_one_date, numeric(1))), origin = "1970-01-01")
}

# ---- metadata entries ("Label: value" paragraphs in the front/footer) ---------

.meta_entries <- function(doc) {
  ps <- xml2::xml_find_all(doc, sprintf("//p[%s]", .cls("metadata-entry")))
  if (length(ps) == 0L) return(.t_meta())
  lab_nodes <- xml2::xml_find_first(ps, sprintf("./span[%s][1]", .cls("generated")))
  full <- .clean_text(xml2::xml_text(ps))
  lab  <- .clean_text(xml2::xml_text(lab_nodes))
  has_lab <- !is.na(lab) & nzchar(lab) & startsWith(full, dplyr::coalesce(lab, ""))
  value <- ifelse(has_lab, stringr::str_trim(stringr::str_sub(full, nchar(lab) + 1L)), full)
  label <- ifelse(has_lab, stringr::str_remove(lab, "\\s*:\\s*$"), NA_character_)
  tibble::tibble(label = label, label_l = tolower(label), value = value)
}

.meta_value <- function(meta, pattern) {
  hit <- meta$value[!is.na(meta$label_l) & stringr::str_detect(meta$label_l, pattern)]
  hit <- hit[!is.na(hit) & nzchar(hit)]
  if (length(hit)) hit[1] else NA_character_
}

# ---- title -------------------------------------------------------------------

.get_title <- function(doc, dg) {
  n <- xml2::xml_find_all(doc, sprintf("//h1[%s]", .cls("document-title")))
  src <- "document-title"
  if (length(n) == 0L) {
    n <- xml2::xml_find_all(doc, "//body//h1")
    src <- "first_h1"
  }
  if (length(n) > 0L) {
    tt <- .clean_text(xml2::xml_text(n))
    tt <- tt[!is.na(tt) & nzchar(tt)]
    if (length(tt)) {
      if (length(tt) > 1L) .dg(dg, "title", "uncertain",
                               sprintf("%d title elements; first used", length(tt)))
      return(list(title = tt[1], source = src))
    }
  }
  # last resort: <title> in <head> is "Authors: Title" -> strip up to first ": "
  ht <- .node_text(xml2::xml_find_first(doc, "//head/title"))
  if (!is.na(ht) && nzchar(ht)) {
    .dg(dg, "title", "uncertain", "title taken from <head><title>; author prefix stripped heuristically")
    return(list(title = stringr::str_remove(ht, "^[^:]{1,2000}?:\\s+"), source = "head_title_fallback"))
  }
  .dg(dg, "title", "absent", "no title element found")
  list(title = NA_character_, source = NA_character_)
}

# ---- received date -----------------------------------------------------------
# Original received date = label mentions received/submitted, but NOT revision /
# resubmission / accepted / published. If several candidates parse, the
# earliest is returned.

.get_received <- function(meta, dg) {
  if (nrow(meta) == 0L) {
    .dg(dg, "received_date", "absent", "no metadata entries"); return(list(date = as.Date(NA), raw = NA_character_))
  }
  l <- meta$label_l
  is_cand <- !is.na(l) &
    stringr::str_detect(l, "\\b(received|recd|submitted)\\b") &
    !stringr::str_detect(l, "\\brev|resubmi|accept|publi|online|epub")
  cand <- meta$value[is_cand]
  cand <- cand[!is.na(cand) & nzchar(cand)]
  if (length(cand) == 0L) {
    .dg(dg, "received_date", "absent", "no original 'received' entry"); return(list(date = as.Date(NA), raw = NA_character_))
  }
  dts <- .parse_date_en(cand)
  if (all(is.na(dts))) {
    .dg(dg, "received_date", "uncertain", "received value present but not parseable; raw text kept")
    return(list(date = as.Date(NA), raw = cand[1]))
  }
  i <- which.min(dts)
  list(date = dts[i], raw = cand[i])
}

# ---- identifier parsing for references ----------------------------------------
# The jats-preview rendering concatenates DOI, PMID, PMCID (and sometimes a
# Scopus EID) WITHOUT separators, e.g.
#   "10.1136/bmj.d592822008217PMC3196245"  = DOI 10.1136/bmj.d5928 + PMID 22008217 + PMC3196245
#   "3323935010.1136/bjsports-2020-102955" = PMID 33239350 + DOI
# Splitting is therefore heuristic. Strategy:
#   1. trailing PMC<digits>: if > 8 digits, last 8 are a PMID
#   2. trailing Scopus EID ("2-s2.0-<digits>")
#   3. publisher DOI grammars with fixed ends (the remainder is a PMID)
#   4. generic: a trailing run of >= 9 digits ends with an 8-digit PMID
# Anything that required (1)-(4) is flagged "heuristic_split".
# Known blind spot: 7-digit PMIDs (pre-~1998 papers) glued to a DOI.

.DOI_GRAMMARS <- c(
  "^(10\\.\\d{4,5}/j\\.[0-9a-z\\-]+\\.\\d{4}\\.[0-9a-z\\-]+\\.x)(.*)$",   # old Wiley ".x"
  "^(10\\.1097/[a-z]{2,8}\\.\\d{16})(.*)$",                              # Wolters Kluwer
  "^(10\\.1097/[a-z0-9\\-]+\\.0b013e[0-9a-f]{10})(.*)$",
  "^(10\\.1007/s\\d{12})(.*)$",                                          # old Springer
  "^(10\\.\\d{4,5}/s\\d{5}-\\d{3}-\\d{4,5}-[0-9xa-z])(.*)$",             # Springer/BMC s12345-020-01234-x
  "^(10\\.1371/journal\\.p[a-z]{3,4}\\.\\d{7})(.*)$",                    # PLOS
  "^(10\\.1016/s?\\d{4}-\\d{3}[\\dx]\\(\\d{2}\\)\\d{5}-[\\dx])(.*)$",    # Elsevier PII
  "^(10\\.7326/\\d{4}-\\d{4}-\\d+-\\d+-\\d{9}-\\d{5})(.*)$",             # Annals
  "^(10\\.1023/a:\\d{13})(.*)$",                                         # Kluwer
  "^(10\\.1017/s\\d{15}[\\dx])(.*)$",                                    # Cambridge
  "^(10\\.1079/[a-z]{2,4}\\d{8})(.*)$",                                  # CABI
  "^(10\\.1056/nejm\\d{15})(.*)$",                                       # NEJM (old)
  "^(10\\.1177/\\d{16})(.*)$",                                           # SAGE
  "^(10\\.1073/pnas\\.\\d{9,10})(.*)$"                                   # PNAS
)

.plausible_pmid <- function(s) {
  !is.na(s) && nchar(s) == 8L && !startsWith(s, "0") && as.numeric(s) <= 4.5e7
}

.strip_trailing_junk <- function(x) {
  repeat {
    y <- stringr::str_remove(x, "[.,;:'\"\\]>]+$")
    if (stringr::str_ends(y, "\\)") &&
        stringr::str_count(y, "\\(") < stringr::str_count(y, "\\)")) {
      y <- stringr::str_sub(y, 1L, -2L)
    }
    if (identical(y, x)) break
    x <- y
  }
  x
}

# strip trailing PMC<digits>[PMID digits]; returns list(rest, pmid, pmcid, heuristic)
.take_pmc_tail <- function(x) {
  out <- list(rest = x, pmid = NA_character_, pmcid = NA_character_, heuristic = FALSE)
  m <- stringr::str_match(x, "PMC(\\d+)$")
  if (is.na(m[1, 1])) return(out)
  d <- m[1, 2]
  out$rest <- stringr::str_sub(x, 1L, nchar(x) - nchar(m[1, 1]))
  if (nchar(d) <= 8L) {
    out$pmcid <- paste0("PMC", d)
  } else {
    out$pmid  <- stringr::str_sub(d, -8L, -1L)
    out$pmcid <- paste0("PMC", stringr::str_sub(d, 1L, nchar(d) - 8L))
    out$heuristic <- TRUE
  }
  out
}

.split_doi_token <- function(tok, known_pmid = FALSE) {
  out <- list(doi = NA_character_, pmid = NA_character_, pmcid = NA_character_,
              scopus = NA_character_, heuristic = FALSE)
  pm <- .take_pmc_tail(tok)
  tok <- pm$rest; out$pmid <- pm$pmid; out$pmcid <- pm$pmcid; out$heuristic <- pm$heuristic

  m <- stringr::str_match(tok, "2-s2\\.0-(\\d+)$")
  if (!is.na(m[1, 1])) {
    d <- m[1, 2]
    tok <- stringr::str_sub(tok, 1L, nchar(tok) - nchar(m[1, 1]))
    out$scopus <- paste0("2-s2.0-", d)
    if (nchar(d) >= 16L && is.na(out$pmid) && !known_pmid) {
      out$pmid <- stringr::str_sub(d, -8L, -1L)
      out$scopus <- paste0("2-s2.0-", stringr::str_sub(d, 1L, nchar(d) - 8L))
      out$heuristic <- TRUE
    }
  }

  tok <- .strip_trailing_junk(tok)

  for (g in .DOI_GRAMMARS) {
    mm <- stringr::str_match(tok, stringr::regex(g, ignore_case = TRUE))
    if (!is.na(mm[1, 1])) {
      core <- mm[1, 2]; rest <- mm[1, 3]
      if (!is.na(rest) && grepl("^[1-9][0-9]{6,7}$", rest) && is.na(out$pmid) && !known_pmid) {
        out$pmid <- rest; out$heuristic <- TRUE
      }
      out$doi <- tolower(core)
      return(out)
    }
  }

  mm <- stringr::str_match(tok, stringr::regex("^(10\\.3390/[a-z]+)(\\d+)$", ignore_case = TRUE))
  if (!is.na(mm[1, 1])) {                       # MDPI: 7-9 digits are part of the DOI
    if (nchar(mm[1, 3]) >= 15L && is.na(out$pmid) && !known_pmid) {
      out$pmid <- stringr::str_sub(mm[1, 3], -8L, -1L)
      tok <- paste0(mm[1, 2], stringr::str_sub(mm[1, 3], 1L, nchar(mm[1, 3]) - 8L))
      out$heuristic <- TRUE
    }
    out$doi <- tolower(tok)
    return(out)
  }

  r <- stringr::str_match(tok, "(\\d+)$")
  if (!is.na(r[1, 1]) && is.na(out$pmid) && !known_pmid) {
    run <- r[1, 2]; L <- nchar(run); cand <- NA_character_
    if (L >= 9L) {
      cand <- stringr::str_sub(run, -8L, -1L)
    } else if (L == 8L) {
      prev <- stringr::str_sub(tok, nchar(tok) - L, nchar(tok) - L)
      if (grepl("[A-Za-z]", prev)) cand <- run
    }
    if (!is.na(cand) && .plausible_pmid(cand)) {
      out$pmid <- cand
      tok <- stringr::str_sub(tok, 1L, nchar(tok) - 8L)
      out$heuristic <- TRUE
    }
  }
  out$doi <- tolower(tok)
  out
}

.parse_ref_ids <- function(text, hrefs) {
  res <- list(doi = NA_character_, doi_source = NA_character_, pmid = NA_character_,
              pmcid = NA_character_, other = character(), parse = NA_character_)
  hrefs <- hrefs[!is.na(hrefs)]
  heuristic <- FALSE

  # --- identifiers from hyperlinks (delimited, therefore reliable) ---
  href_dois <- character(); href_pmid <- NA_character_; href_pmc <- NA_character_
  for (h in hrefs) {
    h2 <- tryCatch(utils::URLdecode(h), error = function(e) h)
    m <- stringr::str_match(h2, "10\\.\\d{4,9}/[^\\s?#]+")
    if (!is.na(m[1, 1])) href_dois <- c(href_dois, tolower(.strip_trailing_junk(m[1, 1])))
    p <- stringr::str_match(h2, "(?i)(?:pubmed\\.ncbi\\.nlm\\.nih\\.gov|ncbi\\.nlm\\.nih\\.gov/pubmed)/(\\d{5,9})")
    if (!is.na(p[1, 2]) && is.na(href_pmid)) href_pmid <- p[1, 2]
    c2 <- stringr::str_match(h2, "/(PMC\\d+)")
    if (!is.na(c2[1, 2]) && is.na(href_pmc)) href_pmc <- c2[1, 2]
  }
  href_dois <- unique(href_dois)

  # --- identifiers from visible text (possibly glued) ---
  starts <- stringr::str_locate_all(text, "10\\.\\d{4,9}/")[[1]]
  text_dois <- character(); pmids <- character(); pmcs <- character(); other <- character()
  pre_pmid <- NA_character_
  if (nrow(starts) > 0L) {
    s <- starts[, 1]
    pre <- stringr::str_sub(text, 1L, s[1] - 1L)
    pm <- stringr::str_match(pre, "(?<!\\d)(\\d{8})$")[1, 2]
    if (.plausible_pmid(pm)) pre_pmid <- pm
    for (i in seq_along(s)) {
      e <- if (i < length(s)) s[i + 1] - 1L else nchar(text)
      tok <- stringr::str_extract(stringr::str_sub(text, s[i], e), "^\\S+")
      if (is.na(tok)) next
      tl <- tolower(tok)
      hit <- href_dois[startsWith(tl, href_dois)]
      if (length(hit)) {                        # token starts with a link-confirmed DOI
        d <- hit[1]
        tail <- .take_pmc_tail(stringr::str_sub(tok, nchar(d) + 1L))
        text_dois <- c(text_dois, d)
        if (!is.na(tail$pmcid)) pmcs <- c(pmcs, tail$pmcid)
        if (!is.na(tail$pmid))  { pmids <- c(pmids, tail$pmid); heuristic <- TRUE }
        rest <- .strip_trailing_junk(tail$rest)
        if (is.na(tail$pmid) && grepl("^[1-9][0-9]{6,7}$", rest)) { pmids <- c(pmids, rest); heuristic <- TRUE }
      } else {
        sp <- .split_doi_token(tok, known_pmid = !is.na(pre_pmid))
        if (!is.na(sp$doi)) text_dois <- c(text_dois, sp$doi)
        if (!is.na(sp$pmid))   pmids <- c(pmids, sp$pmid)
        if (!is.na(sp$pmcid))  pmcs  <- c(pmcs, sp$pmcid)
        if (!is.na(sp$scopus)) other <- c(other, paste0("scopus_eid:", sp$scopus))
        if (sp$heuristic) heuristic <- TRUE
      }
    }
    if (!is.na(pre_pmid)) { pmids <- c(pmids, pre_pmid); heuristic <- TRUE }
  } else {
    pm <- .take_pmc_tail(text)                  # reference without DOI
    if (!is.na(pm$pmcid)) { pmcs <- c(pmcs, pm$pmcid); if (pm$heuristic) heuristic <- TRUE }
    if (!is.na(pm$pmid))  pmids <- c(pmids, pm$pmid)
  }

  lab <- stringr::str_match(text, "(?i)\\b(?:PMID|PubMed(?: ID)?)\\s*[:#]?\\s*(\\d{6,9})")[1, 2]
  ax  <- stringr::str_match(text, "(?i)arXiv:\\s*(\\d{4}\\.\\d{4,5})")[1, 2]
  if (!is.na(ax)) other <- c(other, paste0("arxiv:", ax))

  # --- combine: link-derived values take precedence ---
  all_dois <- unique(c(href_dois, text_dois))
  if (length(all_dois)) {
    res$doi <- all_dois[1]
    res$doi_source <- if (length(href_dois)) "href" else "text"
    if (length(all_dois) > 1L) other <- c(other, paste0("doi_alt:", all_dois[-1]))
  }
  res$pmid  <- dplyr::coalesce(href_pmid, lab, if (length(pmids)) pmids[1] else NA_character_)
  res$pmcid <- dplyr::coalesce(href_pmc, if (length(pmcs)) pmcs[1] else NA_character_)
  res$other <- unique(other)
  if (all(is.na(c(res$doi, res$pmid, res$pmcid)))) {
    res$parse <- NA_character_
  } else if (heuristic && is.na(href_pmid) && is.na(lab)) {
    res$parse <- "heuristic_split"
  } else {
    res$parse <- "explicit"
  }
  res
}

# ---- references -------------------------------------------------------------

.get_references <- function(doc, dg) {
  rows <- xml2::xml_find_all(doc, sprintf("//*[%s]//div[%s]", .cls("ref-list"), .cls("row")))
  mode <- "table_rows"
  if (length(rows) == 0L) {
    rows <- xml2::xml_find_all(doc, sprintf("//*[%s]//li", .cls("ref-list")))
    mode <- "list_items"
  }
  if (length(rows) == 0L) {
    rows <- xml2::xml_find_all(doc, sprintf("//p[%s]", .cls("citation")))
    mode <- "citation_paragraphs"
  }
  if (length(rows) == 0L) {
    .dg(dg, "references", "absent", "no bibliography structure found")
    return(.t_refs())
  }
  if (mode != "table_rows") .dg(dg, "references", "uncertain", paste("fallback structure:", mode))

  recs <- purrr::imap(as.list(rows), function(r, i) {
    lab_n <- xml2::xml_find_first(r, sprintf(".//*[%s][1]", .cls("ref-label")))
    lab <- .node_text(lab_n)
    lab <- stringr::str_trim(stringr::str_remove_all(dplyr::coalesce(lab, ""), "^[\\[(]+|[\\])]+$|\\.$"))
    if (!nzchar(lab)) lab <- NA_character_
    aid <- xml2::xml_attr(xml2::xml_find_first(r, ".//a[@id][1]"), "id")
    cit <- xml2::xml_find_first(r, sprintf(".//p[%s][1]", .cls("citation")))
    if (inherits(cit, "xml_missing")) cit <- xml2::xml_find_first(r, sprintf(".//*[%s][1]", .cls("ref-content")))
    if (inherits(cit, "xml_missing")) cit <- r
    txt <- .clean_text(xml2::xml_text(cit))
    hrefs <- xml2::xml_attr(xml2::xml_find_all(cit, ".//a[@href]"), "href")
    ids <- tryCatch(.parse_ref_ids(txt, hrefs),
                    error = function(e) list(doi = NA_character_, doi_source = NA_character_,
                                             pmid = NA_character_, pmcid = NA_character_,
                                             other = character(), parse = NA_character_))
    list(index = i, number = dplyr::coalesce(lab, aid, as.character(i)), id = aid,
         doi = ids$doi, doi_source = ids$doi_source, pmid = ids$pmid, pmcid = ids$pmcid,
         other = if (length(ids$other)) paste(ids$other, collapse = "; ") else NA_character_,
         parse = ids$parse, text = txt)
  })
  g <- function(f, type = character(1)) vapply(recs, function(x) as.vector(x[[f]]), type)
  tibble::tibble(
    reference_index = as.integer(g("index", integer(1))),
    reference_number = g("number"), reference_id = g("id"),
    reference_doi = g("doi"), reference_doi_source = g("doi_source"),
    reference_pmid = g("pmid"), reference_pmcid = g("pmcid"),
    reference_other_ids = g("other"), reference_id_parse = g("parse"),
    reference_text = g("text")
  )
}

# ---- generic block walker (used for abstract AND body) -------------------------
# Walks the element tree in document order. A `div.section` opens a new
# (possibly nested) section; a `p` becomes a paragraph of the CURRENT section;
# figures, tables, captions, footnotes, panels (supplementary items), reference
# lists etc. are skipped entirely (so nothing is counted twice); other
# containers (lists, quotes, boxed text, wrappers) are descended into.

.EXCL_CLASSES <- c("fig", "table-wrap", "table-wrap-foot", "caption", "panel",
                   "footnote", "fn-group", "ref-list", "metadata", "abstract",
                   "def-list", "array")
.EXCL_PAT <- paste0(" ", .EXCL_CLASSES, " ")
.SKIP_TAGS <- c("table", "figure", "script", "style", "a", "span", "i", "b", "em",
                "strong", "sup", "sub", "br", "hr", "img", "h1", "h2", "h3", "h4",
                "h5", "h6", "head", "nav")
.HEAD_XP <- "./*[self::h1 or self::h2 or self::h3 or self::h4 or self::h5 or self::h6]"

.sec_heading <- function(k) {
  h <- xml2::xml_find_first(k, sprintf("%s[not(%s)][1]", .HEAD_XP, .cls("label")))
  l <- xml2::xml_find_first(k, sprintf("%s[%s][1]", .HEAD_XP, .cls("label")))
  t <- .node_text(h); lb <- .node_text(l)
  list(title = if (!is.na(t) && nzchar(t)) t else NA_character_,
       label = if (!is.na(lb) && nzchar(lb)) lb else NA_character_)
}

.walk_blocks <- function(roots) {
  st <- new.env(parent = emptyenv())
  st$sec <- list(); st$par <- list(); st$implicit <- NA_integer_

  new_section <- function(title, label, parent, level) {
    i <- length(st$sec) + 1L
    st$sec[[i]] <- list(index = i, title = title, label = label,
                        parent = parent, level = level)
    i
  }
  add_par <- function(sidx, text, type) {
    if (sidx == 0L) {                       # text outside any section
      if (is.na(st$implicit)) st$implicit <- new_section(NA_character_, NA_character_, NA_integer_, 1L)
      sidx <- st$implicit
    }
    st$par[[length(st$par) + 1L]] <- list(section = sidx, text = text, type = type)
  }
  visit <- function(node, sidx, in_list) {
    kids <- xml2::xml_children(node)
    if (length(kids) == 0L) return(invisible(NULL))
    nm <- xml2::xml_name(kids)
    cl <- xml2::xml_attr(kids, "class")
    cl <- paste0(" ", stringr::str_squish(dplyr::coalesce(cl, "")), " ")
    for (i in seq_along(kids)) {
      k <- kids[[i]]; n <- nm[i]; ci <- cl[i]
      if (n %in% .SKIP_TAGS) next
      if (any(vapply(.EXCL_PAT, function(p) grepl(p, ci, fixed = TRUE), logical(1)))) next
      if (grepl(" section ", ci, fixed = TRUE)) {
        h <- .sec_heading(k)
        lvl <- if (sidx > 0L) st$sec[[sidx]]$level + 1L else 1L
        new_idx <- new_section(h$title, h$label, if (sidx > 0L) sidx else NA_integer_, lvl)
        visit(k, new_idx, FALSE)
      } else if (n == "p") {
        s <- .clean_text(xml2::xml_text(k))
        if (nzchar(s)) add_par(sidx, s, if (in_list) "list_item" else "paragraph")
      } else if (n == "li") {
        if (length(xml2::xml_find_all(k, ".//p")) > 0L) {
          visit(k, sidx, TRUE)
        } else {
          s <- .clean_text(xml2::xml_text(k))
          if (nzchar(s)) add_par(sidx, s, "list_item")
        }
      } else {
        visit(k, sidx, in_list || n %in% c("ul", "ol") || grepl(" list ", ci, fixed = TRUE))
      }
    }
    invisible(NULL)
  }
  root_list <- if (inherits(roots, "xml_nodeset")) as.list(roots) else list(roots)
  for (r in root_list) visit(r, 0L, FALSE)

  ns <- length(st$sec); np <- length(st$par)
  sec <- if (ns == 0L) {
    tibble::tibble(index = integer(), title = character(), label = character(),
                   parent = integer(), level = integer())
  } else {
    tibble::tibble(
      index  = vapply(st$sec, function(s) as.integer(s$index), integer(1)),
      title  = vapply(st$sec, function(s) as.character(s$title), character(1)),
      label  = vapply(st$sec, function(s) as.character(s$label), character(1)),
      parent = vapply(st$sec, function(s) as.integer(s$parent), integer(1)),
      level  = vapply(st$sec, function(s) as.integer(s$level), integer(1)))
  }
  par <- if (np == 0L) {
    tibble::tibble(section = integer(), text = character(), type = character())
  } else {
    tibble::tibble(
      section = vapply(st$par, function(p) as.integer(p$section), integer(1)),
      text    = vapply(st$par, function(p) p$text, character(1)),
      type    = vapply(st$par, function(p) p$type, character(1)))
  }
  list(sections = sec, paras = par)
}

# ---- section classification -----------------------------------------------------

.norm_heading <- function(h) {          # vectorised
  h <- as.character(h); h[is.na(h)] <- ""
  h <- tolower(stringi::stri_trans_general(h, "Latin-ASCII"))
  h <- .norm_ws(h)
  h <- stringr::str_remove(h, "^(?:\\d+(?:\\.\\d+)*|[ivxlcdm]+|[a-z])[\\.\\):]\\s+")
  h <- stringr::str_remove(h, "^\\d+(?:\\.\\d+)*\\s+")
  h <- stringr::str_replace_all(h, "[^\\p{L}\\p{N}\\s'&/-]", " ")
  h <- .norm_ws(h)
  stringr::str_remove_all(h, "^[\\s.:;-]+|[\\s.:;-]+$")
}

# Order matters: first full match wins.
.TYPE_PATTERNS <- c(
  introduction = "(?:general )?introduction(?: and (?:background|aims?|objectives?|rationale))?|background(?: and (?:aims?|objectives?|rationale|significance|introduction))?|rationale|aims?(?: and objectives?)?|objectives?|purpose|context|overview",
  methods = "(?:materials? (?:and|&) )?methods?|methods and materials?|methodology|patients? and methods?|participants and methods?|subjects and methods?|study design(?: and methods?)?|research design(?: and methods?)?|design and methods?|data and methods?|methods and analysis|experimental(?: procedures| design| section)?|materials|study methods|review methods|search strategy(?: and selection criteria)?",
  results_discussion = "results? (?:and|&) discussion|results?, discussion|discussion (?:and|&) results?|findings and discussion",
  results = "results?|findings|results and analysis|analysis and results|outcomes?",
  discussion = "(?:general )?discussion|interpretation",
  conclusion = "conclusions?|concluding remarks|summary and conclusions?|conclusions? and (?:implications|recommendations|future (?:work|directions|research)|outlook)|discussion and conclusions?|final remarks|implications(?: for practice)?|future (?:work|directions|perspectives|research)",
  limitations = "(?:study |strengths and )?limitations?(?: and strengths)?|strengths and limitations|limitations of (?:the )?study|limitations and future (?:work|research|directions)",
  other_main = "the review|review|main text|main body|literature review|theoretical (?:background|framework)|conceptual framework|case (?:report|presentation|description)|the case|synthesis|analysis",
  acknowledgments = "acknowledge?ments?(?: and funding)?|thanks",
  funding = "fund(?:ing|s)(?: information| statement| sources?| support)?|financial support|sources? of funding|grants?|funding and acknowledg(?:e)?ments?",
  author_contributions = "(?:authors?'? )?contributions?|author contributions?|credit authorship contribution statement|contributorship",
  conflicts = "conflicts? of interests?|competing interests?|declarations? of (?:competing )?interests?|disclosures?|disclosure statement|conflict of interest statement|declaration of competing interest",
  data_availability = "data (?:availability|sharing)(?: statement)?|availability of data(?: and materials?)?|availability of supporting data|data and materials availability|code availability",
  ethics = "ethics?(?: approval| statement| declarations?| committee)?|ethical (?:approval|considerations?|statement|issues?)|ethics approval and consent(?: to participate)?|institutional review board statement|informed consent(?: statement)?|consent for publication|patient consent",
  declarations = "declarations?|statements? and declarations|additional information|notes|publisher'?s? note|article notes",
  supplementary = "supplement(?:ary|al)(?: (?:materials?|information|data|files?|tables?|figures?|content))?|supporting information|additional files?|electronic supplementary material|extra material|online resources?",
  abbreviations = "abbreviations?(?: and acronyms)?|list of abbreviations|glossary|acronyms|nomenclature",
  references = "references?|bibliography|literature cited|reference list|works cited",
  appendix = "appendix.*|appendices|annex.*"
)
# Only these may override the inherited type for NESTED sections (a nested
# "Quality appraisal" under Results stays "results"; a nested "Limitations"
# under Discussion becomes "limitations").
.NESTED_OVERRIDE <- c("limitations", "ethics", "funding", "conflicts", "data_availability",
                      "acknowledgments", "supplementary", "abbreviations",
                      "author_contributions", "declarations", "references", "appendix")
# Section types whose text is NOT main prose (excluded from body_word_count)
.NONPROSE <- c("acknowledgments", "funding", "author_contributions", "conflicts",
               "data_availability", "ethics", "declarations", "supplementary",
               "abbreviations", "references", "appendix")
.KEYWORD_PATTERNS <- c(discussion = "\\bdiscussion\\b", conclusion = "\\bconclusions?\\b",
                       results = "\\b(?:results?|findings)\\b",
                       methods = "\\b(?:methods?|methodology)\\b",
                       introduction = "\\b(?:introduction|background)\\b")

.classify_heading <- function(title, top_level) {
  n <- .norm_heading(title)
  if (!nzchar(n)) return(list(type = NA_character_, source = NA_character_))
  for (tp in names(.TYPE_PATTERNS)) {
    if (stringr::str_detect(n, paste0("^(?:", .TYPE_PATTERNS[[tp]], ")$"))) {
      if (top_level || tp %in% .NESTED_OVERRIDE) return(list(type = tp, source = "heading_exact"))
      return(list(type = NA_character_, source = NA_character_))
    }
  }
  if (top_level) {
    hits <- names(.KEYWORD_PATTERNS)[vapply(.KEYWORD_PATTERNS, function(p) stringr::str_detect(n, p), logical(1))]
    if (all(c("results", "discussion") %in% hits)) return(list(type = "results_discussion", source = "heading_keyword"))
    if (length(hits) == 1L) return(list(type = hits, source = "heading_keyword"))
    if (setequal(hits, c("discussion", "conclusion"))) return(list(type = "discussion", source = "heading_keyword"))
  }
  list(type = NA_character_, source = NA_character_)
}

.classify_sections <- function(sec) {
  n <- nrow(sec)
  type <- rep(NA_character_, n); src <- rep(NA_character_, n)
  root <- rep(NA_integer_, n); path <- rep(NA_character_, n)
  for (i in seq_len(n)) {
    p <- sec$parent[i]; top <- is.na(p)
    cl <- .classify_heading(sec$title[i], top)
    if (is.na(cl$type)) {
      if (!top) { type[i] <- type[p]; src[i] <- "inherited" }
      else      { type[i] <- "unclassified"; src[i] <- "none" }
    } else { type[i] <- cl$type; src[i] <- cl$source }
    root[i] <- if (top) sec$index[i] else root[p]
    tt <- ifelse(is.na(sec$title[i]), "(untitled)", sec$title[i])
    path[i] <- if (top) tt else paste(path[p], tt, sep = " > ")
  }
  # weak positional fallback: an unclassified FIRST top-level section that is
  # followed by recognisable Methods/Results/Discussion/Conclusion is treated
  # as Introduction, and says so in section_type_source = "position".
  tops <- which(is.na(sec$parent))
  if (length(tops) > 1L && type[tops[1]] == "unclassified" &&
      any(type[tops[-1]] %in% c("methods", "results", "discussion", "conclusion"))) {
    r <- sec$index[tops[1]]
    inh <- root == r & src == "inherited"
    type[tops[1]] <- "introduction"; src[tops[1]] <- "position"
    type[inh] <- "introduction"
  }
  sec$root <- root; sec$type <- type; sec$src <- src; sec$path <- path
  sec
}

.build_body <- function(w) {
  sec <- w$sections; pr <- w$paras
  if (nrow(pr) == 0L && nrow(sec) == 0L) return(list(body = .t_body(), sections = .t_sections()))
  sec <- .classify_sections(sec)
  idx <- pr$section
  nw <- .count_words(pr$text)
  root_type <- sec$type[sec$root[idx]]
  body <- if (nrow(pr) == 0L) .t_body() else tibble::tibble(
    paragraph_id = seq_len(nrow(pr)),
    section_index = idx,
    section_title = sec$title[idx],
    section_label = sec$label[idx],
    section_level = sec$level[idx],
    parent_section_index = sec$parent[idx],
    parent_section = sec$title[sec$parent[idx]],
    section_path = sec$path[idx],
    root_section_index = sec$root[idx],
    root_section_title = sec$title[sec$root[idx]],
    section_type = sec$type[idx],
    root_section_type = root_type,
    section_type_source = sec$src[idx],
    paragraph_index = as.integer(stats::ave(idx, idx, FUN = seq_along)),
    block_type = pr$type,
    paragraph_text = pr$text,
    n_words = nw,
    in_word_count = !(root_type %in% .NONPROSE)
  )
  npar <- tabulate(idx, nbins = nrow(sec))
  nwd  <- vapply(seq_len(nrow(sec)), function(i) sum(nw[idx == i]), numeric(1))
  sections <- tibble::tibble(
    section_index = sec$index, section_title = sec$title, section_label = sec$label,
    section_level = sec$level, parent_section_index = sec$parent,
    root_section_index = sec$root, section_path = sec$path,
    section_type = sec$type, root_section_type = sec$type[sec$root],
    section_type_source = sec$src, n_paragraphs = as.integer(npar),
    n_words = as.integer(nwd))
  list(body = body, sections = sections)
}

.get_body <- function(doc, dg) {
  roots <- xml2::xml_find_all(doc, sprintf("//body/div[%s]", .cls("body")))
  how <- "class"
  if (length(roots) == 0L) {
    roots <- xml2::xml_find_all(doc, sprintf("//div[%s][not(ancestor::div[%s])]", .cls("body"), .cls("body")))
  }
  if (length(roots) == 0L) {
    roots <- xml2::xml_find_all(doc, "//body/div[substring(@id, string-length(@id) - 3) = 'body']")
    how <- "id_suffix"
  }
  if (length(roots) == 0L) {
    .dg(dg, "body", "absent", "no body container (div.body / id ending 'body') found")
    return(NULL)
  }
  if (how != "class") .dg(dg, "body", "uncertain", paste("body container located via", how))
  w <- .walk_blocks(roots)
  if (nrow(w$paras) == 0L) .dg(dg, "body", "absent", "body container found but contains no paragraphs")
  .build_body(w)
}

# ---- abstract ---------------------------------------------------------------------

.EN_STOP <- c("the","of","and","in","to","a","was","were","with","for","is","that","on","by","as","are","this","from","at","be","or","an","which","not","had","has","been","we","these","than","also","between","among","all","can","may")
.OTHER_STOP <- list(
  es = c("el","la","los","las","del","y","que","con","para","por","una","se","su","como","m\u00e1s","fue","entre","sobre","sin","ser","han","este","esta","estos"),
  fr = c("le","les","des","du","et","est","une","dans","pour","par","sur","avec","qui","que","au","aux","ont","\u00e9t\u00e9","cette","ces","sont","nous","plus","pas"),
  de = c("der","die","das","und","ist","nicht","mit","von","den","dem","ein","eine","auf","f\u00fcr","bei","auch","wurden","wurde","sich","zu","im","als","werden","oder"),
  pt = c("o","os","do","da","dos","das","em","um","uma","para","com","por","que","foi","n\u00e3o","s\u00e3o","mais","entre","ao","como"),
  it = c("il","lo","gli","della","delle","dei","di","che","per","con","una","sono","nel","nella","non","pi\u00f9","\u00e8","tra","alla")
)
.ABS_LABEL_EN <- "^(?:abstract|summary|structured abstract|synopsis|abstract and keywords?)$"
.ABS_LABEL_OTHER <- "^(?:resumen|r\u00e9sum\u00e9|resume|resumo|zusammenfassung|riassunto|sommario|samenvatting|sammendrag|\u0430\u043d\u043d\u043e\u0442\u0430\u0446\u0438\u044f|\u0440\u0435\u0437\u044e\u043c\u0435|\u0440\u0435\u0444\u0435\u0440\u0430\u0442|\u6458\u8981|\u8981\u65e8|\u6982\u8981|\u8981\u7d04|\ucd08\ub85d|\uc694\uc57d|\u00f6zet|streszczenie|abstrakt|abstrakti)$"
.ABS_LABEL_SIDE <- "plain|lay |layman|author summary|key ?points?|key messages?|highlights?|graphical|teaser|impact|significance|already known|research in context|in brief|at a glance|clinical relevance|practice points|what is"

.guess_language <- function(text) {
  tk <- stringr::str_extract_all(tolower(text), "[\\p{L}]+")[[1]]
  nonlat <- stringr::str_count(text, "[\\p{Han}\\p{Hiragana}\\p{Katakana}\\p{Hangul}\\p{Cyrillic}\\p{Arabic}\\p{Hebrew}\\p{Thai}\\p{Greek}]")
  if (nchar(text) > 0L && nonlat / nchar(text) > 0.3) return(list(lang = "non-en", conf = "high", basis = "script"))
  if (length(tk) < 20L) return(list(lang = NA_character_, conf = "low", basis = "too_short"))
  en <- mean(tk %in% .EN_STOP)
  ot <- max(vapply(.OTHER_STOP, function(s) mean(tk %in% s), numeric(1)))
  if (en >= 0.20 && en >= 2 * ot) return(list(lang = "en", conf = "high", basis = "text"))
  if (ot >= 0.15 && ot >= 2 * en) return(list(lang = "non-en", conf = "high", basis = "text"))
  list(lang = NA_character_, conf = "low", basis = "text_ambiguous")
}

.format_abstract <- function(par) {
  par <- par[!par$is_note, , drop = FALSE]
  if (nrow(par) == 0L) return(NA_character_)
  blocks <- vapply(unique(par$section_index), function(si) {
    p <- par[par$section_index == si, ]
    h <- p$heading[1]
    body <- paste(p$text, collapse = " ")
    if (!is.na(h)) paste0(stringr::str_remove(h, "\\s*:\\s*$"), ": ", body) else body
  }, character(1))
  paste(blocks, collapse = "\n\n")
}

.get_abstracts <- function(doc, dg) {
  blocks <- xml2::xml_find_all(doc, sprintf(
    "//div[%s][.//h4[%s]][not(ancestor::div[%s or %s])]",
    .cls("metadata"), .cls("callout-title"), .cls("body"), .cls("back")))
  if (length(blocks) == 0L) {
    blocks <- xml2::xml_find_all(doc, sprintf("//div[%s][not(ancestor::div[%s])]", .cls("abstract"), .cls("abstract")))
  }
  if (length(blocks) == 0L) {
    .dg(dg, "abstract", "absent", "no abstract block found")
    return(list(abstracts = .t_abstracts(), sections = .t_abs_sections(), selected = NA_integer_))
  }
  all_secs <- list(); recs <- list()
  for (i in seq_along(blocks)) {
    b <- blocks[[i]]
    label <- .node_text(xml2::xml_find_first(b, sprintf(".//h4[%s][1]", .cls("callout-title"))))
    cells <- xml2::xml_find_all(b, sprintf(".//div[%s][not(.//h4[%s])]", .cls("cell"), .cls("callout-title")))
    w <- if (length(cells)) .walk_blocks(cells) else .walk_blocks(b)
    if (nrow(w$paras) == 0L) {                       # unstructured fallback
      fallback <- if (length(cells)) .clean_text(paste(xml2::xml_text(cells), collapse = " ")) else NA_character_
      if (!is.na(fallback) && nzchar(fallback)) {
        w$sections <- tibble::tibble(index = 1L, title = NA_character_, label = NA_character_, parent = NA_integer_, level = 1L)
        w$paras <- tibble::tibble(section = 1L, text = fallback, type = "paragraph")
        .dg(dg, "abstract", "uncertain", "abstract text taken as one unstructured block")
      }
    }
    sec <- w$sections; pr <- w$paras
    note_re <- "^(?:supplementary|supporting)"
    par <- if (nrow(pr) == 0L) .t_abs_sections() else tibble::tibble(
      section_index = pr$section,
      heading = sec$title[pr$section],
      level = sec$level[pr$section],
      paragraph_index = as.integer(stats::ave(pr$section, pr$section, FUN = seq_along)),
      text = pr$text,
      is_note = stringr::str_detect(.norm_heading(sec$title[pr$section]), note_re))
    text <- .format_abstract(par)
    lab_n <- .norm_heading(label)
    kind <- if (stringr::str_detect(lab_n, .ABS_LABEL_EN) || stringr::str_detect(lab_n, .ABS_LABEL_OTHER)) "abstract"
            else if (stringr::str_detect(lab_n, .ABS_LABEL_SIDE)) "other_summary" else "other"
    lab_lang <- if (stringr::str_detect(lab_n, .ABS_LABEL_EN)) "en" else if (kind == "abstract") "non-en" else NA_character_
    gl <- if (!is.na(text)) .guess_language(text) else list(lang = NA_character_, conf = "low", basis = "no_text")
    lang <- gl$lang; conf <- gl$conf; basis <- gl$basis
    if (is.na(lang) && !is.na(lab_lang)) { lang <- lab_lang; conf <- "low"; basis <- "label_only" }
    if (is.na(lang)) lang <- "unknown"
    recs[[i]] <- list(label = label, kind = kind, language = lang, conf = conf, basis = basis, text = text)
    all_secs[[i]] <- par
  }
  gv <- function(f) vapply(recs, function(r) as.character(r[[f]]), character(1))
  tab <- tibble::tibble(
    abstract_index = seq_along(recs), label = gv("label"), kind = gv("kind"),
    language = gv("language"), language_confidence = gv("conf"),
    language_basis = gv("basis"), is_selected = FALSE, text = gv("text"))
  has_text <- !is.na(tab$text) & nzchar(tab$text)
  rank_lang <- ifelse(tab$language == "en" & tab$language_confidence == "high", 1L,
               ifelse(tab$language == "en", 2L, ifelse(tab$language == "unknown", 3L, 4L)))
  pool <- which(tab$kind == "abstract" & has_text)
  if (length(pool) == 0L) {
    pool <- which(tab$kind == "other" & has_text)
    if (length(pool)) .dg(dg, "abstract", "uncertain", "no block labelled Abstract/Summary; unlabelled block used")
  }
  sel <- NA_integer_
  if (length(pool)) {
    sel <- pool[order(rank_lang[pool], pool)][1]
    tab$is_selected[sel] <- TRUE
    if (tab$language[sel] != "en" || tab$language_confidence[sel] != "high")
      .dg(dg, "abstract", "uncertain",
          sprintf("selected abstract language = %s (%s confidence, basis: %s)",
                  tab$language[sel], tab$language_confidence[sel], tab$language_basis[sel]))
    if (sum(tab$kind == "abstract" & has_text) > 1L)
      .dg(dg, "abstract", "uncertain", "multiple abstracts present; preferred English original selected")
  } else {
    .dg(dg, "abstract", "absent", "abstract block(s) present but empty / only side summaries")
  }
  list(abstracts = tab, sections = if (!is.na(sel)) all_secs[[sel]] else .t_abs_sections(), selected = sel)
}

# ---- supplementary materials -------------------------------------------------------

.resolve_url <- function(url, base) {
  if (is.na(url) || !nzchar(url)) return(NA_character_)
  if (is.null(base) || is.na(base) || !nzchar(base)) return(url)
  if (grepl("^(?:[a-z][a-z0-9+.-]*:|#|//)", url, ignore.case = TRUE, perl = TRUE)) return(url)
  tryCatch(xml2::url_absolute(url, base), error = function(e) url)
}

.url_ext <- function(url) {
  if (is.na(url)) return(NA_character_)
  e <- tolower(tools::file_ext(sub("[?#].*$", "", url)))
  if (nzchar(e) && nchar(e) <= 5L) e else NA_character_
}

.SUPP_HEAD_RE <- "^(?:electronic )?(?:supplement(?:ary|al)|supporting|additional (?:file|data|information)|extended data|online (?:resource|material))"
.SUPP_ID_RE   <- "(?i)(moesm|supp|supitem|esm|media|additional|mmc|suppl)"
.SUPP_HREF_RE <- "(?i)(moesm|supplement|suppl|[/_.-]supp[-_]?[0-9]|mmc[0-9]|media-[0-9]|[-_]esm\\b)"
.SUPP_TEXT_RE <- "(?i)^(?:supplement|supporting information|additional file|online resource|electronic supplementary)"
.BOILER_RE    <- "(?i)^click here (?:for|to) .*(?:file|data|download)\\.?$"

.nearest_section_title <- function(nodes) {
  h <- xml2::xml_find_first(nodes, sprintf(
    "ancestor::div[%s][1]/*[self::h1 or self::h2 or self::h3 or self::h4 or self::h5 or self::h6][not(%s)][1]",
    .cls("section"), .cls("label")))
  .clean_text(xml2::xml_text(h))
}

.get_supplements <- function(doc, meta, base_url, dg) {
  base <- base_url
  if (is.null(base) || is.na(base) || !nzchar(base)) {
    b <- xml2::xml_attr(xml2::xml_find_first(doc, "//head/base"), "href")
    base <- if (is.na(b)) NULL else b
  }
  self_uri <- xml2::xml_attr(xml2::xml_find_all(doc, sprintf(
    "//p[%s][starts-with(normalize-space(.), 'Self URI')]//a", .cls("metadata-entry"))), "href")
  self_uri <- sub("^file:", "", self_uri)
  rows <- list()

  # (1) <supplementary-material> -> div.panel (not fig / table-wrap)
  panels <- xml2::xml_find_all(doc, sprintf(
    "//div[%s][not(%s)][not(%s)][not(ancestor::div[%s])]",
    .cls("panel"), .cls("fig"), .cls("table-wrap"), .cls("panel")))
  for (p in as.list(panels)) {
    link <- xml2::xml_find_first(p, ".//a[@href][not(starts-with(@href, '#'))][1]")
    href <- xml2::xml_attr(link, "href")
    aid  <- paste(xml2::xml_attr(xml2::xml_find_all(p, "./a[@id]"), "id"), collapse = " ")
    label <- .node_text(xml2::xml_find_first(p, sprintf("./*[%s][1]", .cls("label"))))
    # captions may sit directly in the panel OR inside the <a> link element
    cap_p <- xml2::xml_find_all(p, sprintf(".//div[%s]//p", .cls("caption")))
    caps <- if (length(cap_p)) .clean_text(xml2::xml_text(cap_p))
            else .clean_text(xml2::xml_text(xml2::xml_find_all(p, sprintf(".//div[%s]", .cls("caption")))))
    caps <- caps[nzchar(caps) & !stringr::str_detect(caps, .BOILER_RE)]
    sec_t <- .nearest_section_title(p)
    sig <- stringr::str_detect(aid, .SUPP_ID_RE) ||
      (!is.na(sec_t) && stringr::str_detect(.norm_heading(sec_t), .SUPP_HEAD_RE)) ||
      stringr::str_detect(paste(dplyr::coalesce(label, ""), paste(caps, collapse = " ")),
                          "(?i)supplement|supporting information|additional (?:file|data)|online resource|appendix s|\\b(?:table|figure|fig\\.?) s\\d") ||
      (!is.na(href) && !is.na(.url_ext(href)))
    if (!sig) next
    nm <- if (length(caps)) caps[1] else if (!is.na(label)) label else .node_text(link)
    rows[[length(rows) + 1L]] <- list(
      name = nm, url = .resolve_url(href, base), url_raw = href, type = .url_ext(href),
      link_text = .node_text(link), label = label,
      desc = if (length(caps) > 1L) paste(caps[-1], collapse = " | ") else NA_character_,
      section = sec_t, source = "panel")
  }

  # (2) ordinary links outside panels / bibliography / front matter that either
  #     look like supplementary files, or sit in a "Supplementary ..." section
  links <- xml2::xml_find_all(doc, sprintf(
    "//div[%s or %s]//a[@href][not(starts-with(@href, '#'))][not(ancestor::div[%s])][not(ancestor::div[%s])]",
    .cls("body"), .cls("back"), .cls("panel"), .cls("ref-list")))
  if (length(links) > 0L) {
    href <- xml2::xml_attr(links, "href")
    ltxt <- .clean_text(xml2::xml_text(links))
    sect <- .nearest_section_title(links)
    in_supp_sec <- !is.na(sect) & stringr::str_detect(.norm_heading(sect), .SUPP_HEAD_RE)
    looks <- stringr::str_detect(href, .SUPP_HREF_RE) | stringr::str_detect(ltxt, .SUPP_TEXT_RE)
    bad <- stringr::str_detect(href, "(?i)doi\\.org|pubmed|ncbi\\.nlm\\.nih\\.gov") | sub("^file:", "", href) %in% self_uri
    keep <- which((looks | in_supp_sec) & !bad)
    for (i in keep) {
      ctx <- .node_text(xml2::xml_find_first(links[[i]], "ancestor::p[1]"))
      rows[[length(rows) + 1L]] <- list(
        name = ltxt[i], url = .resolve_url(href[i], base), url_raw = href[i],
        type = .url_ext(href[i]), link_text = ltxt[i], label = NA_character_,
        desc = ctx, section = sect[i], source = "link")
    }
  }

  if (length(rows) == 0L) {
    .dg(dg, "supplementary", "absent", "no supplementary material found")
    return(.t_supp())
  }
  g <- function(f) vapply(rows, function(r) as.character(.or(r[[f]], NA_character_)), character(1))
  out <- tibble::tibble(
    material_name = g("name"), material_url = g("url"), material_url_raw = g("url_raw"),
    material_type = g("type"), link_text = g("link_text"), material_label = g("label"),
    material_description = g("desc"), material_section = g("section"), source = g("source"))
  dplyr::distinct(out, material_url_raw, material_name, .keep_all = TRUE)
}

# ---- main --------------------------------------------------------------------------

.as_doc <- function(html, dg) {
  if (inherits(html, "xml_node")) return(html)
  if (is.character(html) && length(html) == 1L) {
    return(tryCatch(
      xml2::read_html(html, options = c("RECOVER", "NOERROR", "NOWARNING", "HUGE")),
      error = function(e) { .dg(dg, "document", "error", paste("cannot parse:", conditionMessage(e))); NULL }))
  }
  .dg(dg, "document", "error", "`html` must be an xml_document or a single file path / HTML string")
  NULL
}

#' Extract structured data from one PMC jats-preview HTML article
#'
#' @param html  xml2/rvest document from read_html(), or a local path.
#' @param base_url optional base URL used ONLY to resolve relative supplementary
#'   links (never fetched). If NULL, <base href> is used when present; otherwise
#'   relative links are returned unchanged.
#' @return one-row tibble. See column documentation in the accompanying README.
extract_article <- function(html, base_url = NULL) {
  dg <- new.env(parent = emptyenv()); dg$rows <- list()
  doc <- .as_doc(html, dg)

  meta <- .t_meta(); ttl <- list(title = NA_character_, source = NA_character_)
  alt <- .t_alt_titles(); rec <- list(date = as.Date(NA), raw = NA_character_)
  abs <- list(abstracts = .t_abstracts(), sections = .t_abs_sections(), selected = NA_integer_)
  refs <- .t_refs(); bd <- NULL; supp <- .t_supp()

  if (!is.null(doc)) {
    meta <- .guard(dg, "metadata", .t_meta(), .meta_entries(doc))
    ttl  <- .guard(dg, "title", ttl, .get_title(doc, dg))
    alt  <- .guard(dg, "title", .t_alt_titles(), {
      a <- meta[!is.na(meta$label_l) & stringr::str_starts(meta$label_l, "alternative title"), ]
      if (nrow(a)) tibble::tibble(type = stringr::str_match(a$label, "\\(([^)]*)\\)")[, 2], title = a$value) else .t_alt_titles()
    })
    rec  <- .guard(dg, "received_date", rec, .get_received(meta, dg))
    abs  <- .guard(dg, "abstract", abs, .get_abstracts(doc, dg))
    refs <- .guard(dg, "references", .t_refs(), .get_references(doc, dg))
    bd   <- .guard(dg, "body", NULL, .get_body(doc, dg))
    supp <- .guard(dg, "supplementary", .t_supp(), .get_supplements(doc, meta, base_url, dg))
  }

  sel <- abs$selected
  a_sel <- if (!is.na(sel)) abs$abstracts[sel, ] else NULL
  body <- if (is.null(bd)) .t_body() else bd$body
  secs <- if (is.null(bd)) .t_sections() else bd$sections
  dgt <- .dg_table(dg)

  tibble::tibble(
    pmcid       = .meta_value(meta, "^article id \\(pmcid\\)$"),
    pmid        = .meta_value(meta, "^pubmed id$"),
    article_doi = tolower(.meta_value(meta, "^doi$")),
    title = ttl$title, title_source = ttl$source,
    alternative_titles = list(alt),
    received_date = rec$date, received_date_raw = rec$raw,
    abstract_text = if (is.null(a_sel)) NA_character_ else a_sel$text,
    abstract_label = if (is.null(a_sel)) NA_character_ else a_sel$label,
    abstract_language = if (is.null(a_sel)) NA_character_ else a_sel$language,
    abstract_language_confidence = if (is.null(a_sel)) NA_character_ else a_sel$language_confidence,
    abstract_sections = list(abs$sections),
    abstracts = list(abs$abstracts),
    n_references = nrow(refs),
    references = list(refs),
    body = list(body),
    sections = list(secs),
    n_body_paragraphs = if (is.null(bd)) NA_integer_ else nrow(body),
    body_word_count = if (is.null(bd)) NA_integer_ else as.integer(sum(body$n_words[body$in_word_count])),
    body_word_count_all = if (is.null(bd)) NA_integer_ else as.integer(sum(body$n_words)),
    supplementary = list(supp),
    extraction_status = if (any(dgt$status == "error")) "partial_error" else if (any(dgt$status == "uncertain")) "ok_with_notes" else "ok",
    diagnostics = list(dgt)
  )
}

#' Apply extract_article() to many files; never aborts the whole run.
#' Returns one row per path (a failed file gets a row with extraction_status
#' "failed" and the error message in diagnostics).
extract_articles <- function(paths,
                             base_url = NULL,
                             workers = 1L,
                             chunk_size = 500L,
                             checkpoint_dir = NULL,   # e.g. "temp/extraction"
                             progress = TRUE) {
  
  paths <- unname(as.character(paths))   # drop fs_path class and names
  
  one <- function(p) {
    res <- tryCatch(
      extract_article(
        xml2::read_html(p, options = c("RECOVER", "NOERROR", "NOWARNING", "HUGE")),
        base_url = base_url
      ),
      error = function(e) {
        r <- extract_article(NULL)
        r$extraction_status <- "failed"
        r$diagnostics <- list(tibble::tibble(
          field = "document", status = "error", message = conditionMessage(e)))
        r
      })
    dplyr::bind_cols(tibble::tibble(path = p), res)
  }
  
  # restore the user's plan on exit; use multisession on Windows / in RStudio
  old_plan <- future::plan(future::sequential, workers = workers)
  on.exit(future::plan(old_plan), add = TRUE)
  
  if (!is.null(checkpoint_dir)) fs::dir_create(checkpoint_dir)
  chunks <- split(paths, ceiling(seq_along(paths) / chunk_size))
  out <- vector("list", length(chunks))
  
  for (i in seq_along(chunks)) {
    f <- if (!is.null(checkpoint_dir))
      file.path(checkpoint_dir, sprintf("extraction_chunk_%05d.rds", i))
    
    if (!is.null(f) && file.exists(f)) {          # resume: reuse finished chunk
      out[[i]] <- readr::read_rds(f)
    } else {
      out[[i]] <- dplyr::bind_rows(purrr::map(chunks[[i]], one))
      if (!is.null(f)) readr::write_rds(out[[i]], f, compress = "gz")
    }
    if (progress) message(sprintf("chunk %d/%d done (%d files)", i, length(chunks),
                                  min(i * chunk_size, length(paths))))
  }
  dplyr::bind_rows(out)
}


