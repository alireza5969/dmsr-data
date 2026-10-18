library(fs)
library(glue)
library(htmltools)
library(purrr)
library(readr)
library(stringr)

#' Add text tabs to an HTML file, in place
#'
#' Rewrites `html_path` so that its existing `<body>` content becomes the first
#' tab, and each text in `texts` becomes an additional tab. Everything the tool
#' adds is wrapped in marker comments, so:
#'   * calling the function again replaces the previous tabs (no stacking);
#'   * `remove_text_tabs()` restores the file to its original content exactly.
#' All added CSS classes and ids use the `tt-` prefix to avoid clashing with
#' the page's own styles (e.g. `jats-preview.css`).
#'
#' @param html_path Path to the HTML file. It is modified in place.
#' @param texts Character vector of plain texts, one per extra tab. Names are
#'   used as tab labels: `c(Abstract = "...", Notes = "...")`. Unnamed texts are
#'   labelled "Text 1", "Text 2", ... (or "Text" when there is only one).
#' @param article_label Label of the first tab (the original content).
#' @param colors Optional tab colors for the text tabs, any R color name or hex
#'   code ("red", "#c0392b"). Either
#'   * named by tab label, to colour only some tabs: `c(Notes = "red")`, or
#'   * unnamed, one per text, with `NA` for "keep the default":
#'     `c(NA, "red", "steelblue")`.
#'   Tabs without a color keep the default look. Text color (dark/light) is
#'   chosen automatically for readability.
#' @param article_color Optional color for the first (article) tab.
#' @param na_text What to show in a tab whose text is `NA`
#'   (default "(No text available)"). The tab is still created.
#' @param backup If `TRUE`, the untouched original is saved once as
#'   `<html_path>.bak` (never overwritten on later runs).
#'
#' @return `html_path`, invisibly.
add_text_tabs <- function(html_path,
                          texts,
                          article_label = "Article",
                          colors = NULL,
                          article_color = NULL,
                          na_text = "(No text available)",
                          backup = TRUE) {
  # `c(A = NA, B = NA)` is logical; treat an all-NA vector as character NAs
  if (is.logical(colors) && all(is.na(colors))) storage.mode(colors) <- "character"
  if (is.logical(texts)  && all(is.na(texts)))  storage.mode(texts)  <- "character"
  
  stopifnot(
    is.character(texts), length(texts) >= 1,
    is.character(na_text), length(na_text) == 1, !is.na(na_text),
    is.character(article_label), length(article_label) == 1,
    is.null(colors) || is.character(colors),
    is.null(article_color) || (is.character(article_color) && length(article_color) == 1)
  )
  if (!file_exists(html_path)) {
    stop("HTML file not found: ", html_path, call. = FALSE)
  }
  
  texts[is.na(texts)] <- na_text   # keeps names; NA texts get a placeholder
  
  # build first, so invalid colors fail before anything is written
  blocks <- build_blocks(article_label, texts, colors, article_color)
  
  # start from the pristine original, even if tabs were added before
  html <- read_file(html_path) |> strip_text_tabs()
  
  if (backup) {
    backup_path <- str_c(html_path, ".bak")
    if (!file_exists(backup_path)) write_file(html, backup_path)
  }
  
  parts <- split_html(html)
  
  page <- str_c(
    parts$before_head_end, blocks$head,
    parts$head_end_to_body_open,
    blocks$body_before, parts$body, blocks$body_after,
    parts$body_close_to_end
  )
  
  write_file(page, html_path)
  invisible(html_path)
}

#' Restore the HTML file to its original content (removes all added tabs)
remove_text_tabs <- function(html_path) {
  if (!file_exists(html_path)) {
    stop("HTML file not found: ", html_path, call. = FALSE)
  }
  read_file(html_path) |>
    strip_text_tabs() |>
    write_file(html_path)
  invisible(html_path)
}

# helpers ---------------------------------------------------------------

`%||%` <- function(x, y) if (is.null(x)) y else x

mark <- list(
  style_begin   = "<!-- text-tabs:style-begin -->",
  style_end     = "<!-- text-tabs:style-end -->",
  begin         = "<!-- text-tabs:begin -->",
  article_begin = "<!-- text-tabs:article-begin -->",
  article_end   = "<!-- text-tabs:article-end -->",
  end           = "<!-- text-tabs:end -->"
)

# Normalise newlines and escape &, <, > so the text is shown literally
text_to_html <- function(text) {
  text |>
    str_replace_all("\r\n?", "\n") |>
    htmlEscape()
}

# Tab labels: use names when given, otherwise "Text", "Text 1", "Text 2", ...
tab_labels <- function(texts) {
  labels <- names(texts) %||% rep("", length(texts))
  labels[is.na(labels)] <- ""
  default <- if (length(texts) == 1) "Text" else str_c("Text ", seq_along(texts))
  labels[labels == ""] <- default[labels == ""]
  labels
}

# Match `colors` to the tabs. Returns one color (or NA) per text tab.
resolve_colors <- function(colors, labels) {
  out <- rep(NA_character_, length(labels))
  if (is.null(colors)) return(out)
  
  if (is.null(names(colors))) {
    if (length(colors) != length(labels)) {
      stop("Unnamed `colors` must have one entry per text (", length(labels),
           "); use NA to keep a tab's default, or name the colors by tab label.",
           call. = FALSE)
    }
    return(unname(colors))
  }
  
  unknown <- setdiff(names(colors), labels)
  if (length(unknown) > 0) {
    stop("`colors` names not matching any tab: ", str_flatten_comma(unknown),
         ". Tabs are: ", str_flatten_comma(labels), ".", call. = FALSE)
  }
  out[match(names(colors), labels)] <- unname(colors)
  out
}

# HTML attributes for one tab button: class, plus a color style when given.
# Text color is picked from the background's brightness.
color_attrs <- function(color) {
  if (is.na(color)) return('class="tt-tab"')
  
  ch <- tryCatch(
    grDevices::col2rgb(color),
    error = function(e) stop("Not a valid color: '", color, "'", call. = FALSE)
  )
  hex <- grDevices::rgb(ch[1, 1], ch[2, 1], ch[3, 1], maxColorValue = 255)
  luminance <- (0.299 * ch[1, 1] + 0.587 * ch[2, 1] + 0.114 * ch[3, 1]) / 255
  fg <- if (luminance > 0.6) "#111111" else "#ffffff"
  
  str_c('class="tt-tab tt-colored" style="--tt-bg: ', hex, '; --tt-fg: ', fg, '"')
}

# Cut the document into: [..before </head>] [</head> .. <body ..>] [body] [</body> ..]
split_html <- function(html) {
  head_close <- str_locate(html, regex("</head\\s*>", ignore_case = TRUE))
  body_open  <- str_locate(html, regex("<body\\b[^>]*>", ignore_case = TRUE))
  body_close <- str_locate_all(html, regex("</body\\s*>", ignore_case = TRUE))[[1]]
  
  if (anyNA(head_close) || anyNA(body_open) || nrow(body_close) == 0) {
    stop("Could not find <head>...</head> and <body>...</body> in the file.", call. = FALSE)
  }
  body_close_start <- body_close[nrow(body_close), "start"]
  
  list(
    before_head_end        = str_sub(html, 1, head_close[1, "start"] - 1),
    head_end_to_body_open  = str_sub(html, head_close[1, "start"], body_open[1, "end"]),
    body                   = str_sub(html, body_open[1, "end"] + 1, body_close_start - 1),
    body_close_to_end      = str_sub(html, body_close_start)
  )
}

# Exact inverse of the injection: no-op if the file has no tabs
strip_text_tabs <- function(html) {
  if (!str_detect(html, fixed(mark$begin))) return(html)
  
  loc <- function(marker) str_locate(html, fixed(marker))
  
  # 1. style block in <head>
  s <- loc(mark$style_begin)
  e <- loc(mark$style_end)
  html <- str_c(str_sub(html, 1, s[1, "start"] - 1), str_sub(html, e[1, "end"] + 1))
  
  # 2. body wrapper: keep only what sits between the article markers
  b  <- loc(mark$begin)
  ab <- loc(mark$article_begin)
  ae <- loc(mark$article_end)
  en <- loc(mark$end)
  original <- str_sub(html, ab[1, "end"] + 1, ae[1, "start"] - 1)
  
  str_c(str_sub(html, 1, b[1, "start"] - 1), original, str_sub(html, en[1, "end"] + 1))
}

build_blocks <- function(article_label, texts, colors = NULL, article_color = NULL) {
  # one entry per extra tab; glue() is vectorised over these
  n      <- seq_along(texts)
  labels <- htmlEscape(tab_labels(texts))
  bodies <- text_to_html(unname(texts))
  attrs  <- resolve_colors(colors, tab_labels(texts)) |> map_chr(color_attrs)
  article_attrs <- color_attrs(article_color %||% NA_character_)
  
  buttons <- glue(
    '<button <<attrs>> role="tab" id="tt-tab-<<n>>" aria-controls="tt-panel-<<n>>" aria-selected="false"><<labels>></button>',
    .open = "<<", .close = ">>"
  )
  panels <- glue(
    '<div class="tt-panel tt-text" id="tt-panel-<<n>>" role="tabpanel" aria-labelledby="tt-tab-<<n>>">
  <div class="tt-inner"><pre><<bodies>></pre></div>
</div>',
    .open = "<<", .close = ">>", .trim = FALSE
  )
  
  list(
    head = glue(
      head_template,
      style_begin = mark$style_begin, style_end = mark$style_end,
      .open = "<<", .close = ">>", .trim = FALSE
    ),
    body_before = glue(
      body_before_template,
      begin = mark$begin, article_begin = mark$article_begin,
      article_label = htmlEscape(article_label),
      article_attrs = article_attrs,
      buttons = str_flatten(buttons, "\n  "),
      .open = "<<", .close = ">>", .trim = FALSE
    ),
    body_after = glue(
      body_after_template,
      article_end = mark$article_end, end = mark$end,
      panels = str_flatten(panels, "\n"),
      .open = "<<", .close = ">>", .trim = FALSE
    )
  )
}

head_template <- r"-(<<style_begin>><style>
  .tt-bar { position: sticky; top: 0; z-index: 1000; display: flex; gap: 4px;
            padding: 8px 12px 0; background: #2b2f36; overflow-x: auto; }
  .tt-tab { border: 0; padding: 9px 18px; cursor: pointer; font: 14px system-ui, sans-serif;
            white-space: nowrap; color: #cfd3da; background: #3a3f48; border-radius: 6px 6px 0 0; }
  .tt-tab:hover { background: #454b56; }
  .tt-tab[aria-selected="true"] { background: #fff; color: #111; font-weight: 600; }
  /* per-tab colors: set via --tt-bg / --tt-fg on the individual button */
  .tt-tab.tt-colored { background: var(--tt-bg); color: var(--tt-fg); opacity: .7; }
  .tt-tab.tt-colored:hover { opacity: .9; }
  .tt-tab.tt-colored[aria-selected="true"] { opacity: 1; box-shadow: inset 0 -3px 0 rgba(255,255,255,.9); }
  .tt-panel { display: none; }
  .tt-panel.tt-active { display: block; }
  .tt-text .tt-inner { max-width: 820px; margin: 0 auto; padding: 24px 20px 60px; }
  .tt-text pre { white-space: pre-wrap; word-wrap: break-word; margin: 0; color: inherit;
                 font: 16px/1.65 Georgia, "Times New Roman", serif; }
</style><<style_end>>)-"

body_before_template <- r"-(<<begin>>
<div class="tt-bar" role="tablist">
  <button <<article_attrs>> role="tab" id="tt-tab-0" aria-controls="tt-panel-0" aria-selected="true"><<article_label>></button>
  <<buttons>>
</div>
<div class="tt-panel tt-active" id="tt-panel-0" role="tabpanel" aria-labelledby="tt-tab-0">
<<article_begin>>)-"

body_after_template <- r"-(<<article_end>>
</div>
<<panels>>
<script>
  (function () {
    var tabs = Array.prototype.slice.call(document.querySelectorAll('.tt-tab'));
    var scroll = {};
    function show(tab) {
      var current = tabs.filter(function (t) { return t.getAttribute('aria-selected') === 'true'; })[0];
      if (current) scroll[current.id] = window.scrollY;
      tabs.forEach(function (t) {
        var on = t === tab;
        t.setAttribute('aria-selected', on);
        document.getElementById(t.getAttribute('aria-controls')).classList.toggle('tt-active', on);
      });
      window.scrollTo(0, scroll[tab.id] || 0);
    }
    tabs.forEach(function (t, i) {
      t.addEventListener('click', function () { show(t); });
      t.addEventListener('keydown', function (e) {
        if (e.key === 'ArrowRight' || e.key === 'ArrowLeft') {
          var next = tabs[(i + (e.key === 'ArrowRight' ? 1 : tabs.length - 1)) % tabs.length];
          next.focus(); show(next);
        }
      });
    });
  })();
</script>
<<end>>
)-"

# examples --------------------------------------------------------------
# add_text_tabs(
#   "01181_PMC7811829.html",
#   texts = c(
#     Abstract = "First text...",
#     Notes    = "Second text..."
#   )
# )
#
# Color only the second tab red (others keep the default look):
# add_text_tabs("01181_PMC7811829.html",
#               c(Abstract = "...", Notes = "...", Summary = "..."),
#               colors = c(Notes = "red"))
#
# Same thing, positional (NA = default):
# add_text_tabs("01181_PMC7811829.html", c(Abstract = "...", Notes = "...", Summary = "..."),
#               colors = c(NA, "red", NA))
#
# Color the article tab too, mix names and hex codes:
# add_text_tabs("01181_PMC7811829.html", c(Abstract = "...", Notes = "..."),
#               article_color = "#2c7be5",
#               colors = c(Abstract = "seagreen", Notes = "#d9534f"))
#
# Run again with different texts: the old tabs are replaced, not stacked.
# add_text_tabs("01181_PMC7811829.html", c(Summary = "New text"))
#
# Go back to the original file:
# remove_text_tabs("01181_PMC7811829.html")