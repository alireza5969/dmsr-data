#!/usr/bin/env Rscript
# Usage: Rscript R/07_build.r <github|cloudflare> [dist_dir]
suppressPackageStartupMessages(library(jsonlite))

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1 || !args[1] %in% c("github", "cloudflare")) {
  stop("usage: Rscript R/07_build.r <github|cloudflare> [dist_dir]")
}
target <- args[1]
dist <- if (length(args) >= 2) args[2] else file.path("dist", target)

DATA_DIR <- "data/03_labor"
CSS_NAME <- "jats-preview.css"
CF_LIMIT <- 20000L

stopifnot(dir.exists(DATA_DIR), file.exists("site/index.html"))

# ---- inventory (everything under DATA_DIR, relative paths) -----------------
rel <- list.files(DATA_DIR, recursive = TRUE)
src <- file.path(DATA_DIR, rel)
parts <- strsplit(rel, "/", fixed = TRUE)
depth <- lengths(parts)
is_css <- basename(rel) == CSS_NAME
is_html <- grepl("\\.html?$", rel, ignore.case = TRUE)

if (any(depth != 3L)) {
  warning(sum(depth != 3L), " file(s) are not at depth 3; they are copied but not listed in the manifest")
}
if (any(!is_css & !is_html)) {
  message("note: ", sum(!is_css & !is_html), " non-HTML/non-CSS files are copied unchanged")
}

# ---- CSS checksums -> canonical copy ---------------------------------------
md5 <- rep(NA_character_, length(rel))
md5[is_css] <- unname(tools::md5sum(src[is_css]))
tab <- sort(table(md5[is_css]), decreasing = TRUE)
message(
  "HTML files: ", sum(is_html), " | CSS files: ", sum(is_css),
  " | distinct CSS checksums: ", length(tab)
)
canonical <- if (length(tab)) names(tab)[1] else NA_character_
canon_src <- if (!is.na(canonical)) src[is_css][match(canonical, md5[is_css])] else NA_character_

# Only byte-identical copies of the canonical file may be dropped, and only on Cloudflare.
drop <- target == "cloudflare" & is_css & !is.na(md5) & md5 %in% canonical
if (target == "cloudflare" && length(tab) > 1) {
  message("WARNING: ", sum(is_css & !drop), " CSS file(s) differ from the canonical copy and are KEPT.")
}

# ---- fresh output dir, link-or-copy files ----------------------------------
unlink(dist, recursive = TRUE)
dir.create(dist, recursive = TRUE)

from <- src[!drop]
to <- file.path(dist, DATA_DIR, rel[!drop])
invisible(lapply(unique(dirname(to)), dir.create, recursive = TRUE, showWarnings = FALSE))
ok <- suppressWarnings(file.link(from, to)) # hard links: fast, no extra disk
if (!all(ok)) ok[!ok] <- file.copy(from[!ok], to[!ok])
stopifnot(all(ok))

# ---- manifest (HTML files only, depth 3) -----------------------------------
h <- which(is_html & depth == 3L)
df <- data.frame(
  g = vapply(parts[h], `[`, "", 1),
  d = vapply(parts[h], `[`, "", 2),
  f = vapply(parts[h], `[`, "", 3),
  stringsAsFactors = FALSE
)
df <- df[order(df$g, df$d, df$f), ]
groups <- lapply(split(df, df$g), function(gd) {
  list(
    name = gd$g[1],
    dirs = unname(lapply(split(gd, gd$d), function(dd) {
      list(name = dd$d[1], files = I(dd$f))
    }))
  )
})
manifest <- list(base = paste0(DATA_DIR, "/"), total = nrow(df), groups = unname(groups))
writeLines(as.character(toJSON(manifest, auto_unbox = TRUE)), file.path(dist, "manifest.json"))

# ---- site files -------------------------------------------------------------
stopifnot(file.copy("site/index.html", file.path(dist, "index.html")))
writeLines(
  c(
    "<!doctype html><meta charset=\"utf-8\">",
    "<meta name=\"viewport\" content=\"width=device-width,initial-scale=1\">",
    "<title>Not found</title><p>Not found."
  ),
  file.path(dist, "404.html")
)

# ---- Cloudflare: shared stylesheet + per-directory rewrites ----------------
if (target == "cloudflare" && any(drop)) {
  dir.create(file.path(dist, "assets"))
  stopifnot(file.copy(canon_src, file.path(dist, "assets", CSS_NAME)))
  dirs <- dirname(rel[drop])
  stopifnot(length(dirs) < 2000) # static-redirect limit
  writeLines(
    sprintf("/%s/%s/%s /assets/%s 200", DATA_DIR, dirs, CSS_NAME, CSS_NAME),
    file.path(dist, "_redirects")
  )
}

# ---- final count ------------------------------------------------------------
n <- length(list.files(dist, recursive = TRUE, all.files = TRUE))
message("Deployment files in ", dist, ": ", n)
if (target == "cloudflare") {
  message("Headroom under ", CF_LIMIT, ": ", CF_LIMIT - n)
  if (n > CF_LIMIT) stop("Over the Cloudflare Pages Free-plan file limit.")
}
