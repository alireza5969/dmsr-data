#!/usr/bin/env Rscript
# Usage: Rscript R/08_validate.r <source|github|cloudflare>
args <- commandArgs(trailingOnly = TRUE)
mode <- if (length(args)) args[1] else "source"
stopifnot(mode %in% c("source", "github", "cloudflare"))
root <- if (mode == "source") "." else file.path("dist", mode)

DATA_DIR <- "data/03_labor"; CSS_NAME <- "jats-preview.css"
CF_LIMIT <- 20000L; CF_MAX_BYTES <- 25 * 1024^2; GH_MAX_BYTES <- 1024^3

failures <- 0L
check <- function(ok, msg) {
  cat(if (isTRUE(ok)) "[ OK ] " else "[FAIL] ", msg, "\n", sep = "")
  if (!isTRUE(ok)) failures <<- failures + 1L
}
info <- function(...) cat("[info] ", ..., "\n", sep = "")

data_root <- file.path(root, DATA_DIR)
all_files <- list.files(data_root, recursive = TRUE)
html <- all_files[grepl("\\.html?$", all_files, ignore.case = TRUE)]
css  <- all_files[basename(all_files) == CSS_NAME]
info(length(html), " HTML and ", length(css), " ", CSS_NAME, " files in ", data_root)
info("HTML + CSS as-is = ", length(html) + length(css), " (Cloudflare Free limit ", CF_LIMIT, ")")

# ---- CSS checksums ----------------------------------------------------------
if (length(css)) {
  m <- tools::md5sum(file.path(data_root, css))
  print(table(unname(m)))
  if (mode != "cloudflare") check(length(unique(m)) == 1, "all CSS copies are byte-identical")
}

# ---- deployment totals (built targets) -------------------------------------
if (mode != "source") {
  deployed <- list.files(root, recursive = TRUE, all.files = TRUE)
  sizes <- file.size(file.path(root, deployed))
  info("deployed files: ", length(deployed), " | total size: ", round(sum(sizes) / 1024^2), " MiB")
  if (mode == "cloudflare") {
    check(length(deployed) <= CF_LIMIT, paste0("file count <= ", CF_LIMIT, " (headroom ", CF_LIMIT - length(deployed), ")"))
    check(max(sizes) <= CF_MAX_BYTES, "largest file <= 25 MiB")
  } else {
    check(sum(sizes) <= GH_MAX_BYTES, "published size <= 1 GB (GitHub Pages)")
  }

  # ---- manifest <-> files ---------------------------------------------------
  mf <- jsonlite::fromJSON(file.path(root, "manifest.json"), simplifyVector = FALSE)
  listed <- unlist(lapply(mf$groups, function(g) unlist(lapply(g$dirs, function(d)
    paste(g$name, d$name, unlist(d$files), sep = "/")))))
  check(!anyDuplicated(listed), "manifest has no duplicate entries")
  check(setequal(listed, html), paste0("manifest matches HTML files exactly (",
        length(setdiff(listed, html)), " listed-but-missing, ", length(setdiff(html, listed)), " unlisted)"))
}

# ---- redirects -------------------------------------------------------------
redir <- character()
rf <- file.path(root, "_redirects")
if (file.exists(rf)) {
  l <- trimws(readLines(rf)); l <- l[nzchar(l) & !startsWith(l, "#")]
  redir <- sub("^/", "", sub("\\s.*$", "", l))
  info(length(redir), " redirect rules")
}

# ---- local references inside every HTML file --------------------------------
norm <- function(p) {
  out <- character()
  for (s in strsplit(p, "/", fixed = TRUE)[[1]]) {
    if (s == "" || s == ".") next
    if (s == "..") out <- head(out, -1) else out <- c(out, s)
  }
  paste(out, collapse = "/")
}
ref_re <- "(?:href|src)\\s*=\\s*[\"']([^\"']+)[\"']"
resolved <- vector("list", length(html)); abs_refs <- character()
for (i in seq_along(html)) {
  f <- file.path(data_root, html[i])
  txt <- readChar(f, file.size(f), useBytes = TRUE)
  mm <- regmatches(txt, gregexpr(ref_re, txt, perl = TRUE, ignore.case = TRUE))[[1]]
  r <- sub(ref_re, "\\1", mm, perl = TRUE, ignore.case = TRUE)
  r <- sub("[?#].*$", "", r)
  r <- r[nzchar(r) & !grepl("^(?:[a-z][a-z0-9+.-]*:|//|#)", r, ignore.case = TRUE, perl = TRUE)]
  abs_refs <- c(abs_refs, r[startsWith(r, "/")])
  r <- r[!startsWith(r, "/")]
  if (length(r))
    resolved[[i]] <- vapply(r, function(x) norm(file.path(DATA_DIR, dirname(html[i]), x)), "", USE.NAMES = FALSE)
}
res <- unlist(resolved)
good <- unique(res)
good <- good[file.exists(file.path(root, good)) | good %in% redir]
bad <- !(res %in% good)
info(length(res), " relative references checked in ", length(html), " HTML files")
if (any(bad)) { cat("Most common missing targets:\n"); print(head(sort(table(basename(res[bad])), decreasing = TRUE), 10)) }
check(!any(bad), paste0("no broken relative references (", sum(bad), " broken)"))
check(length(abs_refs) == 0, paste0("no root-relative ('/...') references, which break on project sites (", length(abs_refs), " found)"))
info("note: url(...) references inside CSS files are not scanned.")

quit(status = if (failures) 1L else 0L)
