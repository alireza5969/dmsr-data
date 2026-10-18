source("R/04_html-decorations_annotate.r")
source("R/00_functions.r")

pmc_list <-
  fs::dir_ls(
    path = "data/03_labor/",
    recurse = TRUE,
    glob = "*.html",
  )

# Fail loudly instead of "succeeding" on an empty/missing data folder
if (length(pmc_list) == 0) {
  stop("No .html files found in data/03_labor/", call. = FALSE)
}

# Perform marking keywords

kws <-
  read.csv("env/kw_colors.csv")


safe_annotate <- function(path, keywords) {
  tryCatch(
    {
      r <- annotate_html(path, keywords = keywords, in_place = TRUE, backup = FALSE)
      tibble::tibble(
        path = path, ok = TRUE,
        n_annotations = r$n_annotations,
        n_nodes = r$n_text_nodes_modified,
        warnings = paste(r$warnings, collapse = " | "),
        counts = list(r$counts), error = NA_character_
      )
    },
    error = function(e) {
      tibble::tibble(
        path = path, ok = FALSE,
        n_annotations = NA_integer_, n_nodes = NA_integer_,
        warnings = NA_character_, counts = list(NULL),
        error = conditionMessage(e)
      )
    }
  )
}


# --- Parallel setup ----------------------------------------------------------
# GitHub-hosted Linux runners have 4 vCPUs (public repos) or 2 (private repos),
# so a hard-coded 10 workers would oversubscribe them. availableCores() adapts
# to whatever machine this runs on; min() keeps the original cap of 10.
n_workers <- min(10L, future::availableCores())
message("Using ", n_workers, " parallel worker(s)")

# multicore (forking) works on Linux/macOS runners but not on Windows, so fall
# back to multisession there.
old_plan <-
  if (future::supportsMulticore()) {
    future::plan(future::multicore, workers = n_workers)
  } else {
    future::plan(future::multisession, workers = n_workers)
  }

res <- furrr::future_map(pmc_list, safe_annotate,
  keywords = kws, .progress = TRUE
) |>
  dplyr::bind_rows()

future::plan(old_plan)


# --- Results -----------------------------------------------------------------
dir.create("output", showWarnings = FALSE)
saveRDS(res, "output/highlight_res.rds")

print(dplyr::count(res, ok))

failed <- dplyr::filter(res, !ok)
if (nrow(failed) > 0) {
  print(failed[, c("path", "error")], n = 20)
  # Non-zero exit marks the workflow run as failed; delete these two lines if
  # you'd rather tolerate per-file errors.
  quit(status = 1)
}
