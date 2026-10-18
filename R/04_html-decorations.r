library(fs)
library(magrittr)

source("R/04_html-decorations_annotate.r")
source("R/00_functions.r")

pmc_list <-
  fs::dir_ls(
    path = "data/03_labor/",
    recurse = TRUE,
    glob = "*.html",
  )

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


old_plan <- future::plan(future::multicore, workers = 10)
res <- furrr::future_map(pmc_list, safe_annotate,
  keywords = kws, .progress = TRUE
) |>
  dplyr::bind_rows()

# res %>% write_rds("highlight_res.rds")
dplyr::count(res, ok)
dplyr::filter(res, !ok)


future::plan(old_plan)