source("R/00_functions.r")
source("R/05_html-tabbing_function.r")

library(furrr)
library(magrittr)
library(dplyr)
library(purrr)
library(stringr)


pmc_primary_extracted_address <-
  "data/05_text-extracted/pmc_primary_extraction.rds"

if (file_exists(pmc_primary_extracted_address)) {
  pmcs_text_extracted <-
    read_rds(pmc_primary_extracted_address)
} else {
  pmcs <-
    readr::read_rds(
      "data/02_pmc/html_processed.rds"
    ) %>%
    dplyr::filter_out(
      removable
    )

  pmcs_text_extracted <-
    pmcs %>%
    mutate(
      ai_text = map(body, \(body) body %>% split_intro_methods(), .progress = TRUE)
      # references_txt = map_chr(references, \(refs) {
      #   reference_n <-
      #     refs %>%
      #     pluck("reference_index")
      #   reference_doi <-
      #     refs %>%
      #     pluck("reference_doi") %>%
      #     ifelse(is.na(.), " ", .)
      #   paste(reference_n, reference_doi, sep = "\t|\t", collapse = "\n")
      # }, .progress = TRUE),
    ) %>%
    tidyr::unnest_wider(col = ai_text, names_sep = "_")


  pmcs_text_extracted %>%
    readr::write_rds(
      pmc_primary_extracted_address,
      compress = "gz"
    )
}


create_tabs_replace_files <-
  function(path, intro, method) {
    which_is_na <-
      c(intro, method) %>%
      is.na() %>%
      which()

    colors <- c(Introduction = NA, Methodology = NA)

    colors[which_is_na] <- "magenta"

    tab_contents <-
      c(
        Introduction = intro,
        Methodology = method
      )

    temp_dir <-
      add_text_tabs(
        path,
        texts = tab_contents,
        backup = FALSE,
        colors = colors
      )

    # fs::file_delete(path = path)
    # fs::file_move(temp_dir, path)
  }



old_plan <- future::plan(future::multicore, workers = 10)


future_pwalk(
  pmcs_text_extracted,
  \(path, ai_text_intro, ai_text_methods, ...)
  create_tabs_replace_files(path, ai_text_intro, ai_text_methods),
  .progress = TRUE
)


future::plan(old_plan)
