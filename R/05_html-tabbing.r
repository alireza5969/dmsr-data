source("R/00_functions.r")
source("R/05_html-tabbing_function.r")
source("R/03_screening_preprocessing_extract.r")

library(furrr)
library(magrittr)
library(dplyr)
library(purrr)
library(stringr)


pmc_primary_extracted_address <-
  "data/05_text-extracted/"

extraction_site <-
  "temp/extraction_chunks/"

if (!is_dir_empty(pmc_primary_extracted_address)) {
  pmcs_text_extracted <-
    read_rds_chunks(pmc_primary_extracted_address)
} else {
  pmc_list <-
    fs::dir_ls(
      path = "data/03_labor/",
      recurse = TRUE,
      glob = "*.html",
    )

  pmcs <-
    extract_articles(
      pmc_list,
      checkpoint_dir = extraction_site,
      workers = 1L
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
    tidyr::unnest_wider(col = ai_text, names_sep = "_") %>%
    mutate(prospero = extract_prospero_ids(paste0(abstract_text, ai_text_methods)))


  pmcs_text_extracted %>%
    write_rds_chunks(
      pmc_primary_extracted_address
    )
}


create_tabs_replace_files <-
  function(path, intro, method, prospero, pmcid) {
    which_is_na <-
      c(intro, method) %>%
      is.na() %>%
      which()

    colors <-
      c(
        Introduction = NA,
        Methodology = NA,
        PROSPERO = NA
      )

    colors[which_is_na] <- "magenta"

    if (length(prospero) > 1) {
      colors["PROSPERO"] <- "magenta"
      prospero <-
        prospero %>%
        paste0(collapse = "\n")
    }

    tab_contents <-
      c(
        Introduction = intro,
        Methodology = method,
        PROSPERO = prospero
      )

    add_text_tabs(
      path,
      texts = tab_contents,
      backup = FALSE,
      colors = colors
    )
    
    add_tab_link(
      path, 
      'Open on PMC',
      fs::path('https://pmc.ncbi.nlm.nih.gov/articles/', pmcid)
    )

    # fs::file_delete(path = path)
    # fs::file_move(temp_dir, path)

    # return(list(content = tab_contents, col = colors))
  }



old_plan <- future::plan(future::multicore, workers = 10)


future_pwalk(
  pmcs_text_extracted,
  \(path, ai_text_intro, ai_text_methods, prospero, pmcid, ...)
  create_tabs_replace_files(
    path, 
    ai_text_intro, 
    ai_text_methods, 
    prospero, 
    pmcid
    ),
  .progress = TRUE
)


future::plan(old_plan)
