# This file reads all HTML files and creates a tibble of its content and metadata
# The result will be saved in chunks for later use

library(magrittr)
library(dplyr)
library(rvest)
library(stringr)
library(purrr)
library(fs)
library(glue)
library(htmltools)
library(readr)

source("R/00_functions.r")
source("R/03_screening_preprocessing_extract.r")

pmc_list <-
  fs::dir_ls(
    path = "data/03_labor/",
    recurse = TRUE,
    glob = "*.html",
  )

extraction_site <- 
  "temp/extraction_chunks/"

final_site <- 
  "data/02_pmc/"

extraction_name <- "html_processed"

if (fs::is_dir_empty("data/02_pmc/html_processed")) {
  
  res <-
    extract_articles(
      pmc_list, 
      checkpoint_dir = extraction_site,
      workers = 1L
    ) 
  
  res %>% 
    mutate(
      no_abstract = is.na(abstract_text),
      no_received_on = is.na(received_date),
      is_worthless_a = is_worthless(titles = title, type = "a"),
      is_worthless_b = is_worthless(titles = title, type = "b"),
      no_pubmed_medline = map_lgl(
        body, 
        \(body_tbl) {
          body_tbl$paragraph_text %>%
            paste(collapse = "\n") %>%
            tolower %>% 
            str_detect("pubmed|medline") %>% 
            !.
        }, .progress = TRUE),
      body_word_count = map_int(
        body, 
        \(body_tbl) {
          body_tbl$paragraph_text %>%
            paste(collapse = "\n") %>%
            stringi::stri_count_words()
        }, .progress = TRUE)
      # This part was used to first remove some papers that din't meet criteria
      # I had to remove the actual file that contained the whole dataset
      # Therefore, this makes the reproducibility a little bit harder
      # Those who want to reproduce the results, have to run the process from the begning
      # and uncomment this file
      
      # removable = if_else(
      #   if_any(
      #     no_abstract:no_pubmed_medline
      #     ) | body_word_count < 1500, 
      #   TRUE, 
      #   FALSE
      #   )
    ) %>% 
    write_rds_chunks(path(final_site, extraction_name), )
  
  file_delete(dir_ls(extraction_site))
  
  res %>% 
    filter(removable) %>% 
    .$path %>% 
    file.remove
  
}  else {
  
  res <- 
    read_rds_chunks(path(final_site, extraction_name))
  
}
