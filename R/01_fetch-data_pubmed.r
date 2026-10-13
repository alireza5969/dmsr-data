set.seed(42069)


readRenviron(".Renviron.local")

library(magrittr)
library(dplyr)

# I should have used this syntax to get the publications with the right license only.
# However, I corrected this mistake by using `PMC Open Access Subset` and `BioC API`.

# QUERY <-
#   # Only systematic reviews with available free full text are included
#   '("systematic review"[Title]) AND (ffrft[Filter]) AND (2021/1/1:2026/10/5[pdat]) AND (pubmed pmc open access[filter])'

QUERY <-
  # Only systematic reviews with available free full text are included
  '("systematic review"[Title]) AND (ffrft[Filter]) AND (2021/1/1:2026/10/5[pdat])'

# ---

epm <- easyPubMed::epm_query(QUERY)

epm <-
  easyPubMed::epm_fetch(
    epm,
    format = "xml",
    api_key = read_env_vars("API_KEY", secret = TRUE)
  )

# ---

# Books must be removed
books <- epm@misc$book_chapter_pmids

# Keep the number of fetched data for the report
n_fetched <- epm@meta$exp_count

write_env_vars("N_FETCHED", n_fetched, secret = FALSE)

# ---

# Parse the fetched data

epm <- epm@raw

library(future)
plan(multicore, workers = 10)
plan(multisession, workers = 10)

epm_df <-
  furrr::future_map(epm, parse_pubmed, .progress = TRUE)


# ---

# Clean the data

PROTOCOL_REGEX <-
  "a protocol for|protocol for systematic|protocol for a|protocol for meta-analysis|systematic reivew protocol|meta-analysis protocol"

epm_df_clean <-
  epm_df %>%
  bind_rows() %>%
  select(pmid, doi, pmc, title, abstract, journal, year) %>%
  mutate(year = as.integer(year)) %>%
  # These records were removed: books, no abstract, no pmc, no doi, before 2021
  filter_out(
    is.na(pmc) |
      is.na(doi) |
      pmid %in% books |
      is.na(abstract) |
      # https://chatgpt.com/c/6ac6506f-930c-83eb-9f41-54c6639114bb
      year < 2021 |
      is.na(year) |
      stringr::str_detect(tolower(title), PROTOCOL_REGEX)
  ) %>%
  # some records' `pmc` value are not like 'PMC....'. Introduces unnecessary complexity.
  filter(stringr::str_detect(tolower(pmc), "pmc")) %>%
  select(pmid, doi, pmc)


# ---

# Write the file

epm_df_clean %>%
  readr::write_rds(
    read_env_vars("PUBMED_RECORDS_ADDRESS"),
    compress = "xz"
  )


"
alireza sadeghi
is the best
" %>% stringr::str_squish()
