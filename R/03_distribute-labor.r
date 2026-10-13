set.seed(42069)

source("R/00_functions.r")

library(dplyr)
library(magrittr)

N_FEASIBLE <- 2e4

REVIEWER_NAMES <-
  c(
    "Mohammad Amin Shahlaee",
    "Ali Jamshidi Kerachi",
    "Fatemeh Zamani Marandi",
    "Pardis Habibi",
    "Kimia Falamarzi", 
    "Arashk Mallahzadeh",
    "Tahereh Something"
  )


pm_records_raw <-
  readr::read_rds(read_env_vars("PUBMED_RECORDS_ADDRESS"))

# ---

# for the sake of feasibility, i need to include only a subset of the records

pm_records_sliced <-
  pm_records_raw %>%
  slice_sample(n = N_FEASIBLE, replace = FALSE)

# allocate reviewers randomly

pm_records_alloc <-
  pm_records_sliced %>%
  mutate(
    reviewer = rep(extract_initials(REVIEWER_NAMES), length.out = nrow(.))
  ) %>%
  group_by(reviewer) %>%
  mutate(
    n = row_number()
  )


# create a directory structure

pm_records_w_dir <-
  pm_records_alloc %>%
  mutate(
    dir1 = dir_names_distribute_workload(n, max_num = max(n)),
    dir2 = paste(
      pad_maker(n, max_num = max(n)),
      pmc,
      sep = "."
    ),
    dir = paste("data/03_labor", reviewer, dir1, dir2, sep = "/"),
    url = paste0("https://pmc.ncbi.nlm.nih.gov/articles/", pmc)
  ) %>%
  select(-dir1, -dir2)

fs::dir_create(pm_records_w_dir$dir)

pm_records_w_dir %>%
  readr::write_rds("data/02_pmc/pmc.rds")
