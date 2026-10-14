set.seed(42069)

source("R/00_functions.r")

library(magrittr)
# library(rvest)
library(httr2)
library(dplyr)
library(xml2)
library(xslt)
library(fs)
library(glue)
library(purrr)
library(httr2)
library(gert)
library(tibble)
library(stringr)

N_FEASIBLE <- 2e4

pmc_data <-
  readr::read_rds("data/01_pubmed/pubmed_records.rds") %>%
  mutate(success = 0)

# PMC prohibits downloading HTML files directly
# This method uses AWS CLI cited in the notes/


# Paths and settings -------------------------------------------------------

pmcids <- pmc_data$pmc # your list; keep the "PMC" prefix

xsl_dir <- "data/02_pmc/JATSPreviewStylesheets"
temp_dir <- "temp"
out_dir <- "data/03_labor"
base_url <- "https://pmc-oa-opendata.s3.amazonaws.com"


xsl <-
  read_xml(
    path(
      xsl_dir,
      "xslt",
      "main",
      "jats-html.xsl"
    )
  )


# fetch last state

current_list <- 
  fs::dir_ls(
    "data/03_labor/",
    recurse = TRUE, 
    glob = "*.html"
    ) %>%
  stringr::str_extract("PMC.*.html") %>%
  stringr::str_remove(".html")

pmc_data <-
  pmc_data[!(pmc_data %in% current_list)]

for (i in seq_along(pmcids)) {
  print(i)

  pmcid <- pmcids[i]

  Sys.sleep(.2)
                                      
  cat100 <-
    dir_names_distribute_workload(i, 2e4, 1e2)
  cat500 <-
    dir_names_distribute_workload(i, 2e4, 5e2)

  html_dir <-
    fs::path(
      out_dir,
      cat500,
      cat100,
      paste0(pad_maker(length(current_list) + i, 2e4), ".", pmcid),
      ext = "html"
    )


  if (fs::file_exists(html_dir)) next

  # Download pmc xml and convert it to html
  status <-
    process_pmcid_safely(
      pmcid,
      html_dir,
      "temp/temp.xml"
    )

  if (status != "ok" && !startsWith(status, "skipped")) {
    message(pmcid, ": ", status)
  }

  tryCatch(
    file_copy(
      path(
        xsl_dir,
        "jats-preview.css"
      ),
      fs::path_dir(html_dir),
      overwrite = FALSE
    ),
    error = function(e) NULL
  )

  if (!(i %% 100) | (i == length(pmcids))) {
    commit_changes(
      commit_message = paste0("saved up to ", i)
    )
  }
}
