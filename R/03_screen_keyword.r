library(magrittr)
library(dplyr)
library(rvest)
library(stringr)

pmc_list <- 
  fs::dir_ls(
    path = "data/03_labor/",
    recurse = TRUE,
    glob = "*.html", 
  )


screening_kws <- 
  c(
    "pubmed",
    "received on"
  )

highlight_kws <- 
  c(
    "pubmed",
    "syntaxt",
    "received on",
    "strategy"
  )

pmc_tbl <- 
  tibble(
    pmc_dir = pmc_list,
    
  )