library(dplyr)
library(fs)
library(magrittr)
library(stringr)

all_htmls <-
  dir_ls("data/03_labor/", recurse = TRUE, glob = "*.html") %>% 
  path_split


emethod_paths <- 
  all_htmls %>% 
  purrr::map_chr(\(path_vec) {
    path_vec[2] <- "06_emethods"
    path_vec[5] <- 
      path_vec[5] %>% 
      str_c("emethod.", .) %>% 
      str_replace("html", "txt")
    path_vec %>% 
      paste(collapse = "/") 
  })

dir_create(path_dir(emethod_paths))
file_create(emethod_paths)
