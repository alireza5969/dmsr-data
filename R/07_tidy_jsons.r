library(magrittr)
library(dplyr)
library(jsonlite)

all_htmls <- 
  fs::dir_ls("data/03_labor/", glob = "*.html", recurse = TRUE)

num_pmcid_conv <- 
  tibble(
    num_chr = all_htmls %>% stringr::str_extract('(?<=/)\\d+(?=\\.PMC)'),
    pmcid = all_htmls %>% stringr::str_extract("PMC\\d+")
  ) %>% 
  mutate(num_num = as.integer(num_chr))


.extract_jsons <- 
  function(owner) {
    
    raw_json <- 
      readr::read_file(paste0("labeling_jsons/", owner, ".json")) %>% 
      jsonlite::fromJSON()
    
    personal_keys <- 
      raw_json$categories %>% 
      as_tibble()
    
    all_annotations <- tibble()
    
    for (i in seq_along(raw_json$pages$title)) {
      
      pmcid <- 
        raw_json$pages$url[i] %>% 
        stringr::str_extract("PMC\\d+")
      
      page_annotations <- 
        raw_json$pages$annotations[[i]] %>% 
        tidyr::unnest_wider(col = quote) %>% 
        select(-note, -position) %>% 
        mutate(
          pmcid = pmcid,
          labler = owner
        )
      
      all_annotations <- 
        bind_rows(all_annotations, page_annotations)
      
    }
    
    spreadsheet <- 
      readxl::read_excel(paste0("data/", owner, ".xlsx")) %>% 
      mutate(n = as.integer(n)) %>% 
      rename_with(.fn = {\(x) paste0("excel_", x)}, .cols = everything()) %>% 
      select(!where(~ all(is.na(.x))))
      
    
    all_annotations <- 
      all_annotations %>% 
      left_join(personal_keys, by = join_by(color == id)) %>% 
      left_join(num_pmcid_conv, by = join_by(pmcid)) %>% 
      relocate(pmcid, labler, name) %>% 
      left_join(spreadsheet, by = join_by(num_num == excel_n))
    
    return(setNames(list(all_annotations), owner))
    
  }

owners <- 
  c('kf', "mas", "ajk", "fzm")

raw_data <- 
  purrr::map(owners, .extract_jsons, .progress = TRUE) %>% 
  unlist(recursive = F) 

purrr::iwalk(raw_data, \(x, idx) {
  
  x <- 
    x %>% 
    group_by(pmcid) %>% filter(any(!is.na(exact) & !is.na(excel_pm_syntax)))
  
  all_nums <- x$num_num %>% unique()
  all_nums <- all_nums[all_nums != 120]
  
  total <- 
    num_pmcid_conv %>% 
    filter(
      num_num <= max(all_nums),
      num_num >= min(all_nums)
    ) %>% 
    nrow()
  
  cat(
    paste(
      idx, "-->",
      length(all_nums), "out of",
      total, "-->" ,
      scales::percent(
        length(all_nums)/total),
      "inclusion rate",
      "\n\n"
      )
    ) 
})
