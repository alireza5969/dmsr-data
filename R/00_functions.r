library(magrittr)

write_env_vars <- 
  function(var_name, var_content, secret = FALSE) {
    # To write variables that persist across sessions
    
    if (!is.logical(secret)) stop("Secret must be logical")
    
    if (secret) {
      output_file <- ".Renviron.local"
    } else {
      output_file <- ".Renviron"
    }
    
    text_output <- 
      paste(
        var_name,
        "=",
        var_content
      )
    
    readr::write_file(
      text_output,
      file = output_file,
      append = TRUE
    )
    
  }

read_env_vars <- 
  function(var_name, secret = FALSE) {
    
    if (!is.logical(secret)) stop("Secret must be logical")
    
    if (secret) {
      input_file <- ".Renviron.local"
    } else {
      input_file <- ".Renviron"
    }
    
    readRenviron(input_file)
    Sys.getenv(var_name) %>% 
      return()
    
  }


parse_pubmed <- function(x) {
  library(xml2)
  library(tibble)
  
  doc <- read_xml(paste0("<PubmedArticle>", x, "</PubmedArticle>"))
  
  # Extract abstract
  abstract_nodes <- xml_find_all(doc, ".//Abstract/AbstractText")
  
  abstract <- if (length(abstract_nodes) == 0) {
    NA_character_
  } else {
    labels <- xml_attr(abstract_nodes, "Label")
    
    text <- xml_text(abstract_nodes)
    
    # Structured abstract
    if (any(!is.na(labels))) {
      paste(
        ifelse(
          is.na(labels) | labels == "",
          text,
          paste0(labels, ": ", text)
        ),
        collapse = " "
      )
    } else {
      # Unstructured abstract
      paste(text, collapse = " ")
    }
  }
  
  tibble(
    pmid = xml_text(xml_find_first(doc, ".//PMID")),
    title = xml_text(xml_find_first(doc, ".//ArticleTitle")),
    journal = xml_text(xml_find_first(doc, ".//Journal/Title")),
    year = xml_text(xml_find_first(doc, ".//PubDate/Year")),
    doi = xml_text(
      xml_find_first(doc, './/ArticleId[@IdType="doi"]')
    ),
    pmc = xml_text(
      xml_find_first(doc, './/ArticleId[@IdType="pmc"]')
    ),
    abstract = abstract
  )
}


extract_initials <- 
  function(fullnames) {
    # extract the initials from the full name
    
    fullnames %>% 
      purrr::map_chr(
        \(x) {
          x %>% 
            stringr::str_split_1(pattern = " ") %>% 
            stringr::str_sub(1, 1) %>% 
            paste0(collapse = "") %>% 
            tolower() %>% 
            return()
        }
      )
    
  }


pad_maker <- 
  function(num, max_num) {
    
    stringr::str_pad(num, stringr::str_count(max_num), pad = "0") %>% 
      return
    
  }


dir_names_distribute_workload <- 
  function(num, max_num) {
    
    num <- as.integer(num)
    
    quotient <- 
      (num - 1) %/% 100
    
    beg <- 
      ((100 * quotient) + 1) %>% 
      pad_maker(max_num = max_num)
    
    end <- 
      (100 * (quotient + 1)) %>% 
      pad_maker(max_num = max_num)
    
    return(paste0(beg, "-", end))
  }
