set.seed(42069)

source("R/00_functions.r")

library(magrittr)
library(rvest)
library(httr2)
library(dplyr)

pmc_data <-
  readr::read_rds("data/01_pubmed/pubmed_records.rds") %>%
  mutate(success = 0)


for (i in seq_len(nrow(pmc_data))) {
  print(i)

  if (pmc_data[[i, "success"]] == 1) next

  url <- pmc_data[[i, "url"]]

  success <- tryCatch(
    {
      response <- request(url) |>
        req_user_agent(
          "open-source researcher (contact: alireza@sadeghimd.ir)"
        ) |>
        req_timeout(60) |>
        req_retry(
          max_tries = 5,
          retry_on_failure = TRUE,
          is_transient = function(resp) {
            resp_status(resp) %in% c(
              408, 425, 429, 500, 502, 503, 504
            )
          },
          backoff = function(tries) min(2^tries, 30)
        ) |>
        req_perform()

      response %>%
        resp_body_string() %>%
        readr::write_file(
          paste0(
            pmc_data[[i, "dir"]],
            "/",
            pmc_data[[i, "pmc"]],
            "_fulltext",
            ".html"
          )
        )


      TRUE
    },
    error = function(e) {
      message("Failed: ", url, " — ", conditionMessage(e))
      FALSE
    }
  )

  pmc_data[[i, "success"]] <- as.integer(success)

  if ((i %% 50) == 0) {
    pmc_data %>%
      readr::write_rds("data/02_pmc/pmc_temp_", i, ".rds")
  }

  Sys.sleep(0.15)
}
