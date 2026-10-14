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
      return()
  }


dir_names_distribute_workload <-
  function(num, max_num, divideBy = 100) {
    num <- as.integer(num)

    quotient <-
      (num - 1) %/% divideBy

    beg <-
      ((divideBy * quotient) + 1) %>%
      pad_maker(max_num = max_num)

    end <-
      (divideBy * (quotient + 1)) %>%
      pad_maker(max_num = max_num)

    return(paste0(beg, "-", end))
  }

# Download XML, convert to HTML, upload to Koofr ----

latest_version <-
  function(pmcid) {
    listing <-
      read_xml(
        glue(
          "{base_url}/?list-type=2&prefix={pmcid}.&delimiter=/"
        )
      )
    xml_ns_strip(listing)

    prefixes <-
      listing |>
      xml_find_all("//CommonPrefixes/Prefix") |>
      xml_text()

    if (length(prefixes) == 0) {
      return(NA_character_)
    }

    versions <-
      prefixes |>
      str_extract("(?<=\\.)\\d+(?=/$)") |>
      as.integer()
    prefixes[which.max(versions)] |> str_remove("/$")
  }

download_xml <-
  function(version, xml_path) {
    download.file(
      glue("{base_url}/{version}/{version}.xml"),
      destfile = xml_path, quiet = TRUE, mode = "wb"
    )
  }

# Drops image elements (keeps figure labels and captions), then applies the XSLT
convert_to_html <-
  function(xml_path, html_path) {
    doc <- read_xml(xml_path)

    doc |>
      xml_find_all("//graphic | //inline-graphic") |>
      xml_remove()

    doc %>%
      xml_xslt(xsl) |>
      xml2::write_html(html_path)
  }

# Download -> convert -> delete the XML. Returns a status string.
process_pmcid <-
  function(pmcid, html_path, xml_path) {
    if (file_exists(html_path)) {
      return("skipped (already done)")
    }

    version <- latest_version(pmcid)
    if (is.na(version)) {
      return("not in bucket")
    }

    # make sure output and temp directories exist
    dir_create(path_dir(html_path))
    dir_create(path_dir(xml_path))

    on.exit(if (file_exists(xml_path)) file_delete(xml_path))

    download_xml(version, xml_path)
    convert_to_html(xml_path, html_path)
    "ok"
  }

process_pmcid_safely <-
  function(pmcid, html_path, xml_path) {
    tryCatch(
      process_pmcid(pmcid, html_path, xml_path),
      error = \(e) stringr::str_c("error: ", conditionMessage(e))
    )
  }

koofr_config <-
  function() {
    user <- Sys.getenv("KOOFR_USER")
    password <- Sys.getenv("KOOFR_APP_PASSWORD")

    if (!nzchar(user) || !nzchar(password)) {
      stop(
        "Set KOOFR_USER and KOOFR_APP_PASSWORD environment variables.",
        call. = FALSE
      )
    }

    list(
      user = user,
      password = password,
      base_url = "https://app.koofr.net/dav/Koofr"
    )
  }

koofr_url <-
  function(path = character()) {
    cfg <- koofr_config()

    if (length(path) == 0L || all(!nzchar(path))) {
      return(cfg$base_url)
    }

    path <- path[nzchar(path)]

    encoded <-
      vapply(
        path,
        curl::curl_escape,
        FUN.VALUE = character(1)
      )

    paste0(cfg$base_url, "/", paste(encoded, collapse = "/"))
  }

koofr_request <-
  function(url, method, timeout = 3600) {
    cfg <- koofr_config()

    request(url) |>
      req_method(method) |>
      req_auth_basic(cfg$user, cfg$password) |>
      req_user_agent("R-koofr-workflow/1.0") |>
      req_timeout(timeout) |>
      req_retry(
        max_tries = 5,
        retry_on_failure = TRUE,
        is_transient = function(resp) {
          resp_status(resp) %in% c(408, 425, 429, 500, 502, 503, 504)
        }
      )
  }

koofr_mkdir <-
  function(remote_dir) {
    remote_dir <- gsub("^/+|/+$", "", remote_dir)

    if (!nzchar(remote_dir)) {
      return(invisible(TRUE))
    }

    parts <- strsplit(remote_dir, "/", fixed = TRUE)[[1]]

    if (any(!nzchar(parts)) || any(parts %in% c(".", ".."))) {
      stop("Invalid remote directory path.", call. = FALSE)
    }

    current <- character()

    for (part in parts) {
      current <- c(current, part)

      resp <- koofr_request(
        koofr_url(current),
        method = "MKCOL"
      ) |>
        req_error(
          is_error = function(resp) {
            # 405 means the collection already exists on many WebDAV servers.
            !resp_status(resp) %in% c(200, 201, 204, 405)
          }
        ) |>
        req_perform()

      # MKCOL normally returns 201 for a newly created collection.
      # 405 is accepted as "already exists"; other errors still propagate.
    }

    invisible(TRUE)
  }

koofr_upload <-
  function(
    files,
    remote_dir = "",
    timeout = 3600
  ) {
    files <- unique(as.character(files))

    if (length(files) == 0L) {
      stop("No files supplied.", call. = FALSE)
    }

    purrr::map_dfr(files, function(file) {
      tryCatch(
        koofr_upload_one(
          file = file,
          remote_dir = remote_dir,
          timeout = timeout
        ),
        error = function(e) {
          tibble(
            local_file = file,
            remote_path = NA_character_,
            bytes = if (file.exists(file)) {
              unname(file.info(file)$size)
            } else {
              NA_real_
            },
            http_status = NA_integer_,
            uploaded_at = Sys.time(),
            status = "error",
            error = conditionMessage(e)
          )
        }
      )
    })
  }

commit_changes <-
  function(commit_message = "Save progress") {
    gert::git_add(".")

    staged <- gert::git_status(staged = TRUE)

    if (nrow(staged) == 0) {
      message("Nothing to commit, skipping.")
      return(invisible(FALSE))
    }


    gert::git_commit(commit_message)
    gert::git_push()

    message("Checkpoint pushed to GitHub.")
  }
