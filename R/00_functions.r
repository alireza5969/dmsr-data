library(magrittr)
library(xml2)
library(tibble)

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


# Detect worthless publications ----


is_worthless <- 
  function(titles, type = "both") {
    
    
    worthless_a <- c(
      C1_correction = r"-(^\W*((author|publisher|editorial|joint|printer)'?s?'?\s+|additions?\s*(/|and)\s*)?(corrections?|correction\s+notice|notice\s+of\s+correction)\s*($|[:.\[(-]|to\s*:|to\s+["'])|^\W*corrections?\s+and\s+clarifications?)-",
      C1c_pnas      = r"-(^\W*corrections?\s+for\s+.{2,100}?\bet\s+al\b)-",
      C2_corrigendum= r"-(\bcorrigend(um|a|ium)\b)-",
      C3_erratum    = r"-(\b(err?att?(um|a)|erat(um|a)|fe\s+de\s+erratas?)\b)-",
      C4_addendum   = r"-(^\W*addend(um|a)\b)-",
      C5_retraction = r"-(^\W*((partial|editorial|formal|author|authors)\s+)?(retraction|retracted|withdrawn|withdrawal)(\s+(note|notice|statement|article|paper|publication))?(\s*$|\s*[.\[(-]|(\s+(of|to|for))?\s*:))-",
      C5b_notice_of = r"-(^\W*(notice|statement|note)\s+of\s+(formal\s+)?(retraction|withdrawal|removal|duplicate\s+publication|redundant\s+publication|plagiarism)\b)-",
      C6_eoc        = r"-(\bexpressions?\s+of\s+concern\b)-",
      C7_pub_note   = r"-(^\W*((publisher|editor|editorial|journal)'?s?'?\s+(note|notice|statement)\b|note\s+from\s+the\s+(editors?|publisher)))-",
      C9_comment    = r"-(^\W*(((comments?|replys?|replies|rebuttals?|rejoinders?|in\s+reply)\s*((to|on|re|regarding)\b|:))|(authors?'?s?'?\s+(reply|response)\b)|re\s*:))-",
      C9b_response  = r"-(^\W*responses?\s+to\s+(the\s+)?(comments?|letters?|critique|criticisms?|editorials?|commentary|discussions?|reviewers?|"|'))-",
      C11_tags      = r"-([\[(]\s*(erratum|errata|corrigendum|correction|retracted|retraction|withdrawn|addendum|expression\s+of\s+concern|republished)\s*[\])]|\bthis\s+(corrects|retracts)\s+the\s+article\b)-",
      C12_foreign   = r"-(^\W*(correccion(\s+de\s+errores)?|correcao|retificacao|rectificatif|correctif|berichtigung|korrektur|richtigstellung|rettifica|retractacion|retractation|zuruckziehung|erratum\s+(zu|a|al|para))\b)-",
      
      E1a_editorial = r"-(^\W*((guest|invited|inaugural|joint|special\s+issue|new|first|final|farewell)\s+)*editorials?\s*($|[:.\[(-]|(note|comment|introduction|statement|board|policy)\b))-",
      E1b_from_ed   = r"-(^\W*((letter|message|note)\s+)?from\s+the\s+(editors?|editor-in-chief|editorial\s+board|chair|president|publisher|desk)\b|^\W*editor'?s?'?\s+(choice|letter|desk|column|picks?|page|welcome|message|farewell)\b)-",
      E2_letter     = r"-(^\W*(letters?\s+to\s+(the\s+)?editors?|to\s+the\s+editors?|dear\s+(editors?|sir)|cartas?\s+(al|a\s+la)\s+(editor|director|redaccion)|lettre\s+a\s+la\s+redaction|brief\s+an\s+die\s+redaktion|leserbrief)\b|\bletters?\s+to\s+the\s+editors?\b|^\W*(letters?|correspondence|readers?'?\s+letters?)\W*$|^\W*correspondence\s*[:.-])-",
      E3a_book_rev  = r"-(^\W*book\s+reviews?\b|\bbook\s+reviews?\s*[:.-]|\[(book\s+)?reviews?\]|^\W*reviews?\s+of\s+(the\s+)?(books?|films?|movies?|exhibitions?|exhibits?|websites?|dvds?|documentary)\b|^\W*(rezension|buchbesprechung|resena|compte\s+rendu|comptes\s+rendus|recensione|recensao|recension)\b)-",
      E4a_abstract  = r"-(^\W*(abstracts?|posters?|oral\s+(presentations?|abstracts?)|meeting\s+abstracts?|conference\s+abstracts?|poster\s+(presentations?|abstracts?|session)|invited\s+(talks?|lectures?|speakers?))\s*(:|#|-|\d|$|from\b|of\b))-",
      E5a_obit      = r"-(^\W*(obituar(y|ies)|in\s+memoriam|in\s+memory\s+of|in\s+remembrance(\s+of)?|tribute\s+to|a\s+tribute\s+to|in\s+tribute\s+to|eulogy|necrolog\w*|nachruf|obituario|in\s+honou?r\s+of|festschrift|laudatio|celebrating\s+the\s+(life|career|legacy))\b)-",
      E6_preface    = r"-(^\W*(preface|foreword|forword|prologue|epilogue|afterword|prologo|prefacio|vorwort|prefazione|avant-propos)\s*($|[:.(-]|(to|of)\b)|^\W*((guest\s+)?editors?'?s?'?\s+)?introduction\s+(to|of|for)\s+(the\s+|this\s+)?(special|focus|themed|virtual|thematic|topical|joint)\s+(issue|section|volume|collection|theme|topic|series|forum|cluster)\b|^\W*(guest\s+)?editors?'?s?'?\s+(introduction|preface|foreword|note|letter|welcome|comment|remarks?)\b|^\W*introduction\W*$)-",
      E7_ack        = r"-(^\W*acknowledg(e)?ments?\W*$|\b(reviewers?|referees?)\s+(acknowledg\w+|list|thanks|appreciation)\b|\backnowledg\w+\s+(to|of)\s+(our\s+|the\s+)?(peer\s+)?(reviewers|referees)\b|\b(thanks?|thank\s+you|gratitude|appreciation)\s+(to|for|from)\s+(our\s+|the\s+)?(peer\s+)?(reviewers|referees|editors|contributors)\b|\b(list\s+of|our)\s+(peer\s+)?(reviewers|referees)\b)-",
      E8_calls      = r"-(\b(call\s+for\s+(papers?|abstracts?|submissions?|proposals?|nominations?|manuscripts?|applications?|participation|contributions?)|(instructions|information|guide|guidelines)\s+(to|for)\s+authors|author\s+guidelines|submission\s+guidelines|notice\s+to\s+(authors|readers|contributors|subscribers)|save\s+the\s+date|(books|journals|publications)\s+received|calendar\s+of\s+events|upcoming\s+(events?|meetings?|conferences?)|job\s+(openings?|postings?|advertisements?)|positions?\s+(available|vacant|wanted)|classified\s+ads?)\b)-",
      E9_generic    = r"-(^\W*(announcements?|notices?|news|notes|miscellanea|miscellany|errata|corrigendum|bulletin|briefs?|events?|meetings?|calendar|advertisements?|adverts?|diary|forthcoming|personalia|people|obituaries|reviews?|abstracts?|contents|index|cover|masthead|editorial|foreword|preface|introduction|comments?|reply|letters?|correspondence|discussion|summary|appendix|bibliography|references|supplement|front\s+matter|back\s+matter)\W*$)-",
      E10_frontmatt = r"-(^\W*(table\s+of\s+contents|contents(\s+page)?|(front|back|end)\s*(matter|cover|pages?|papers?)|inside\s+(front|back)\s+cover|cover(\s+(page|image|picture|illustration|art|story|photo(graph)?|note|legend|caption|feature))?|frontispiece|title\s+page|masthead|board\s+of\s+(editors|directors|reviewers)|editors?\s+and\s+editorial\s+board|editorial\s+board|issue\s+(information|highlights|contents|overview)|in\s+this\s+issue|(author|subject|keyword|name|cumulative|annual|volume|general|title)\s+index|index\s+(of|to)\s+(authors|volume|subjects|titles|contents)|list\s+of\s+(contents|contributors|abbreviations|figures|tables|illustrations|authors|reviewers)|notes\s+on\s+contributors|about\s+the\s+(authors|contributors|cover|journal|issue)|author\s+biograph(y|ies)|copyright\s+(page|notice)|colophon|imprint|subscription\s+information|journal\s+information|aims\s+and\s+scope)\s*($|[:.\d(-]))-",
      E11a_news     = r"-(^\W*(news\s*(&|and)\s*views|news\s+(in\s+)?(brief|briefs|focus|roundup|round-up|digest|update|feature|item)|news|in\s+the\s+news|research\s+highlights?|journal\s+club|q\s*&\s*a|podcast|video\s+(abstract|summary|article)|short\s+takes?|snapshots?)\s*($|[:.\d-]))-",
      E11b_interview= r"-(^\W*(interview\s+with|a\s+conversation\s+with|in\s+conversation\s+with|highlights?\s+(from|of))\b)-",
      E13_supp_data = r"-(^\W*((supplement(ary|al)|electronic\s+supplementary)\s+(material|materials|information|data|files?|figures?|tables?|text|appendix|methods|content|digital\s+content|videos?|movies?|datasets?|code)\b|additional\s+file\s*\d*|(fig(ure)?s?|tables?|appendix|extended\s+data(\s+fig(ure)?)?)\s*[sS]?\d+[a-z]?(\s*[-,&]\s*[sS]?\d+[a-z]?)*\s+from\s|data\s+from\s*:|(decision\s+letter|author\s+response|peer\s+review\s+(file|history|report)|transparent\s+peer\s+review|reviewer\s+comments?)\s*($|[:.\d]|for\b)))-",
      E14_placeholder = r"-(^\W*(\[?\s*(title\s+(not\s+)?(available|provided|supplied)|no\s+title|untitled|not\s+available|n/a|none|unknown|na|tbd|test|title|null|undefined)\s*\]?)\W*$|^[^\p{L}]*$)-"
    )
    
    worthless_b <- c( 
      C1b_loose     = r"-(^\W*corrections?\s+(to|for|of|in)\b)-",
      C8_error_note = r"-(^\W*(errors?|mistakes?|incorrect|omitted|omission|missing|misspelled|wrong)\s+(in|of)?\s*(the\s+)?(fig(ure)?s?|tables?|byline|author|affiliations?|funding|acknowledg|references?|citations?|text|dosage|dose|equations?|legends?|labels?|captions?|names?|titles?|abstract|conflicts?|disclosures?)\b)-",
      C9c_closure   = r"-(^\W*(discussion|closure)\s+(of|to)\s*(:|"|'|\[)|closure\s+to\s+discussion)-",
      C10_update    = r"-(^\W*(clarifications?|amendments?|updates?|revisions?|replacement)\s*(to\s*)?:)-",
      C13_reprint   = r"-(^\W*(reprint(ed)?|republished)\s*(of|from)?\s*:)-",
      C14_cite_tail = r"-(\[[^\]]{3,100}\(\s*(19|20)\d\d\s*\)[^\]]{0,40}\]\W*$)-",
      C15_sentence  = r"-(^\W*in\s+(the\s+)?(original\s+)?(article|paper|publication|manuscript)\b.{0,250}\b(error|errors|incorrect|mistake|should\s+(read|be)|omitted|inadvertently)|\bhas\s+been\s+(corrected|retracted|withdrawn)\b)-",
      E3b_biblio    = r"-(\bISBN\b|\b\d{2,4}\s*pp\b|\bpp\.\s*\d|\b(hbk|pbk|hardback|hardcover|paperback)\b|\$\s?\d{2,3}(\.\d\d)?\b)-",
      E4b_proc      = r"-(\b(abstract\s+book|book\s+of\s+abstracts|abstracts\s+(from|of)\s+the|programme?\s+and\s+abstracts|proceedings\s+of\s+the\s+\d|annual\s+meeting\s+of\s+the|conference\s+(programme?|proceedings)|symposium\s+(programme?|abstracts?))\b)-",
      E5b_obit_weak = r"-(\bobituar(y|ies)\b|^\W*remembering\b|\(\s*(1[5-9]|20)\d\d\s*-\s*(1[5-9]|20)\d\d\s*\)\W*$)-",
      E6b_welcome   = r"-(^\W*(welcome|message)\s+(to|from)\b)-",
      E7b_rev_year  = r"-(\b(peer\s+)?reviewers?\s+(in|for|of)\s+(19|20)\d\d\b|\b(19|20)\d\d\s+(peer\s+)?(reviewers?|referees?)\b)-",
      E10b_vol_iss  = r"-(^\W*(vol(ume)?|issue|no|number|part)\.?\s*\d+(\s*[,;:.-]\s*(vol(ume)?|issue|no|number|part|pp?)\.?\s*[\d-]+)*\W*$)-",
      E12_opinion   = r"-(^\W*(viewpoints?|opinion|perspectives?|commentary|point\s+of\s+view|personal\s+view|my\s+view)\s*:)-"
    )
    
    
    
    if (type == "both") {
      worthless <- 
        stringr::str_detect(tolower(titles), paste0(c(worthless_a, worthless_b), collapse = "|"))
    } else if (type == "a") {
      worthless <- 
        stringr::str_detect(tolower(titles), paste0(worthless_a, collapse = "|"))
    } else {
      worthless <- 
        stringr::str_detect(tolower(titles), paste0(worthless_b, collapse = "|"))
    }
    
    return(worthless)
    
  }



sections_to_markdown <-
  function(df, stop_type = "methods", header = "##") {
  
      df <- df |> arrange(paragraph_id)
    
    # position of the last row belonging to the stop section (e.g. methods)
    stop_rows <- which(df$section_type == stop_type)
    if (length(stop_rows) == 0) {
      stop("No rows with section_type == '", stop_type, "' were found.")
    }
    last_row <- max(stop_rows)
    
    df |>
      slice(seq_len(last_row)) |>                       # methods + everything before it
      filter(!is.na(paragraph_text),
             str_trim(paragraph_text) != "") |>
      group_by(section_index, section_title) |>
      summarise(
        first_id = min(paragraph_id),
        text     = paste(paragraph_text, collapse = "\n\n"),
        .groups  = "drop"
      ) |>
      arrange(first_id) |>                              # keep document order
      mutate(md = paste0(header, " ", section_title, "\n\n", text)) |>
      pull(md) |>
      paste(collapse = "\n\n")
  }


build_markdown <- function(d, base_header_level = 2) {
  emitted <- character()
  out <- character()
  
  for (i in seq_len(nrow(d))) {
    path <- if (is.na(d$section_path[i])) d$section_title[i] else d$section_path[i]
    parts <- str_split(path, fixed(" > "))[[1]]
    
    # emit any header in the path that hasn't been written yet
    # (this also adds parent headers that have no paragraphs of their own)
    for (k in seq_along(parts)) {
      key <- paste(parts[seq_len(k)], collapse = " > ")
      if (!key %in% emitted) {
        emitted <- c(emitted, key)
        out <- c(out, paste0(strrep("#", base_header_level + k - 1), " ", parts[k]))
      }
    }
    
    txt <- d$paragraph_text[i]
    if (!is.na(txt) && str_trim(txt) != "") out <- c(out, txt)
  }
  
  paste(out, collapse = "\n\n")
}

split_intro_methods <- function(df, stop_type = "methods") {
  df <- df |> arrange(paragraph_id)
  
  m <- which(df$section_type == stop_type)
  if (length(m) == 0) {
    
    list(
      intro   = NA_character_,
      methods = NA_character_
    )
    
  } else {
    
    intro_rows   <- if (min(m) > 1) df[seq_len(min(m) - 1), ] else df[0, ]
    methods_rows <- df[min(m):max(m), ]
    
    list(
      intro   = build_markdown(intro_rows),
      methods = build_markdown(methods_rows)
    )
    
  }
  
}

# Save RDS in chunks ---

.rc_n    <- function(x) if (is.data.frame(x) || is.matrix(x)) nrow(x) else length(x)
.rc_type <- function(x) {
  if (is.data.frame(x)) "data.frame"
  else if (is.matrix(x)) "matrix"
  else if (is.list(x) || is.atomic(x)) "vector"
  else stop("Unsupported object type: ", class(x)[1])
}
.rc_slice <- function(x, type, i) {
  if (type %in% c("data.frame", "matrix")) x[i, , drop = FALSE] else x[i]
}
.rc_manifest_path <- function(dir, prefix) file.path(dir, paste0(prefix, "_manifest.rds"))

# Move a finished temp file onto its final name (works on Windows too)
.rc_commit <- function(tmp, path) {
  if (file.exists(path)) file.remove(path)
  if (!file.rename(tmp, path)) stop("Could not rename ", tmp, " to ", path)
}

.rc_save_manifest <- function(m, dir, prefix) {
  path <- .rc_manifest_path(dir, prefix)
  tmp  <- paste0(path, ".tmp")
  saveRDS(m, tmp)
  .rc_commit(tmp, path)
}

.rc_read_manifest <- function(dir, prefix, allow_incomplete = FALSE) {
  path <- .rc_manifest_path(dir, prefix)
  if (!file.exists(path)) stop("No manifest found for prefix '", prefix, "' in ", dir)
  m <- readRDS(path)
  if (!isTRUE(m$complete) && !allow_incomplete) {
    stop("This chunk set is incomplete (an earlier write was interrupted). ",
         "Re-run write_rds_chunks(..., resume = TRUE) or use allow_incomplete = TRUE.")
  }
  m
}


write_rds_chunks <- function(x, dir, prefix = "chunk", max_mb = 50,
                             compress = "gzip", overwrite = FALSE,
                             resume = FALSE, verbose = TRUE) {
  type <- .rc_type(x)
  n    <- .rc_n(x)
  if (n == 0) stop("Nothing to write: object has length/nrow 0.")
  
  # "MB" = 1e6 bytes (slightly stricter than 1024^2, so it's safe either way)
  max_bytes <- max_mb * 1e6
  target    <- 0.9 * max_bytes   # aim a bit below the limit
  
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  mpath    <- .rc_manifest_path(dir, prefix)
  existing <- list.files(dir, pattern = paste0("^", prefix, "_.*\\.rds(\\.tmp)?$"),
                         full.names = TRUE)
  fingerprint <- list(type = type, n = n, size = as.numeric(object.size(x)))
  
  files <- character(0)
  rows  <- integer(0)
  bytes_per_row <- fingerprint$size / n  # refined after the first chunk
  
  if (resume && file.exists(mpath)) {
    m <- readRDS(mpath)
    if (!identical(m$fingerprint, fingerprint)) {
      stop("The existing chunks were written from a different object ",
           "(type / size / row count differ). Use overwrite = TRUE to start over.")
    }
    if (isTRUE(m$complete)) {
      if (verbose) message("Already complete: nothing to do.")
      return(invisible(file.path(dir, m$files)))
    }
    if (!all(file.exists(file.path(dir, m$files)))) {
      stop("Some chunk files listed in the manifest are missing. ",
           "Use overwrite = TRUE to start over.")
    }
    file.remove(list.files(dir, pattern = "\\.tmp$", full.names = TRUE))  # leftovers
    files <- m$files
    rows  <- m$rows
    bytes_per_row <- m$bytes_per_row
    if (verbose) message(sprintf("Resuming after chunk %d (%s of %s rows done)",
                                 length(files), format(sum(rows), big.mark = ","),
                                 format(n, big.mark = ",")))
  } else if (length(existing)) {
    if (!(overwrite || resume)) {
      stop("Files with prefix '", prefix, "' already exist in ", dir,
           ". Use resume = TRUE to continue or overwrite = TRUE to start over.")
    }
    file.remove(existing)  # nothing usable to resume from
  }
  
  i <- sum(rows) + 1L
  k <- length(files)
  
  while (i <= n) {
    k    <- k + 1L
    path <- file.path(dir, sprintf("%s_%05d.rds", prefix, k))
    tmp  <- paste0(path, ".tmp")
    size <- max(1L, floor(target / bytes_per_row))
    j    <- min(n, i + size - 1L)
    
    repeat {
      saveRDS(.rc_slice(x, type, i:j), tmp, compress = compress)
      sz <- file.size(tmp)
      if (sz <= max_bytes) break
      if (j == i) {
        warning("A single row/element (index ", i, ") is ", round(sz / 1e6, 1),
                " MB, larger than max_mb = ", max_mb, ". Written as-is.")
        break
      }
      # chunk too big: shrink proportionally (and always by at least 1 row)
      cur <- j - i + 1L
      new <- max(1L, min(cur - 1L, floor(cur * target / sz)))
      j   <- i + new - 1L
    }
    
    .rc_commit(tmp, path)                 # chunk is now complete under its final name
    bytes_per_row <- sz / (j - i + 1L)
    files <- c(files, basename(path))
    rows  <- c(rows, j - i + 1L)
    .rc_save_manifest(list(type = type, n = n, files = files, rows = rows,
                           max_mb = max_mb, class = class(x),
                           fingerprint = fingerprint,
                           bytes_per_row = bytes_per_row,
                           complete = FALSE), dir, prefix)
    
    if (verbose) message(sprintf("[%d] %s: %s rows, %.1f MB",
                                 k, basename(path), format(j - i + 1L, big.mark = ","), sz / 1e6))
    i <- j + 1L
  }
  
  .rc_save_manifest(list(type = type, n = n, files = files, rows = rows,
                         max_mb = max_mb, class = class(x),
                         fingerprint = fingerprint,
                         bytes_per_row = bytes_per_row,
                         complete = TRUE), dir, prefix)
  invisible(file.path(dir, files))
}


read_rds_chunks <- function(dir, prefix = "chunk", chunks = NULL, combine = TRUE,
                            allow_incomplete = FALSE) {
  m <- .rc_read_manifest(dir, prefix, allow_incomplete)
  files <- m$files
  if (!is.null(chunks)) files <- files[chunks]
  parts <- lapply(file.path(dir, files), readRDS)
  if (!combine) return(parts)
  
  switch(m$type,
         data.frame = , matrix = do.call(rbind, parts),
         vector = do.call(c, parts)
  )
}


extract_prospero_ids <- function(text) {
  pattern <- "(?i)\\bCRD[\\s:\\-\u2010-\u2015]{0,3}42\\d{9}(?!\\d)"
  
  one <- function(x) {
    if (is.na(x) || !nzchar(x)) return(NA_character_)
    ids <- str_extract_all(x, pattern)[[1]]
    if (length(ids) == 0) return(NA_character_)
    ids |>
      str_remove_all("[^A-Za-z0-9]") |>   # drop spaces, hyphens, colons
      str_to_upper() |>
      unique()
  }
  
  map(text, one)
}
