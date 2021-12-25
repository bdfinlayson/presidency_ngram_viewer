library(rvest)
library(stringr)
source('./scraper/models/document.R')

###########################################
## Helper Methods
###########################################

get_html <-
  function(base_uri = 'https://www.presidency.ucsb.edu',
           path = '',
           sep = '/') {
    url <- str_glue(base_uri, path, .sep = sep)
    return(read_html(url))
  }

clean <- function(text) {
  cleaned <- text %>% 
    str_replace_all(',', ' ') %>% 
    str_trim() %>% 
    str_squish()
  return(cleaned)
}


list_presidents <- function(html) {
  links_list <- html %>%
    html_elements(
      '.view-presidents .view-content .views-row div div .prez-mos .field-diet-pictures a'
    ) %>%
    html_attr("href") %>% 
    str_trim()
  return(links_list)
}

list_documents <- function(html) {
  links_list <- html %>%
    html_elements('.view-content .views-row .views-field .field-content a') %>%
    html_attr("href") %>% 
    str_trim()
  return(links_list)
}

get_document_title <- function(html) {
  title <- html %>%
    html_element('.field-ds-doc-title h1') %>%
    html_text2() %>% 
    clean()
  return(title)
}

get_document_date <- function(html) {
  date <- html %>%
    html_element('.date-display-single') %>%
    html_attr('content') %>% 
    str_trim()
  return(date)
}

get_document_president_name <- function(html) {
  name <- html %>%
    html_element('.diet-title a') %>%
    html_text2() %>% 
    clean()
  return(name)
}

get_document_content <- function(html) {
  content <- html %>%
    html_element('.field-docs-content') %>%
    html_text2() %>% 
    clean()
  return(content)
}

get_document_citation <- function(html) {
  citation <- html %>%
    html_element('.ucsbapp_citation') %>%
    html_text2() %>% 
    clean()
  return(citation)
}

build_document <- function(html, uri) {
  document <- new(
    'document',
    title = get_document_title(html),
    date = get_document_date(html),
    president_name = get_document_president_name(html),
    content = get_document_content(html),
    citation = get_document_citation(html),
    uri = uri
  )
  document@length <- str_count(document@content, "[[:alpha:]]+")
  return(document)
}


###########################################
## Scrape presidential data
###########################################

presidents_uris <- get_html(path='presidents') %>%
  list_presidents()

for(uri in presidents_uris) {
  president <- sapply(str_split(uri, pattern='/'), tail, 1)
  print(c('PREPARING FETCH FOR PRESIDENT: ', president))
 
  csv_name <- str_glue('data', 'presidents_scraped', str_glue(president, 'csv', .sep='.'), .sep='/')
  print(c('PREPARING CSV: ', csv_name))
  data_frame <- data.frame(document_title = character(),
                           document_date = character(),
                           president_name = character(),
                           text_content = character(),
                           citation = character(),
                           word_count = integer(),
                           document_uri = character(),
                           stringsAsFactors = FALSE)
  # print(c('INITIALIZING CSV: ', csv_name))
  write.table(data_frame, file = csv_name, sep = ',')
  
  page <- 1
  has_documents <- TRUE
  while (has_documents) {
    # print('BUILDING DOCUMENT LIST...')
    path <- str_glue(uri, '?page=', as.character(page), .sep='')
    print(c('CURRENT PAGE: ', path))
    print('FETCHING DOCUMENTS...')
    document_uris <- get_html(path=path) %>%
      list_documents()
    
    if(length(document_uris) > 0) {
      for(doc_uri in document_uris) {
        # print(c('FETCHING DOCUMENT: ', str_trim(doc_uri)))
        document <- get_html(path=doc_uri) %>%
          build_document(doc_uri)
        # print(c('FETCHED DOCUMENT: ', document@title))
        row <- data.frame(document@title,
                          document@date,
                          document@president_name,
                          document@content,
                          document@citation,
                          document@length,
                          document@uri)
        write.table(row, file = csv_name, sep = ',', append = TRUE, quote = FALSE, col.names = FALSE, row.names = FALSE)
        # print('APPENDED TO CSV')
      }
      page <- page + 1
      # print(c('CONTINUING TO PAGE: ', page))
    }
    else {
      print('NO NEW DOCUMENTS FOUND')
      print(c('ENDING DOCUMENT FETCH FOR PRESIDENT: ', uri))
      has_documents <- FALSE
    }
  }
}

