library(rvest)
library(stringr)

get_html <-
  function(base_uri = 'https://www.presidency.ucsb.edu',
           path = '',
           sep = '/') {
    str_glue(base_uri, path, .sep = sep) %>% 
      read_html()
  }

clean <- function(text) {
  cleaned <- text %>%
    str_replace_all(',', ' ') %>%
    str_trim() %>%
    str_squish()
  
  if(is.na(cleaned)) {
    return('')
  }
  else {
    return(cleaned)
  }
}

list_presidents <- function(html) {
  links_list <- html %>%
    html_elements(
      '.view-presidents .view-content .views-row div div .prez-mos .field-diet-pictures a'
    ) %>%
    html_attr("href") %>%
    str_trim()
}

list_documents <- function(html) {
  links_list <- html %>%
    html_elements('.view-content .views-row .views-field .field-content a') %>%
    html_attr("href") %>%
    str_trim()
}

get_document_title <- function(html) {
  title <- html %>%
    html_element('.field-ds-doc-title h1') %>%
    html_text2() %>%
    clean()
}

get_document_date <- function(html) {
  date <- html %>%
    html_element('.date-display-single') %>%
    html_attr('content') %>%
    clean()
}

get_document_president_name <- function(html) {
  name <- html %>%
    html_element('.diet-title a') %>%
    html_text2() %>%
    clean()
}

get_document_content <- function(html) {
  content <- html %>%
    html_element('.field-docs-content') %>%
    html_text2() %>%
    clean()
}

get_document_citation <- function(html) {
  citation <- html %>%
    html_element('.ucsbapp_citation') %>%
    html_text2() %>%
    clean()
}

get_categories <- function(html) {
  categories <- html %>%
    html_elements('.group-meta div a') %>% 
    html_text2() %>% 
    str_flatten(collapse = ";") %>% 
    clean()
}

get_location <- function(html) {
  location <- html %>%
    html_element('.field-spot-state') %>%
    html_text2() %>%
    clean()
}

build_document <- function(html, uri) {
  document <- new(
    'document',
    title = get_document_title(html),
    date = get_document_date(html),
    president_name = get_document_president_name(html),
    content = get_document_content(html),
    categories = get_categories(html),
    location = get_location(html),
    citation = get_document_citation(html),
    uri = uri
  )
  document@word_count <- str_count(document@content, "[[:alpha:]]+")
  return(document)
}
