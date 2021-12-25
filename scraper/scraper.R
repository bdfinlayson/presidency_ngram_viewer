library(rvest)
library(stringr)
source('./scraper/models/document.R')
source('./scraper/helpers/scraper_helper.R')

presidents_uris <- get_html(path = 'presidents') %>%
  list_presidents()

for (uri in presidents_uris) {
  president <- sapply(str_split(uri, pattern = '/'), tail, 1)
  print(c('PREPARING FETCH FOR PRESIDENT: ', president))

  csv_name <-
    str_glue('data',
             'presidents_scraped',
             str_glue(president, 'csv', .sep = '.'),
             .sep = '/')
  print(c('PREPARING CSV: ', csv_name))
  data_frame <- data.frame(
    document_title = character(),
    document_date = character(),
    president_name = character(),
    text_content = character(),
    citation = character(),
    categories = character(),
    location = character(),
    word_count = integer(),
    document_uri = character(),
    stringsAsFactors = FALSE
  )
  # print(c('INITIALIZING CSV: ', csv_name))
  write.table(data_frame, file = csv_name, sep = ',')

  page <- 1
  has_documents <- TRUE
  while (has_documents) {
    # print('BUILDING DOCUMENT LIST...')
    path <- str_glue(uri, '?page=', as.character(page), .sep = '')
    print(c('CURRENT PAGE: ', path))
    print('FETCHING DOCUMENTS...')
    document_uris <- get_html(path = path) %>%
      list_documents()

    if (length(document_uris) > 0) {
      for (doc_uri in document_uris) {
        # print(c('FETCHING DOCUMENT: ', str_trim(doc_uri)))
        document <- get_html(path = doc_uri) %>%
          build_document(doc_uri)
        # print(c('FETCHED DOCUMENT: ', document@title))
        row <- data.frame(
          document@title,
          document@date,
          document@president_name,
          document@content,
          document@citation,
          document@categories,
          document@location,
          document@word_count,
          document@uri
        )
        write.table(
          row,
          file = csv_name,
          sep = ',',
          append = TRUE,
          quote = FALSE,
          col.names = FALSE,
          row.names = FALSE
        )
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
