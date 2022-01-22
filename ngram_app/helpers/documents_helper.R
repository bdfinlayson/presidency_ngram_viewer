build_document <-
  function(title, date, uri, word_count, snippet) {
    tags$div(
      strong(tags$a(title,
        href = str_interp('https://www.presidency.ucsb.edu/${uri}'))),
      em(str_interp('Date: ${date} | Word Count: ${word_count}')),
      HTML(snippet)
    )
  }

build_documents <- function(documents) {
    elements <- list()
    
    index = 1
    for (d in documents[[1]]) {
      elements[index] = build_document(d$document_title, d$document_date, d$document_uri, d$word_count, d$snippet) %>% .$children
    }
    
    return(elements)
}