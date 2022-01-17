build_ngram_detail_column <-
  function(title = '',
           president_name = '',
           n_times_said = 0,
           n_documents = 0,
           start_year = 1900,
           end_year = 1900,
           top_category = '',
           top_location = '',
           top_doc = '') {
    column(
      3,
      h5(title),
      tags$image(
        src = name_to_url(president_name),
        height = '100px',
        width = '80px'
      ),
      br(),
      em(president_name),
      br(),
      tags$table(
        tags$tr(tags$td(strong('Times said: ')), tags$td(n_times_said)),
        tags$tr(tags$td(strong('Documents: ')), tags$td(n_documents)),
        tags$tr(tags$td(strong('Years said: ')), tags$td(
          str_interp("${start_year} - ${end_year}")
        )),
        tags$tr(tags$td(strong(
          'Most freq. category: '
        )), tags$td(top_category)),
        tags$tr(tags$td(strong(
          'Most freq. location: '
        )), tags$td(if (is.null(top_location)) 'N/A' else top_location)),
        tags$tr(tags$td(strong(
          'Most freq. document: '
        )), tags$td(
          tags$a(
            href = str_interp('https://www.presidency.ucsb.edu/${top_doc}'),
            top_doc %>%
              str_replace_all('-', ' ') %>%
              str_replace_all('/documents/', '') %>%
              str_to_title()
          )
        ))
      )
    )
  }

n_documents <- function(df) {
  length(unique(df$document_uri))
}

top_item <- function(items) {
  items %>%
    str_c() %>%
    str_split(';') %>%
    unlist() %>%
    table() %>%
    as.data.frame() %>%
    arrange(desc(Freq)) %>%
    head(n = 1) %>%
    .$.
}