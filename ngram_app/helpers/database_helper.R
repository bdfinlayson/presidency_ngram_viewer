db_connect <- function(path = '../data/ngrams/ngrams.sqlite') {
  DBI::dbConnect(RSQLite::SQLite(), dbname = path)
}

db_list_presidents <- function(con, table_name) {
  query <-
    paste('select distinct president from',
          table_name,
          'order by president asc;')
  
  dbGetQuery(con, query)
}

db_select_first_corpus_with_ngram <-
  function(con, table_name, ngram, order = 'desc') {
    query <- str_interp("select * from ${table_name}
where text_content like '%${ngram}%'
and categories not like '%fact sheet%'
order by document_date ${order}
limit 1;")
    
    dbGetQuery(con, query)
  }