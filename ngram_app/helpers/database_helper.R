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

db_find_all_ngram_corpuses <-
  function(con, ngram, president) {
    query <-
      str_interp(
        "select document_title, document_date, categories, document_uri, word_count, location 
        from all_corpuses 
        where president_name == '${president}' 
        and text_content like '%${ngram}%' 
        order by document_date desc"
      )
    
    dbGetQuery(con, query)
  }