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

db_find_all_ngram_corpuses_by_president <-
  function(con, ngram, president) {
    query <-
      str_interp(
        "select document_title, document_date, categories, document_uri, word_count, location 
        from all_corpuses 
        where president_name == '${president}' 
        and LOWER(text_content) like '%${escape_quotes(ngram)}%' COLLATE NOCASE 
        order by document_date desc"
      )
    
    dbGetQuery(con, query)
  }

db_find_all_ngram_corpuses <-
  function(con, ngram) {
    query <-
      str_interp(
        "select document_title, document_date, categories, document_uri, word_count, location 
        from all_corpuses 
        where text_content like '%${escape_quotes(ngram)}%' COLLATE NOCASE 
        order by document_date desc"
      )
    
    dbGetQuery(con, query)
  }

db_get_total_said <-
  function(con, ngram) {
    query <-
      str_interp(
        "SELECT sum((LENGTH(text_content) - LENGTH(REPLACE(text_content, '${escape_quotes(ngram)}', '')))
        / LENGTH('${escape_quotes(ngram)}')) as total_said
        from all_corpuses NOCASE"
      )
    
    dbGetQuery(con, query)
  }

db_get_total_said_by_president <-
  function(con, ngram, president) {
    query <-
      str_interp(
        "SELECT sum((LENGTH(text_content) - LENGTH(REPLACE(text_content, '${escape_quotes(ngram)}', '')))
        / LENGTH('${escape_quotes(ngram)}')) as total_said
        from all_corpuses
        where president_name == '${president}' COLLATE NOCASE"
      )
    
    dbGetQuery(con, query)
  }


escape_quotes <- function(x) { str_replace_all(x, "'", "''")}