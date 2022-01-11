build_ngram_df <- function() {
  tibble(
    ngram = character(),
    year = character(),
    count = integer(),
    volume = integer(),
    presidents = character(),
    categories = character(),
    document_uris = character()
  ) %>% 
    as.data.frame()
}

db_connect <- function(path = '../data/ngrams/ngrams.sqlite') {
  DBI::dbConnect(RSQLite::SQLite(), dbname = path)
}

db_create_table <- function(con, table_name, df, overwrite = FALSE) {
  if (dbExistsTable(con, table_name)) {
    dbDisconnect(con)
    return()
  }
  
  dbWriteTable(con, table_name, df, overwrite = overwrite)
}

db_build_ngram_table_name <- function(president_name, year) {
  president_last_name <- tolower(word(president_name, -1))
  paste0(president_last_name, '_', year, '_ngrams')
}

db_update_ngram <- function(con, table_name, ngram_obj) {
  query <- paste('UPDATE', 
                 table_name, 
                 'SET count=:count, volume=:volume, categories=:categories, document_uris=:document_uris, presidents=:presidents WHERE ngram=:ngram AND year=:year;')
  dbSendQuery(con, query, ngram_obj) %>% 
    dbClearResult()
}

db_find_ngram_by_year <- function(con, table_name, ngram, year) {
  tbl(con, table_name) %>% 
    filter(ngram==ngram, year==year) %>% 
    slice_min(1) %>% 
    as_tibble()
  query <- paste('SELECT * FROM',
                 table_name,
                 'WHERE ngram=:ngram AND year=:year')
  dbGetQuery(con, query, tibble(ngram=ngram, year=year))
}

db_insert_ngram <- function(con, table_name, ngram_obj) {
  query <- paste('INSERT INTO', 
                 table_name, 
                 '(ngram, year, count, volume, categories, document_uris, presidents)',
                 'VALUES (:ngram, :year, :count, :volume, :categories, :document_uris, :presidents)')
  dbSendQuery(con, query, ngram_obj) %>% 
    dbClearResult()
}