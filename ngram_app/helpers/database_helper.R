db_connect <- function(path = '../data/ngrams/ngrams.sqlite') {
  DBI::dbConnect(RSQLite::SQLite(), dbname = path)
}

db_list_presidents <- function(con, table_name) {
  query <- paste('select distinct president from', table_name, 'order by president asc;')
  
  dbGetQuery(con, query)
}