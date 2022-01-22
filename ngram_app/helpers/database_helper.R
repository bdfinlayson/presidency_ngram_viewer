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
        "SELECT DISTINCT document_title, document_date, categories, document_uri, word_count, location 
        from fts4_all_corpuses 
        WHERE president_name == '${president}' 
        AND text_content MATCH '\"${escape_quotes(ngram)}\"'
        ORDER BY document_date DESC"
      )
    
    dbGetQuery(con, query)
  }

db_find_all_ngram_corpuses <-
  function(con, ngram) {
    query <-
      str_interp(
        "SELECT DISTINCT document_title, document_date, categories, document_uri, word_count, location
        FROM fts4_all_corpuses 
        WHERE text_content MATCH '\"${escape_quotes(ngram)}\"'
        ORDER BY document_date DESC"
      )
    
    dbGetQuery(con, query)
  }

db_count_all_ngram_corpuses <-
  function(con, ngram) {
    query <-
      str_interp(
        "SELECT DISTINCT count(document_uri) as total_documents
        FROM fts4_all_corpuses 
        WHERE text_content MATCH '\"${escape_quotes(ngram)}\"'"
      )
    
    dbGetQuery(con, query)
  }

db_find_top_documents_by_president <- function(con, ngram, president) {
   query <-
    str_interp(
      "SELECT president_name, document_title, document_date, document_uri, word_count, snippet(fts4_all_corpuses, '<b style=\"background-color: yellow\"}>', '</b>', '<p>...</p>', -1, -64) as snippet
      FROM fts4_all_corpuses
      WHERE president_name = '${president}' 
      AND text_content MATCH '\"${escape_quotes(ngram)}\"'
      ORDER BY word_count DESC
      LIMIT 3"
    )
  
  dbGetQuery(con, query) 
}

# not currently used
db_find_top_category <- function(con, ngram) {
  query <-
    str_interp(
      "SELECT categories as category, COUNT(categories) as count
        FROM fts4_all_corpuses
        WHERE text_content MATCH '${escape_quotes(ngram)}'
        GROUP BY categories
        ORDER BY count desc Limit 1"
    )
  
  dbGetQuery(con, query)
}

db_find_top_category_by_president <- function(con, ngram, president) {
  query <-
    str_interp(
      "SELECT categories as category, COUNT(categories) as count
        FROM fts4_all_corpuses
        WHERE president_name = '${president}'
        AND text_content MATCH '${escape_quotes(ngram)}'
        GROUP BY categories
        ORDER BY count desc Limit 1"
    )
  
  dbGetQuery(con, query)
}

db_find_top_location <- function(con, ngram) {
  query <-
    str_interp(
      "SELECT location, COUNT(location) as count
        FROM fts4_all_corpuses
        WHERE text_content MATCH '${escape_quotes(ngram)}'
        GROUP BY location
        ORDER BY count desc Limit 1"
    )
  
  dbGetQuery(con, query)
}

db_find_top_location_by_president <- function(con, ngram, president) {
  query <-
    str_interp(
      "SELECT location, COUNT(location) as count
        FROM fts4_all_corpuses
        WHERE president_name = '${president}'
        AND text_content MATCH '${escape_quotes(ngram)}'
        GROUP BY location
        ORDER BY count desc Limit 1"
    )
  
  dbGetQuery(con, query)
}
# obsolete
db_get_total_said <-
  function(con, ngram) {
    query <-
      str_interp(
        "SELECT sum((LENGTH(text_content) - LENGTH(REPLACE(LOWER(text_content), ' ${escape_quotes(ngram)} ', '')))
        / LENGTH(' ${escape_quotes(ngram)} ')) as total_said
        from all_corpuses NOCASE"
      )
    
    dbGetQuery(con, query)
  }

# obsolete
db_get_total_said_by_president <-
  function(con, ngram, president) {
    query <-
      str_interp(
        "SELECT sum((LENGTH(text_content) - LENGTH(REPLACE(LOWER(text_content), ' ${escape_quotes(ngram)} ', '')))
        / LENGTH(' ${escape_quotes(ngram)} ')) as total_said
        from all_corpuses
        where president_name == '${president}' COLLATE NOCASE"
      )
    
    dbGetQuery(con, query)
  }


escape_quotes <- function(x) { str_replace_all(x, "'", "''")}