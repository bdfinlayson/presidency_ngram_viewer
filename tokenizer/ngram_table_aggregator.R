library(DBI)
library(RSQLite)
library(dbplyr)
source('./helpers/database_helper.R')

con <- db_connect()
table_names <- db_list_tables(con)

new_df <- data.frame(
    ngrams_total = character(),
    Freq = integer(),
    year = integer(),
    president = character(),
    ngram_length = integer(),
    total_ngrams_for_year = integer()
  )

for (table_name in table_names) {
  table <- tbl(con, table_name) %>% 
    as.data.frame() %>% 
    mutate(year = as.integer(year)) %>% 
    mutate(total_ngrams_for_year = sum(Freq))
  new_df <- bind_rows(new_df, table)
  print(c("APPENDED Table", table_name))
  print(c("TOTAL Rows", nrow(new_df)))
}

new_df <- new_df %>% 
  rename(
    ngram = ngrams_total,
    freq = Freq
  )

print(c('CREATING NEW TABLE WITH', nrow(new_df), 'rows'))

db_create_table(con, 'all_ngrams', new_df, overwrite = TRUE)

print('TABLE CREATED')