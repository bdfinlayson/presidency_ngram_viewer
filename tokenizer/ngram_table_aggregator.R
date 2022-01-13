library(DBI)
library(RSQLite)
library(dbplyr)
source('./helpers/database_helper.R')

con <- db_connect()
table_names <- db_list_tables(con)

new_df <- data.frame(
    ngrams_total = character(),
    Freq = integer(),
    year = character(),
    president = character(),
    ngram_length = integer()
  )

for (table_name in table_names) {
  table <- tbl(con, table_name)
  new_df <- bind_rows(new_df, as.data.frame(table))
  print(c("APPENDED Table", table_name))
  print(c("TOTAL Rows", nrow(new_df)))
}

new_df <- new_df %>% 
  rename(
    ngram = ngrams_total,
    freq = Freq
  )

print(c('CREATING NEW TABLE WITH', nrow(new_df), 'rows'))

db_create_table(con, 'all_ngrams', new_df)

print('TABLE CREATED')