library(fs)
library(DBI)
library(RSQLite)
library(dbplyr)
library(dplyr)
library(readr)
source('./helpers/database_helper.R')

con <- db_connect()
file_paths <- dir_ls(path = '../data/presidents_scraped')

main_df <- data.frame(
  document_title = character(),
  document_date = character(),
  president_name = character(),
  text_content = character(),
  citation = character(),
  categories = character(),
  location = character(),
  word_count = integer(),
  document_uri = character()
)

count <- 1
for (file_path in file_paths) {
  new_df <- read_csv(file_path, lazy = TRUE, col_types = list(col_character(), col_character())) %>%
    arrange(document_date) %>%
    as.data.frame()
  
  main_df <- bind_rows(main_df, new_df)
  print(c("APPENDED", file_path, 'with', nrow(new_df), 'rows'))
  print(c("TOTAL Rows", nrow(main_df)))
  print(c('PROCESSED', count, 'of', length(file_paths), 'files'))
  count <- count + 1
}

db_create_table(con, 'all_corpuses', main_df, overwrite = TRUE)
print('TABLE CREATED')
print("CORPUS TRANSFER TO SQL DONE")
dbDisconnect(con)