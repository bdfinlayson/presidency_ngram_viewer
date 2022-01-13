library(fs)
library(readr)
library(foreach)
library(iterators)
library(quanteda)
library(tibble)
library(stringr)
library(DBI)
library(RSQLite)
library(dbplyr)
library(timeDate)
source('./helpers/tokenizer_helper.R')
source('./helpers/database_helper.R')

collect_and_process_ngrams_v2 <-
  function(corpus_df, con, file_path) {
    president_name <- corpus_df$president_name %>% unique()
    years <- get_unique_years(corpus_df)
    for (year in years) {
      table_name <- db_build_ngram_table_name(president_name, year)
      if (dbExistsTable(con, table_name)) {
        print(c('TABLE EXISTS', table_name))
        next
      }
      print(c('FETCHING ngrams for', president_name, 'for year', year))
      ngrams_total <- c('')
      for (month in 1:12) {
        text <- aggregate_text_for_year_and_month(corpus_df, year, as.character(month))
        if (is.na(text) || nchar(text) == 0) {
          next
        }
        ngrams <- get_ngrams(text, n = 1:5)
        ngrams_total <- c(ngrams_total[[1]], ngrams[[1]])
      }
      ngram_frequencies <- as.data.frame(table(ngrams_total))
      ngram_frequencies$year <- year
      ngram_frequencies$president <- president_name
      ngram_frequencies <- ngram_frequencies %>% mutate(ngram_length = as.integer(str_count(ngrams_total, "\\w+")))    
      db_create_table(con, table_name, ngram_frequencies, overwrite = TRUE)
    }
  }

# get presidential corpus data file paths
con <- db_connect()
file_paths <- dir_ls(path = '../data/presidents_scraped')

# initialize db if not already created
# db_connect() %>%
#  db_create_table('ngrams', build_ngram_df())

# start the tokenizer
count <- 1
for (file_path in file_paths) {
  read_csv(file_path, lazy = TRUE) %>%
    arrange(document_date) %>%
    collect_and_process_ngrams_v2(con, path)
  print(c('PROCESSED', count, 'of', length(file_paths), 'files'))
  count <- count + 1
}

print("TOKENIZATION DONE")
dbDisconnect(con)