library(fs)
library(readr)
library(foreach)
library(iterators)
library(quanteda)
library(tibble)
library(dplyr)
library(stringr)
library(DBI)
library(RSQLite)
library(dbplyr)
source('./helpers/tokenizer_helper.R')
source('./helpers/database_helper.R')

# get presidential corpus data file paths
file_paths <- dir_ls(path = '../data/presidents_scraped')

# test_file <- '../data/presidents_scraped/zachary-taylor.csv'
# test_file_2 <- '../data/presidents_scraped/james-garfield.csv'
# test_file_paths <- c(test_file, test_file_2)

# initialize db if not already created
db_connect() %>% 
  db_create_table('ngrams', build_ngram_df())

# iterate through each token result.
# If the token already exists in the text_collocations_set:
# - add to the 'count' value using the 'count' value from the ngram obj, and increment the 'volume' count by 1
# - otherwise append the new ngram data including corpus's: president name, year, categories, word count
update_or_create_ngram <- function(ng, source_df_row) {
  document_year <- get_document_year(source_df_row$document_date)
  con <- db_connect()
  existing_ngram <- db_find_ngram_by_year(con, 'ngrams', ng, document_year)
  has_ngram <- nrow(existing_ngram) > 0
  if (has_ngram) {
    print(
      c(
        'UPDATING ngram',
        ng,
        'for year',
        document_year,
        'for presidents',
        existing_ngram$presidents
      )
    )
    # ensure volume indicator only gets incremented when new document is detected
    is_new_document <-
      str_detect(existing_ngram$document_uris,
                 source_df_row$document_uri)
    target_df <- db_update_ngram(
      con,
      'ngrams',
      tibble(
        volume = if (is_new_document)
          existing_ngram$volume + 1
        else
          existing_ngram$volume,
        ngram = existing_ngram$ngram,
        year = existing_ngram$year,
        count = existing_ngram$count + 1,
        categories = merge_values(existing_ngram$categories, source_df_row$categories),
        presidents = merge_values(existing_ngram$presidents, source_df_row$president_name),
        document_uris = merge_values(
          existing_ngram$document_uris,
          source_df_row$document_uri
        )
      )
    )
  } else {
    print(
      c(
        'ADDING ngram',
        ng,
        'for year',
        document_year,
        'for president',
        source_df_row$president_name
      )
    )
    target_df <- db_insert_ngram(
      con,
      'ngrams',
      tibble(
        ngram = ng,
        year = document_year,
        count = 1,
        volume = 1,
        presidents = source_df_row$president_name,
        categories = source_df_row$categories,
        document_uris = source_df_row$document_uri
      )
    )
  }
  dbDisconnect(con)
}

collect_and_process_ngrams <-
  function(file_path, corpus_df) {
    foreach(row = iter(corpus_df, by = 'row')) %do% {
      ngrams <- get_ngrams(row)
      for (ngram in ngrams[[1]]) {
        update_or_create_ngram(ngram, row)
      }
    }
  }


# start the tokenizer
for (file_path in file_paths) {
  corpus_df <- read_csv(file_path) %>%
    arrange(document_date)
  collect_and_process_ngrams(path, corpus_df)
}