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

# iterate through each token result.
# If the token already exists in the text_collocations_set:
# - add to the 'count' value using the 'count' value from the ngram obj, and increment the 'volume' count by 1
# - otherwise append the new ngram data including corpus's: president name, year, categories, word count
update_or_create_ngram <-
  function(con, ng, ng_frequency, source_df_row) {
    document_year <- get_document_year(source_df_row$document_date)
    existing_ngram <-
      db_find_ngram_by_year(con, 'ngrams', ng, document_year)
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
          count = existing_ngram$count + ng_frequency,
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
          count = ng_frequency,
          volume = 1,
          presidents = source_df_row$president_name,
          categories = source_df_row$categories,
          document_uris = source_df_row$document_uri
        )
      )
    }
  }

collect_and_process_ngrams <-
  function(corpus_df, con, file_path) {
    foreach(row = iter(corpus_df, by = 'row')) %do% {
      ngrams <- get_ngrams(row)
      ngram_frequencies <- as.data.frame(table(ngrams[[1]]))
      foreach(ngf = iter(ngram_frequencies, by = 'row')) %do% {
        update_or_create_ngram(con, as.character(ngf$Var1), ngf$Freq, row)
      }
    }
  }

collect_and_process_ngrams_v2 <-
  function(corpus_df, con, file_path) {
    president_name <- corpus_df$president_name %>% unique()
    years <- get_unique_years(corpus_df)
    for (year in years) {
      print(c('FETCHING ngrams for', president_name, 'for year', year))
      ngrams_total <- c()
      for (month in 1:12) {
        ngrams <- aggregate_text_for_year_and_month(corpus_df, year, as.character(month)) %>% 
          get_ngrams(n = 1:5)
        ngrams_total <- c(ngrams_total[[1]], ngrams[[1]])
      }
      ngram_frequencies <- as.data.frame(table(ngrams_total))
      ngram_frequencies$year <- year
      ngram_frequencies$president <- president_name
      ngram_frequencies <- ngram_frequencies %>% mutate(ngram_length = as.integer(str_count(ngrams_total, "\\w+")))    
      table_name <- db_build_ngram_table_name(president_name, year)
      db_create_table(con, table_name, ngram_frequencies, overwrite = TRUE)
    }
  }
# test_file <- '../data/presidents_scraped/zachary-taylor.csv'
# test_file_2 <- '../data/presidents_scraped/james-garfield.csv'
# test_file_paths <- c(test_file, test_file_2)

# get presidential corpus data file paths
con <- db_connect()
file_paths <- dir_ls(path = '../data/presidents_scraped')

# initialize db if not already created
# db_connect() %>%
#  db_create_table('ngrams', build_ngram_df())

# start the tokenizer
for (file_path in file_paths) {
  read_csv(file_path, lazy = TRUE) %>%
    arrange(document_date) %>%
    collect_and_process_ngrams_v2(con, path)
}
dbDisconnect(con)