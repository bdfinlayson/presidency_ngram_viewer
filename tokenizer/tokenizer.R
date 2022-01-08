library(fs)
library(readr)
library(foreach)
library(iterators)
library(quanteda)
library(tibble)
library(dplyr)
library(stringr)

file_paths <- dir_ls(path = '../data/presidents_scraped')

test_file <- '../data/presidents_scraped/zachary-taylor.csv'

corpus_df <- read_csv(test_file)

setClass(
  'text_collocations_set',
  slots = list(
    ngram = 'character',
    year = 'numeric',
    count = 'numeric',
    volume = 'numeric',
    president_name = 'character',
    categories = 'character'
  )
)


# initialize new text_collocations_set obj which will be used to track totals across an individual president's corpuses
ngrams_df <- tibble(
  ngram = character(),
  year = character(),
  count = numeric(),
  volume = numeric(),
  presidents = character(),
  categories = character(),
  document_uris = character()
)

# iterate through each row of the df and tokenize the text content
get_ngrams <- function(row) {
  tokens(row$text_content, remove_punct = TRUE) %>%
    tokens_remove(stopwords('en'), padding = TRUE) %>%
    # textstat_collocations()
    tokens_ngrams(n = 1:4, concatenator = ' ') # google ngrams evaluates 1-5 ngrams
}

get_document_year <- function(date_string) {
  as.POSIXct(date_string, format = "%Y-%m-%d") %>%
    format(format = "%Y")
}

merge_values <- function(existing_values, new_values) {
  existing_values_list <- str_split(existing_values, pattern = ';')
  new_values_list <- str_split(new_values, pattern = ';')
  merged_values <-
    union(existing_values_list, new_values_list)[[1]] %>%
    str_flatten(';')
  return(merged_values)
}

# iterate through each token result.
# If the token already exists in the text_collocations_set:
# - add to the 'count' value using the 'count' value from text_collocations, and increment the 'volume' count by 1
# - otherwise append the new collocation data including corpus's: president name, year, categories, word count
update_or_create_ngram <- function(ng, target_df, source_df_row) {
  document_year <- get_document_year(source_df_row$document_date)
  existing_ngram <-
    filter(target_df, ngram == ng, year == document_year)
  has_ngram <- nrow(existing_ngram) > 0
  if (has_ngram) {
    print(c('updating ngram', ng))
    # ensure volume indicator only gets incremented when new document is detected
    is_new_document <- str_detect(existing_ngram$document_uris, source_df_row$document_uri)
    target_df <- rows_update(
      target_df,
      tibble(
        volume = if(is_new_document) existing_ngram$volume + 1 else existing_ngram$volume,
        ngram = existing_ngram$ngram,
        count = existing_ngram$count + 1,
        categories = merge_values(existing_ngram$categories, source_df_row$categories),
        presidents = merge_values(existing_ngram$presidents, source_df_row$president_name),
        document_uris = merge_values(existing_ngram$document_uris, source_df_row$document_uri)
      ),
      by = 'ngram'
    )
  } else {
    print(c('adding ngram', ng))
    target_df <- rows_insert(
      target_df,
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
  return(target_df)
}

normalize <- function(ngram) {
  ngram %>%
    tolower() %>%
    str_replace_all('[^A-Za-z0-9]', ' ') %>%
    str_trim() %>%
    str_squish()
}

foreach(row = iter(corpus_df, by = 'row')) %do% {
  ngrams <- get_ngrams(row)
  for (ngram in ngrams[[1]]) {
    # skip ngram if ngram length is one character
    if (nchar(ngram) == 1) {
      print(c("ngram too short, skipping...", ngram))
      next
    }
    # normalize ngram by lowercasing, removing special characters and extra whitespace
    normalized_ngram <- normalize(ngram)
    ngrams_df <-
      update_or_create_ngram(normalized_ngram, ngrams_df, row)
  }
}

head(ngrams_df)
