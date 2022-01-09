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
test_file_2 <- '../data/presidents_scraped/james-garfield.csv'
test_files <- c(test_file, test_file_2)

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
build_ngram_df <- function() {
  tibble(
    ngram = character(),
    year = character(),
    count = numeric(),
    volume = numeric(),
    presidents = character(),
    categories = character(),
    document_uris = character()
  )
}


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
  existing_values_list <-
    str_split(existing_values, pattern = ';')[[1]]
  new_values_list <- str_split(new_values, pattern = ';')[[1]]
  merged_values <-
    union(existing_values_list, new_values_list) %>%
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
    target_df <- rows_update(
      target_df,
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
      ),
      by = c('ngram', 'year')
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
      ),
      by = c('ngram', 'year')
    )
  }
  return(target_df)
}

normalize <- function(ngram) {
  ngram %>%
    tolower() %>%
    str_replace_all("[^A-Za-z0-9']", ' ') %>%
    str_trim() %>%
    str_squish()
}

itemize_ngrams_for_presidential_documents <-
  function(file_path, corpus_df, ngram_df) {
    foreach(row = iter(corpus_df, by = 'row')) %do% {
      ngrams <- get_ngrams(row)
      for (ngram in ngrams[[1]]) {
        # skip ngram if ngram length is less than three characters
        if (nchar(ngram) < 3) {
          print(c("ngram too short, skipping...", ngram))
          next
        }
        # normalize ngram by lowercasing, removing special characters and extra whitespace
        normalized_ngram <- normalize(ngram)
        ngram_df <-
          update_or_create_ngram(normalized_ngram, ngram_df, row)
      }
    }
    return(ngram_df)
  }

ngram_df <- build_ngram_df()

for (file_path in test_files) {
  corpus_df <- read_csv(file_path) %>%
    arrange(document_date)
  ngram_df <-
    itemize_ngrams_for_presidential_documents(path, corpus_df, ngram_df)
  write_csv(ngram_df, '../data/presidents_ngram/ngrams.csv')
}