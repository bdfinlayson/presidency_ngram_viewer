library(fs)
library(readr)
library(foreach)
library(iterators)
library(quanteda.textstats)
library(tibble)
library(dplyr)

file_paths <- dir_ls(path = '../data/presidents_scraped')

test_file <- '../data/presidents_scraped/zachary-taylor.csv'

df <- read_csv(test_file)

setClass('text_collocations_set', slots=list(
  ngram='character',
  year='numeric',
  count='numeric',
  volume='numeric',
  president_name='character',
  categories='character'
))


# initialize new text_collocations_set obj which will be used to track totals across an individual president's corpuses
collocations_df <- tibble(ngram=character(),
                        year=numeric(),
                        count=numeric(),
                        volume=numeric(),
                        president_name=character(),
                        categories=character())

# iterate through each row of the df and tokenize the text content
get_collocations <- function(row) {
  tokens(row$text_content) %>% 
    tokens_remove(stopwords('en'), padding = TRUE) %>% 
    textstat_collocations()
}

# iterate through each token result. 
# If the token already exists in the text_collocations_set:
# - add to the 'count' value using the 'count' value from text_collocations, and increment the 'volume' count by 1
# - otherwise append the new collocation data including corpus's: president name, year, categories, word count
update_or_create_collocation <- function(collocation_row, cols_df, df_row) {
  existing_collocation <- filter(cols_df, ngram == collocation_row$collocation)
  has_collocation <- nrow(existing_collocation) > 0
  if(has_collocation) {
    print('has collocation')
    cols_df <- rows_update(cols_df, tibble(
      volume = existing_collocation$volume + 1,
      ngram = existing_collocation$ngram, 
      count = existing_collocation$count + collocation_row$count,
      categories = existing_collocation$categories), # TODO: add new categories to existing list
      by = 'ngram')
    print(cols_df %>% filter(ngram == existing_collocation$ngram))
  } else {
    print('does not have collocation')
    cols_df <- rows_insert(cols_df, tibble(
      ngram = collocation_row$collocation, 
      year = 2022, # TODO: extract real year from string using df_row
      count = collocation_row$count, 
      volume = 1,
      president_name = df_row$president_name,
      categories = df_row$categories))
  }
  return(cols_df)
}


foreach(row=iter(df, by='row')) %do% {
   collocations <- get_collocations(row)
   foreach(collocation_row=iter(collocations, by='row')) %do% {
     collocations_df <- update_or_create_collocation(collocation_row, collocations_df, row)
   }
}

head(collocations_df)





