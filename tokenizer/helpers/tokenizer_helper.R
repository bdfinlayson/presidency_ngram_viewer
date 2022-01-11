get_file_paths <- function(dir_path = '') {
  dir_ls(path = dir_path)
}

get_ngrams <- function(text_content, n = 2) {
  tokens(text_content, 
         remove_numbers = TRUE, 
         remove_symbols = TRUE, 
         remove_punct = TRUE, 
         split_hyphens = TRUE) %>%
    tokens_remove(stopwords('en'), 
                  padding = TRUE, 
                  case_insensitive = TRUE,
                  min_nchar=3) %>%
    tokens_tolower() %>% 
    tokens_ngrams(n = n, concatenator = ' ') # google ngrams evaluates 1-5 ngrams
}

get_document_year <- function(date_string) {
  as.POSIXct(date_string, format = "%Y-%m-%d") %>%
    format(format = "%Y")
}

get_unique_years <- function(corpus_df) {
  corpus_df$document_date %>% substr(1,4) %>% unique()
}

aggregate_text_for_year_and_month <- function(corpus_df, year, month) {
  datetime <- paste0(year, "-", month, "-01 00:00:00")
  corpus_df %>% 
    filter(document_date >= as.POSIXct(strptime(datetime, "%Y-%m-%d %H:%M:%S"))) %>% 
    pull(text_content) %>% 
    str_flatten()
}

merge_values <- function(existing_values, new_values) {
  if (!is.character(existing_values) && !is.character(new_values)) {
    return('');
  }
  if (is.character(existing_values) && !is.character(new_values)) {
    return(existing_values)
  }
  if (!is.character(existing_values) && is.character(new_values)) {
    return(new_values)
  }
  if (nchar(new_values) == 0) {
    return(existing_values)
  }
  if (nchar(existing_values) == 0) {
    return(new_values)
  }
  existing_values_list <-
    str_split(existing_values, pattern = ';')[[1]]
  new_values_list <- str_split(new_values, pattern = ';')[[1]]
  union(existing_values_list, new_values_list) %>%
    str_flatten(';')
}