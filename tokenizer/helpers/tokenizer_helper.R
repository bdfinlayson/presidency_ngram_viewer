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

get_unique_years <- function(corpus_df) {
  corpus_df$document_date %>% substr(1,4) %>% unique()
}

aggregate_text_for_year_and_month <- function(corpus_df, year, month) {
  start <- as.POSIXct(strptime(paste0(year, "-", month, "-01 00:00:00"), "%Y-%m-%d %H:%M:%S"))
  end <- timeLastDayInMonth(start)@Data
  text <- corpus_df %>% 
    filter(between(document_date, start, end)) %>% 
    pull(text_content) 
  text %>% 
    str_flatten()
}