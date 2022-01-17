name_to_url <- function(name) {
  name_uri <- name %>%
    str_replace_all('\\W', ' ') %>% 
    str_squish() %>% 
    str_replace_all(' ', '-') %>% 
    tolower() 
  url <- str_interp('https://www.presidency.ucsb.edu/sites/default/files/styles/large/public/people/${name_uri}.jpg')
  return(url)
}
