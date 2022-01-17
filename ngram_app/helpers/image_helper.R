name_to_url <- function(name) {
  name_uri <- name %>%
    str_replace_all('\\W', ' ') %>% 
    str_squish() %>% 
    str_replace_all(' ', '-') %>% 
    tolower() 
  
  url <- str_interp('https://www.presidency.ucsb.edu/sites/default/files/styles/large/public/people/${name_uri}.jpg')
  
  if (name == 'Ronald Reagan') {
    url <- 'https://www.presidency.ucsb.edu/sites/default/files/styles/large/public/election-maps/1980.1/40.jpg'
  }
  
  if (name == 'Jimmy Carter') {
    url <- 'https://www.presidency.ucsb.edu/sites/default/files/styles/large/public/election-maps/1980.1/39.jpg'
  }
  
  if (name == 'John F. Kennedy') {
    url <- 'https://www.presidency.ucsb.edu/sites/default/files/styles/large/public/people/john-f-kennedy_2.jpg'
  }
  
  return(url)
}
