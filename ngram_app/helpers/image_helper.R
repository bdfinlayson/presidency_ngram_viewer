name_to_urn <- function(name) {
  name %>%
    str_replace_all('\\W', ' ') %>% 
    str_squish() %>% 
    str_replace_all(' ', '-') %>% 
    tolower()
}
