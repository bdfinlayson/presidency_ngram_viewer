library(dplyr)
library(dbplyr)
library(stringr)
source('./helpers/database_helper.R')

con <- db_connect(path = './data/ngrams.sqlite')

ngrams_df <- tbl(con, 'all_ngrams') %>%
  as.data.frame()

dbDisconnect(con = con)
