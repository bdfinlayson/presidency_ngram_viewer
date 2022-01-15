library(dplyr)
library(dbplyr)
source('./helpers/database_helper.R')

con <- db_connect(path = './data/ngrams.sqlite')
ngrams_df <- tbl(con, 'all_ngrams') %>%
    as.data.frame()

presidents <- db_list_presidents(con, 'all_ngrams')

print(presidents$president)



dbDisconnect(con = con)