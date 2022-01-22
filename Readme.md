# Presidential Documents Ngram Viewer

## Purpose
To discover word usage frequencies by US presidents over time across 118,561 official documents and transcripts. Answers questions such as:
- What does word usage by US presidents look like over time?
- Who said it first? Last? Most?
- Where was it said?
- When was it said?
- Can I preview the documents it was said in?

## Data
The primary data source was The American Presidency Project at https://www.presidency.ucsb.edu/. I wrote a web scraper in R to capture all 118,561 official documents and associated metadata including:
- Date
- Location
- Categories
- President
- Citation
- Document uri
- Word count

Corpus data totaled 117,374,146 words, which was then tokenized using Quanteda to produce a SQLite database of 7,069,561 n-grams. This project gathered n-grams 1:5, meaning single words up to 5 word pairs. 

The corpus data was further optimized for full text search by leveraging Sqlite’s FST4 extension. With FST4, it was also possible to extract snippets from the corpus data (pictured below).

## Demo
Live version available at: https://bryanfinlayson.shinyapps.io/presidential_ngram_search/

## Screenshots

![image](https://user-images.githubusercontent.com/7499749/150654490-c1277962-9825-4f0d-8a6c-3d3d03795550.png)

![image](https://user-images.githubusercontent.com/7499749/150654561-2fa07297-d026-4e99-8ad6-0276884bd4fc.png)
