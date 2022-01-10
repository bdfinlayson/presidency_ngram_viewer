library(fs)
library(testthat)
library(tibble)
library(quanteda)
library(stringr)
source('../helpers/tokenizer_helper.R')

stub_row <- function(
  document_title = 'Special Message', 
  document_date = '1850-04-22T00:00:00+00:00',
  president_name = 'Zachary Taylor',
  text_content = 'This is a # Dr. Bryan   CORPUS   . 123 A 100%   tokenizer-test of the @ tokenizer/code :)!',
  citation = 'NA',
  categories = 'Messages;Written Messages;Presidential;to Congress',
  location = '',
  word_count = 9,
  document_uri = '/documents/special-message-4400') {
  tibble(
    document_title = document_title,
    document_date = document_date,
    president_name = president_name,
    text_content = text_content,
    citation = citation,
    categories = categories,
    location = location,
    word_count = word_count,
    document_uri = document_uri
  )
}

test_that("get_file_paths() returns list of file paths", {
  result <- get_file_paths(dir_path='./data/stubs')
  expect_equal(result, c('./data/stubs/test-corpus.csv'))
})

test_that('get_ngrams() returns list of lowercased ngrams without special characters, stop words or punctuation, and at least three characters', {
  stub <- stub_row()
  result <- get_ngrams(stub)
  expect_equal(result[[1]], c("bryan", "corpus", "tokenizer", "test", "tokenizer", "code", "bryan corpus", "tokenizer test", "tokenizer code"))
})

test_that('get_document_year() returns year of document', {
  stub <- stub_row()
  result <- get_document_year(stub$document_date)
  expect_equal(result, '1850')
})

test_that('merge_values() returns semi-colon separated string of values', {
  sample_1 <- 'test;sample'
  sample_2 <- 'single value with spaces'
  sample_3 <- 'single'
  sample_4 <- ''
  sample_5 <- 'double;trouble'
  
  expect_equal(merge_values(sample_1, sample_2), 'test;sample;single value with spaces')
  expect_equal(merge_values(sample_2, sample_3), 'single value with spaces;single')
  expect_equal(merge_values(sample_3, sample_4), 'single')
  expect_equal(merge_values(sample_3, NULL), 'single')
  expect_equal(merge_values(NULL, NULL), '')
  expect_equal(merge_values(NULL, sample_3), 'single')
  expect_equal(merge_values(sample_4, sample_3), 'single')
  expect_equal(merge_values(sample_1, sample_5), 'test;sample;double;trouble')
})

