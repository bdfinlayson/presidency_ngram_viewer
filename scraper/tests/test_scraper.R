library(testthat)
library(stringr)
source('../helpers/scraper_helper.R')
source('../models/document.R')

test_that("list_presidents() returns list of uris to president pages", {
  html <- get_html(base_uri = './stubs', path = 'presidents.html', sep = '/')
  result <- list_presidents(html)
  expect_length(result, 46)
  expect_equal(result[1], '/people/president/joseph-r-biden')
  expect_equal(result[46], '/people/president/george-washington')
})

test_that("list_documents() returns list of links to presidential documents when valid", {
  html <- get_html(base_uri = './stubs', path = 'related_documents_valid.html', sep = '/')
  result <- list_documents(html)
  expect_length(result, 35)
  expect_equal(result[1], '/documents/press-release-senator-joe-biden-announces-campaign-for-president-the-united-states')
  expect_equal(result[35], '/documents/statement-senator-joe-biden-defense-departments-decision-extend-tours-duty-iraq') 
})

test_that("list_documents() returns an empty list when invalid", {
  html <- get_html(base_uri = './stubs', path = 'related_documents_invalid.html', sep = '/')
  result <- list_documents(html)
  expect_length(result, 0)
})

test_that("get_document_title() returns presidential document title", {
  html <- get_html(base_uri = './stubs', path = 'document_biden.html', sep = '/')
  result <- get_document_title(html)
  expect_equal(result, 'Remarks by President-elect Joe Biden in Wilmington Delaware')
})

test_that("get_document_date() returns presidential document date", {
  html <- get_html(base_uri = './stubs', path = 'document_biden.html', sep = '/')
  result <- get_document_date(html)
  expect_equal(result, '2020-12-11T00:00:00+00:00')
})

test_that("get_document_president_name() returns presidential document president name", {
  html <- get_html(base_uri = './stubs', path = 'document_biden.html', sep = '/')
  result <- get_document_president_name(html)
  expect_equal(result, 'Joseph R. Biden')
})

test_that("get_document_content() returns presidential document content", {
  html <- get_html(base_uri = './stubs', path = 'document_biden.html', sep = '/')
  result <- get_document_content(html)
  expect_equal(str_count(result, "[[:alpha:]]+"), 2311)
})

test_that("get_document_citation() returns presidential document citation", {
  html <- get_html(base_uri = './stubs', path = 'document_biden.html', sep = '/')
  result <- get_document_citation(html)
  expect_equal(str_count(result, "[[:alpha:]]+"), 30)
})

test_that("get_categories() returns document categories", {
  html <- get_html(base_uri = './stubs', path = 'document_biden_multi_category.html', sep = '/')
  result <- get_categories(html)
  expect_equal(result, 'Press Office;Miscellaneous Press Secretary;Press Releases')
})

test_that("get_categories() returns empty string when no document categories present", {
  html <- get_html(base_uri = './stubs', path = 'document_biden_no_categories.html', sep = '/')
  result <- get_categories(html)
  expect_equal(result, '')
})

test_that("get_location() returns document location", {
  html <- get_html(base_uri = './stubs', path = 'document_biden_location.html', sep = '/')
  result <- get_location(html)
  expect_equal(result, 'Washington DC')
})

test_that("get_location() returns empty string when no document location present", {
  html <- get_html(base_uri = './stubs', path = 'document_biden_location_na.html', sep = '/')
  result <- get_location(html)
  expect_equal(result, '')
})

test_that("build_document() returns a document data object", {
  uri = 'document_biden.html'
  html <- get_html(base_uri = './stubs', path = uri, sep = '/')
  document <- build_document(html, uri)
  expect_equal(document@title, 'Remarks by President-elect Joe Biden in Wilmington Delaware')
  expect_equal(document@date, '2020-12-11T00:00:00+00:00')
  expect_equal(document@president_name, 'Joseph R. Biden')
  expect_equal(str_count(document@content, "[[:alpha:]]+"), 2311)
  expect_equal(str_count(document@citation, "[[:alpha:]]+"), 30)
  expect_equal(document@categories, 'Elections and Transitions;President-elect;Spoken Addresses and Remarks;Transition Documents')
  expect_equal(document@location, 'Delaware')
  expect_equal(document@word_count, 2311)
  expect_equal(document@uri, uri)
})
