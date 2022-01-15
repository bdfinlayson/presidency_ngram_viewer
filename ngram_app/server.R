#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(stringr)
library(ggplot2)
library(dplyr)
library(dbplyr)
source('./helpers/plot_helper.R')

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    observe({
        search_terms <- input$ngram_search %>%
            str_squish() %>%
            tolower() %>%
            str_split(';')
        
        print(search_terms)
    })
    
    observe({
        search_terms <- input$ngram_search %>%
            str_squish() %>%
            str_replace_all('; ', ';') %>%
            tolower() %>%
            str_split(';')
        
        if (search_terms[[1]] == '') {
            text = paste(
                "\n   Please enter a search term in the input field above.\n",
                "       Separate each search term with a semicolon.\n",
                "       Usage frequency of the term will then be displayed."
            )
            output$ngram_line_chart <- build_empty_plot(text)
        }
        else {
            output$ngram_line_chart <-
                renderPlot({
                    progress <- shiny::Progress$new()
                    # Make sure progress closes when reactive exits, even if there's an error
                    on.exit(progress$close())
                    progress$set(message = "Processing: ", value = 0)
                    
                    # Search for matching terms
                    progress$inc(1 / 2, detail = "Finding matches...")
                    results <- ngrams_df %>%
                        filter(ngram %in% search_terms[[1]])
                    
                    # Display empty message if no results found
                    if (nrow(results) == 0) {
                        text = "No results were found. Please try a new search."
                        output$ngram_line_chart <-
                            build_empty_plot(text)
                    }
                    
                    # Draw Plot
                    progress$inc(2 / 2, detail = "Creating plot...")
                    build_ngram_line_plot(results)
                })
        }
    })
})
