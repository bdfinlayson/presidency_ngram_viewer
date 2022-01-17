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
source('./helpers/image_helper.R')
source('./helpers/ngram_detail_helper.R')

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    observe({
        search_terms <- input$ngram_search %>%
            str_squish() %>%
            str_replace_all('; ', ';') %>%
            tolower() %>%
            str_split(';')
       
        ###################
        ## Render line plot
        ###################
        
        results <- data.frame()
        
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
        
        ######################
        # Render ngram details
        ######################
        
        output$ngram_details <- renderUI({
            elements <- list()
            progress <- shiny::Progress$new()
            # Make sure progress closes when reactive exits, even if there's an error
            on.exit(progress$close())
            progress$set(message = "Gathering stats...", value = 0)
            
            progress$inc(1 / 1, detail = "Building details...")
            
            if (search_terms[[1]] != '') {
                index <- 1
                for (term in search_terms[[1]]) {
                    # first_corpus <-
                    #     db_select_first_corpus_with_ngram(con,
                    #                                       'all_corpuses',
                    #                                       term,
                    #                                       order = 'asc')
                    # last_corpus <-
                    #     db_select_first_corpus_with_ngram(con,
                    #                                       'all_corpuses',
                    #                                       term,
                    #                                       order = 'desc')
                    summary <-
                        ngrams_df %>% filter(ngram == term) %>% group_by(president) %>% summarize(
                            frequency = sum(freq),
                            start_year = min(year),
                            end_year = max(year)
                        )
                    
                    if (nrow(summary) == 0) {
                        next
                    }
                    
                    first_said <- summary %>% arrange(start_year) %>% head(n = 1) 
                    last_said <- summary %>% arrange(desc(end_year)) %>% head(n = 1) 
                    most_said <- summary %>% arrange(desc(frequency)) %>% head(n = 1)
                    
                    element <- tags$div(
                        fluidRow(
                            hr(),
                            column(3,
                                   tags$blockquote(term)),
                            build_ngram_detail_column(
                                title = 'First To Say:',
                                president_name = first_said$president,
                                n_times_said = first_said$frequency,
                                start_year = first_said$start_year,
                                end_year = first_said$end_year 
                            ),
                            build_ngram_detail_column(
                                title = 'Last To Say:',
                                president_name = last_said$president,
                                n_times_said = last_said$frequency,
                                start_year = last_said$start_year,
                                end_year = last_said$end_year 
                            ),
                            build_ngram_detail_column(
                                title = 'Most Often Said:',
                                president_name = most_said$president,
                                n_times_said = most_said$frequency,
                                start_year = most_said$start_year,
                                end_year = most_said$end_year
                            )
                        )
                    )
                    elements[index] = element$children
                    index <- index + 1
                }
            }
            
            return(elements)
        })
    })
})
