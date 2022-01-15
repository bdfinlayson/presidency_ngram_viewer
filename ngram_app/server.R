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
        
        if (search_terms[[1]] != '') {
            output$ngram_line_chart <-
                renderPlot({
                    # Create a Progress object
                    progress <- shiny::Progress$new()
                    # Make sure it closes when we exit this reactive, even if there's an error
                    on.exit(progress$close())
                    
                    progress$set(message = "Processing: ", value = 0)
                    
                    progress$inc(1 / 2, detail = "Finding matches...")
                    
                    res <- ngrams_df %>%
                        filter(ngram %in% search_terms[[1]])
                    
                    if (nrow(res) == 0) {
                        text = paste("No results were found. Please try a new search.")
                        output$ngram_line_chart <- renderPlot({
                            ggplot() +
                                annotate(
                                    "text",
                                    x = 4,
                                    y = 25,
                                    size = 8,
                                    label = text
                                ) +
                                theme_void()
                        })
                    }
                    
                    progress$inc(2 / 2, detail = "Creating plot...")
                    
                    # Plot
                    plot <- res %>%
                        group_by(year) %>%
                        ggplot(aes(
                            x = year,
                            y = log(freq, base = 10),
                            group = ngram,
                            color = ngram
                        )) +
                        geom_smooth(method = "loess", se = FALSE)
                    
                    plot + theme(axis.text.x = element_text(
                        angle = 90,
                        vjust = 0.5,
                        hjust = 1
                    ))
                })
        }
        else {
            text = paste(
                "\n   Please enter a search term in the input field above.\n",
                "       Separate each search term with a semicolon.\n",
                "       Usage frequency of the term will then be displayed."
            )
            output$ngram_line_chart <- renderPlot({
                ggplot() +
                    annotate(
                        "text",
                        x = 4,
                        y = 25,
                        size = 8,
                        label = text
                    ) +
                    theme_void()
            })
        }
    })
})
