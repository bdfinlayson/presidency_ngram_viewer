#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

jscode <- '$(document).keyup(function(e) {
    if (e.key == "Enter") {
    $("button").click();
}});'

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    fluidPage(
        tags$head(tags$script(HTML(jscode))),
        fluidRow(column(
            2,
            br(),
            tags$img(
                src = 'https://upload.wikimedia.org/wikipedia/commons/3/36/Seal_of_the_President_of_the_United_States.svg',
                height = 100,
                widgth = 100
            )
        ),
        column(
            10,
            h1('Presidential Documents Word Usage Search'),
            strong(
                'Discover word usage frequencies by US presidents over time across 118,561 official documents and transcripts'
            ),
            p()
        )),
        fluidRow(column(
            10,
            br(),
            textInput(
                'ngram_search',
                label = NULL,
                value = '',
                width = '100%',
                placeholder = 'global warming; climate change'
            ),
            em('Separate each search term with a semicolon',
               align = 'top')
        ),
        column(
            2,
            br(),
            submitButton('Search',
                         icon = icon('search'))
        )),
        
        fluidRow(column(12,
                        p(),
                        plotOutput('ngram_line_chart')))

    ),
    tabsetPanel(type = 'tabs',
            tabPanel('Summary',
                     fluidPage(fluidRow(
                         column(12,
                                p(),
                                htmlOutput('summary'))
                     ))),
            tabPanel('Documents',
                     fluidPage(fluidRow(
                         column(12,
                                p(),
                                htmlOutput('documents'))
                     )))
)))