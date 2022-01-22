library(shiny)
library(stringr)
library(ggplot2)
library(dplyr)
library(dbplyr)
library(DBI)
source('./helpers/plot_helper.R')
source('./helpers/image_helper.R')
source('./helpers/ngram_detail_helper.R')
# source('./helpers/documents_helper.R')

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
                    
                    progress$set(message = ": ", value = 0)
                    
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
        
        output$summary <- renderUI({
            elements <- list()
            
            if (search_terms[[1]] != '') {
                con <- db_connect(path = './data/ngrams.sqlite')
                index <- 1
                counter = 0
                for (term in search_terms[[1]]) {
                    progress <- shiny::Progress$new()
                    # Make sure progress closes when reactive exits, even if there's an error
                    on.exit(progress$close())
                    progress$set(message = "Usage stats:", value = 10)
                    progress$inc(1 / 10, detail = "building summary data...")
                    summary_df <-
                        ngrams_df %>% filter(ngram == term) %>% group_by(president) %>% summarize(
                            frequency = sum(freq),
                            start_year = min(year),
                            end_year = max(year)
                        )
                    
                    if (nrow(summary_df) == 0) {
                        next
                    }
                    
                    progress$inc(2 / 10, detail = "finding occurances...")
                    first_said <-
                        summary_df %>% arrange(start_year) %>% head(n = 1)
                    last_said <-
                        summary_df %>% arrange(desc(end_year)) %>% head(n = 1)
                    most_said <-
                        summary_df %>% arrange(desc(frequency)) %>% head(n = 1)
                    
                    # get additional ngram stats
                    progress$inc(3 / 10, detail = "finding first said...")
                    corpuses_first <-
                        db_find_all_ngram_corpuses_by_president(con, term, first_said$president) %>% as.data.frame()
                    progress$inc(4 / 10, detail = "finding last said...")
                    corpuses_last <-
                        db_find_all_ngram_corpuses_by_president(con, term, last_said$president) %>% as.data.frame()
                    progress$inc(5 / 10, detail = "finding most said...")
                    corpuses_most <-
                        db_find_all_ngram_corpuses_by_president(con, term, most_said$president) %>% as.data.frame()
                    # total_said <-
                    #    db_get_total_said(con, term) %>% .$total_said
                    progress$inc(6 / 10, detail = "calculating totals...")
                    
                    # totals
                    total_documents <-
                        db_count_all_ngram_corpuses(con, term) %>%
                        .$total_documents %>%
                        format(format = 'd', big.mark = ',')
                    # top_category <-
                    #     db_find_top_category(con, term) %>%
                    #     .$category %>%
                    #     first_in_list()
                    # top_location <-
                    #     db_find_top_location(con, term) %>%
                    #     .$location
                    
                    progress$inc(9 / 10, detail = "building ui...")
                    element <- tags$div(
                        fluidRow(
                            hr(),
                            column(
                                3,
                                tags$blockquote(term),
                                tags$table(# tags$tr(tags$td(
                                    #     strong('Total said:')
                                    # ), tags$td(total_said)),
                                    tags$tr(tags$td(
                                        strong('Total documents said in:')
                                    )),
                                    tags$tr(
                                        tags$td(total_documents)
                                    ))
                            ),
                            # tags$tr(tags$td(hr()), tags$td(hr())),
                            # tags$tr(tags$td(strong('Top category said in:'))),
                            # tags$tr(tags$td(top_category)),
                            # tags$tr(tags$td(hr()), tags$td(hr())),
                            # tags$tr(tags$td(strong('Top location said in:'))),
                            # tags$tr(tags$td(top_location)))),
                            build_ngram_detail_column(
                                title = 'First To Say:',
                                president_name = first_said$president,
                                # n_times_said = db_get_total_said_by_president(con, term, first_said$president) %>% .$total_said,
                                n_documents = n_documents(corpuses_first),
                                #top_category = db_find_top_category_by_president(con, term, first_said$president) %>%
                                #    .$category %>%
                                #    first_in_list(),
                                top_category = top_item(corpuses_first$categories),
                                # top_doc = top_item(corpuses_first$document_uri),
                                # top_location = db_find_top_location_by_president(con, term, first_said$president) %>% .$location,
                                top_location = top_item(corpuses_first$location),
                                start_year = first_said$start_year,
                                end_year = first_said$end_year
                            ),
                            build_ngram_detail_column(
                                title = 'Last To Say:',
                                president_name = last_said$president,
                                # n_times_said = db_get_total_said_by_president(con, term, last_said$president) %>% .$total_said,
                                n_documents = n_documents(corpuses_last),
                                # top_category = db_find_top_category_by_president(con, term, last_said$president) %>%
                                #     .$category %>%
                                #     first_in_list(),
                                top_category = top_item(corpuses_last$categories),
                                # top_doc = top_item(corpuses_last$document_uri),
                                #top_location = db_find_top_location_by_president(con, term, last_said$president) %>% .$location,
                                top_location = top_item(corpuses_last$location),
                                start_year = last_said$start_year,
                                end_year = last_said$end_year
                            ),
                            build_ngram_detail_column(
                                title = 'Most Often Said:',
                                president_name = most_said$president,
                                # n_times_said = db_get_total_said_by_president(con, term, most_said$president) %>% .$total_said,
                                n_documents = n_documents(corpuses_most),
                                # top_category = db_find_top_category_by_president(con, term, most_said$president) %>%
                                #     .$category %>%
                                #     first_in_list(),
                                top_category = top_item(corpuses_most$categories),
                                # top_doc = top_item(corpuses_most$document_uri),
                                # top_location = db_find_top_location_by_president(con, term, most_said$president) %>% .$location,
                                top_location = top_item(corpuses_most$location),
                                start_year = most_said$start_year,
                                end_year = most_said$end_year
                            ),
                            br(),
                            hr()
                        ),
                    )
                    elements[index] = element$children
                    index <- index + 1
                    progress$close()
                }
                dbDisconnect(con)
            } else {
                elements[1] = 'No results to display'
            }
            
            return(elements)
        })
        
        ####################
        ## Render Documents
        ####################
        
        output$documents <- renderUI({
            elements <- list()
            
            
            build_document <-
                function(title,
                         date,
                         uri,
                         word_count,
                         snippet) {
                    tags$div(tags$div(
                        strong(tags$a(
                            title,
                            href = str_interp('https://www.presidency.ucsb.edu/${uri}')
                        )),
                        br(),
                        HTML(
                            str_interp(
                                '<p><strong>Date:</strong> ${date} | <strong>Word Count:</strong> ${word_count}</p>'
                            )
                        ),
                        HTML(snippet),
                        hr()
                    ))
                }
            
            build_documents <- function(documents) {
                elements <- list()
                
                for (i in 1:nrow(documents)) {
                    df = data.frame(
                        title = documents[i, 2],
                        date = documents[i, 3] %>% str_sub(1, 10),
                        uri = documents[i, 4],
                        word_count = documents[i, 5] %>% format(format = 'd', big.mark =
                                                                    ','),
                        snippet = documents[i, 6]
                    )
                    
                    elements[i] = build_document(df$title,
                                                 df$date,
                                                 df$uri,
                                                 df$word_count,
                                                 df$snippet) %>% .$children
                }
                
                return(elements)
            }
            
            if (search_terms[[1]] != '') {
                con <- db_connect(path = './data/ngrams.sqlite')
                index <- 1
                counter = 0
                for (term in search_terms[[1]]) {
                    progress <- shiny::Progress$new()
                    # Make sure progress closes when reactive exits, even if there's an error
                    on.exit(progress$close())
                    progress$set(message = "Document search:",
                                 value = 10)
                    
                    summary_df <-
                        ngrams_df %>% filter(ngram == term) %>% group_by(president) %>% summarize(
                            frequency = sum(freq),
                            start_year = min(year),
                            end_year = max(year)
                        )
                    
                    if (nrow(summary_df) == 0) {
                        next
                    }
                    
                    first_said <-
                        summary_df %>% arrange(start_year) %>% head(n = 1)
                    last_said <-
                        summary_df %>% arrange(desc(end_year)) %>% head(n = 1)
                    most_said <-
                        summary_df %>% arrange(desc(frequency)) %>% head(n = 1)
                    
                    progress$inc(1 / 2, detail = "finding matches...")
                    first_said_documents <-
                        db_find_top_documents_by_president(con,
                                                           term,
                                                           first_said$president) %>% as.data.frame()
                    last_said_documents <-
                        db_find_top_documents_by_president(con,
                                                           term,
                                                           last_said$president) %>% as.data.frame()
                    most_said_documents <-
                        db_find_top_documents_by_president(con,
                                                           term,
                                                           most_said$president) %>% as.data.frame()
                    
                    progress$inc(2 / 2, detail = "building ui...")
                    element <- tags$div(fluidPage(
                        fluidRow(
                            column(
                                2,
                                tags$blockquote(term),
                                tags$image(
                                    src = name_to_url(first_said$president),
                                    height = '100px',
                                    width = '80px'
                                ),
                                br(),
                                em(first_said$president),
                                br()
                            ),
                            column(
                                10,
                                build_documents(first_said_documents)
                            )
                        ),
                        fluidRow(
                            column(
                                2,
                                tags$blockquote(term),
                                tags$image(
                                    src = name_to_url(last_said$president),
                                    height = '100px',
                                    width = '80px'
                                ),
                                br(),
                                em(last_said$president),
                                br()
                            ),
                            column(
                                10,
                                build_documents(last_said_documents)
                            )
                        ),
                        fluidRow(
                            column(
                                2,
                                tags$blockquote(term),
                                tags$image(
                                    src = name_to_url(most_said$president),
                                    height = '100px',
                                    width = '80px'
                                ),
                                br(),
                                em(most_said$president),
                                br()
                            ),
                            column(
                                10,
                                build_documents(most_said_documents)
                            )
                        )
                    ))
                    elements[index] = element$children
                    index <- index + 1
                    progress$close()
                }
                dbDisconnect(con)
            } else {
                elements[1] = 'No documents to display'
            }
            
            return(elements)
        })
    })
})
