library(shiny)
library(dplyr)

# Define UI
ui <- fluidPage(
        titlePanel("Next Word Predictor"),
        
        sidebarLayout(
                sidebarPanel(
                        textInput("partial_sentence", "Enter a sentence with 3 words such as 'i wish you happy mothers':"),
                        actionButton("submit_button", "Submit")
                ),
                
                mainPanel(
                        div(style = "font-size: 24px; font-weight: bold; margin-bottom: 20px;", "Next word:"),
                        verbatimTextOutput("next_word_output")
                )
        )
)

# Define server
server <- function(input, output) {
        # Read the rds files
        ngram1 <- readRDS("unigrams.RData")
        ngram2 <- readRDS("bigrams.RData")
        ngram3 <- readRDS("trigrams.RData")
        
        predict_next_word <- function(partial_sentence) {
                
                # Convert the partial sentence to a character vector
                partial_words <- strsplit(as.character(partial_sentence), " ")[[1]]
                
                # If the partial sentence is empty or contains only one word, return an empty string
                if (length(partial_words) < 2) {
                        return("")
                }
                
                # Determine the n-gram type to use based on the length of the partial sentence
                if (length(partial_words) == 2) {
                        ngram_type <- ngram2 
                }    else { 
                        ngram_type <- ngram3
                }
                # Check that ngram_type is a data frame with columns "word" and "n"
                if (!is.data.frame(ngram_type) || !all(c("word", "n") %in% names(ngram_type))) {
                        stop("Invalid n-gram data type.")
                }
                
                # Create a data frame with the last two words of the partial sentence
                partial_df <- data.frame(word = tail(partial_words, 2), stringsAsFactors = FALSE)
                
                # Filter the n-gram data frame to only include rows that match the last two words of the partial sentence
                idx <- grep(paste(partial_df$word, collapse = "|"), ngram_type$word)
                filtered_ngrams <- ngram_type[idx, ]
                
                # Create a frequency table of the next word in the filtered n-grams
                next_word_table <- filtered_ngrams %>%
                        group_by(word) %>%
                        summarize(total = sum(n)) %>%
                        arrange(desc(total))
                
                # If there are no matches, return an empty string
                if (nrow(next_word_table) == 0) {
                        return("")
                }
                
                # Return the most frequent next word
                next_word <- next_word_table$word[1]
                return(next_word)
        }
        
        # Initialize the output box with an empty value
        output$next_word_output <- renderPrint("")
        
        # Update the output box when the submit button is clicked
        observeEvent(input$submit_button, {
                next_word <- predict_next_word(input$partial_sentence)
                output$next_word_output <- renderPrint(next_word)
        })
}

# Run the app
shinyApp(ui = ui, server = server)
