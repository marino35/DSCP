# Create a function that predicts the next word given a partial sentence
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