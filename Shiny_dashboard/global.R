# Global Stuff

# Function to generate a word-frequency matrix

getTermMatrix <- function(df) {
  text <-  df$title
  docs = Corpus(VectorSource(text)) 
  # Remove numbers
  docs <- tm_map(docs, removeNumbers)
  # Remove english common stopwords
  docs <- tm_map(docs, removeWords, stopwords("english"))
  # Remove your own stop word
  docs <- tm_map(docs, removeWords, c("and", "AND", "-")) 
  # Remove punctuations
  docs <- tm_map(docs, removePunctuation)
  dtm <- TermDocumentMatrix(docs)
  m <- as.matrix(dtm)
  v <- sort(rowSums(m),decreasing=TRUE)
  d <- data.frame(word = names(v),freq=v)
  return(d)
}

# Function to create country and number of measures matrix

