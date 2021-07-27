topic_model_tweet_corpus = function(tweet_vector,n) {
  #Convert tweets into corpus
  text_corpus <- SimpleCorpus(VectorSource(tweet_vector))
  #Convert corpus into document term matrix
  text_dtm <- DocumentTermMatrix(
    text_corpus,
    control = list(
      tolower = TRUE,
      removePunctuation = TRUE,
      removeNumbers = TRUE,
      stopwords = TRUE,
      sparse = TRUE
    )
  )
  
  #Create sparse matrix for memory issues
  text_dtm2 <- Matrix::sparseMatrix(
    i = text_dtm$i,
    j = text_dtm$j,
    x = text_dtm$v,
    dims = c(text_dtm$nrow, text_dtm$ncol),
    dimnames = text_dtm$dimnames
  )
  #Calculate row sums
  doc_lengths <- Matrix::rowSums(text_dtm2)
  #Remove doc lenths < 0
  text_dtm3 <- text_dtm2[doc_lengths > 0,]
  
  set.seed(9393)
  #LDA model
  text_lda <- LDA(text_dtm3,
                  k = n,
                  method = "Gibbs",
                  control = NULL)
  top10terms_10 = as.matrix(terms(text_lda, 10))
  top10terms_10
  
}

