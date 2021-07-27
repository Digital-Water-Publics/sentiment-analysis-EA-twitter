topic_model_tweet_corpus = function(tweet_vector, n) {
  # Converting tweets into corpus ----------------------------------------------
  print("Converting tweets into corpus")
  text_corpus = SimpleCorpus(VectorSource(tweet_vector))
  # Converting corpus into DTM ----------------------------------------------
  text_dtm = DocumentTermMatrix(
    text_corpus,
    control = list(
      tolower = TRUE,
      removePunctuation = TRUE,
      removeNumbers = TRUE,
      stopwords = TRUE,
      sparse = TRUE
    )
  )
  print("Converting corpus into DTM")
  # Generating sparse matrix to save memory ----------------------------------------------
  text_dtm2 = Matrix::sparseMatrix(
    i = text_dtm$i,
    j = text_dtm$j,
    x = text_dtm$v,
    dims = c(text_dtm$nrow, text_dtm$ncol),
    dimnames = text_dtm$dimnames
  )
  # Calculating row sums ----------------------------------------------
  doc_lengths = Matrix::rowSums(text_dtm2)
  # Removing lengths of 0 ----------------------------------------------
  text_dtm3 = text_dtm2[doc_lengths > 0,]
  print("Calculated spare matrix for memory isses")
  # Building LDA model ----------------------------------------------
  print("building LDA model")
  text_lda = LDA(
    text_dtm3,
    k = n,
    method = "Gibbs",
    control = list(seed = 2021)
  )
  print("Extracting model features")
  top10terms_10 = as.matrix(terms(text_lda, 10))
  write.csv(top10terms_10,
            paste("top_10_topic_terms", n, ".csv", sep = ""))
  #Create topic per tweet
  topic_per_tweet = tidy(text_lda, matrix = "gamma") %>%
    pivot_wider(names_from = topic, values_from = "gamma")
  #Calculate main topic
  pb = txtProgressBar(min = 0,
                      max = length(topic_per_tweet),
                      initial = 0)
  
  if (n == 5) {
    print("Negative number")
    suppressWarnings(for (i in 1:nrow(topic_per_tweet)) {
      topic_per_tweet$key_topic[i] = max.col(topic_per_tweet[2:6])
    })
  } else if (n == 6) {
    suppressWarnings(for (i in 1:nrow(topic_per_tweet)) {
      topic_per_tweet$key_topic[i] = max.col(topic_per_tweet[2:7])
    })
  } else if (n == 7) {
    print("Positive number")
    suppressWarnings(for (i in 1:nrow(topic_per_tweet)) {
      topic_per_tweet$key_topic[i] = max.col(topic_per_tweet[2:8])
    })
  } else if (n == 8) {
    suppressWarnings(for (i in 1:nrow(topic_per_tweet)) {
      topic_per_tweet$key_topic[i] = max.col(topic_per_tweet[2:9])
    })
  }
  
  
  apply(topic_per_tweet$key, 1, FUN = max.col())
  topic_per_tweet$key_topic = apply(topic_per_tweet, 1, function(x)
    which(x == max(x)))
  
  j1 <-
    #Rename column
    topic_per_tweet = topic_per_tweet %>%
    rename_all(paste0, " topic(k=", n, ")")
  colnames(topic_per_tweet)[1] = "document"
  
  tweets_primary_df <<-
    left_join(topic_per_tweet, tweets_primary_df)
  print("merge complete")
}
topic_model_tweet_corpus(tweet_vector = tweets_primary_df$word, n = 5)
topic_model_tweet_corpus(tweet_vector = tweets_primary_df$word, n = 6)
topic_model_tweet_corpus(tweet_vector = tweets_primary_df$word, n = 7)
topic_model_tweet_corpus(tweet_vector = tweets_primary_df$word, n = 8)
