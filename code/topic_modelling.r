if(file.exists("data/tweets_primary_df_topics_best.rds")){
  primary_df_topics = readRDS("data/tweets_primary_df_topics.rds")
} else {
  #set burn in
  burnin = 1000
  #set iterations
  iter=2000
  #thin the spaces between samples
  thin = 500
  #set random starts at 5
  nstart = 5
  #use random integers as seed 
  seed = list(123,456,789,987,654)
  # return the highest probability as the result
  best = TRUE
  
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
      control = list(nstart=nstart, seed = seed, best=best, burnin = burnin, iter = iter, thin=thin)
    )
    print("Extracting model features")
    top10terms_10 = as.matrix(terms(text_lda, 10))
    write.csv(top10terms_10,
              paste("top_10_topic_terms_best_", n, ".csv", sep = ""))
    #Create topic per tweet
    topic_per_tweet = tidy(text_lda, matrix = "gamma") %>%
      pivot_wider(names_from = topic, values_from = "gamma")
    
    if (n == 5) {
      print("n = 5. Calculating best topics")
      suppressWarnings(for (i in 1:nrow(topic_per_tweet)) {
        topic_per_tweet$key_topic[i] = max.col(topic_per_tweet[i,2:6])
      })
    } else if (n == 6) {
      print("n = 6. Calculating best topics")
      suppressWarnings(for (i in 1:nrow(topic_per_tweet)) {
        topic_per_tweet$key_topic[i] = max.col(topic_per_tweet[i,2:7])
      })
    } else if (n == 7) {
      print("n = 7. Calculating best topics")
      suppressWarnings(for (i in 1:nrow(topic_per_tweet)) {
        topic_per_tweet$key_topic[i] = max.col(topic_per_tweet[i,2:8])
      })
    } else if (n == 8) {
      print("n = 8. Calculating best topics")
      suppressWarnings(for (i in 1:nrow(topic_per_tweet)) {
        topic_per_tweet$key_topic[i] =max.col(topic_per_tweet[i,2:9])
      })
    }
    #Rename column
    topic_per_tweet = topic_per_tweet %>%
      rename_all(paste0, " topic(k=", n, ")")
    colnames(topic_per_tweet)[1] = "document"
    topic_per_tweet$document = as.numeric(topic_per_tweet$document)
    
    tweets_primary_df <<-left_join(topic_per_tweet, tweets_primary_df)
    print("merge complete")
  }
  topic_model_tweet_corpus(tweet_vector = tweets_primary_df$word, n = 5)
  topic_model_tweet_corpus(tweet_vector = tweets_primary_df$word, n = 6)
  topic_model_tweet_corpus(tweet_vector = tweets_primary_df$word, n = 7)
  topic_model_tweet_corpus(tweet_vector = tweets_primary_df$word, n = 8)
  
  saveRDS(tweets_primary_df,"data/tweets_primary_df_topics_best.rds")
  
}

topics6 = tweets_primary_df %>% select(2:7)
topics5 = tweets_primary_df %>% select(9:13)

plot_topic_prob = function(topic) {
  
  topics5 %>%
    pivot_longer(cols = 1:5) %>% 
    ggplot(aes(value)) + 
    geom_histogram() + 
    facet_wrap(~name, scales = "free")
  
  ggsave(paste("data/",topic, "_hist_plot.png", sep = ""))
}

plot_topic_prob(topic = topics5)
plot_topic_prob(topic = topics6)
plot_topic_prob(topic = topics7)
plot_topic_prob(topic = topics8)