corpus = tm::Corpus(tm::VectorSource(tweets_primary_df$word))

# Handling UTF-8 encoding problem from the dataset
corpus.cleaned <- tm::tm_map(corpus, tm::removeWords, tm::stopwords('english')) # Removing stop-words
corpus.cleaned <- tm::tm_map(corpus.cleaned, tm::stemDocument, language = "english") # Stemming the words 
corpus.cleaned <- tm::tm_map(corpus.cleaned, tm::stripWhitespace) # Trimming excessive whitespaces

# Building the feature matrices
tdm <- tm::DocumentTermMatrix(corpus.cleaned)
tdm.tfidf <- tm::weightTfIdf(tdm)
# We remove A LOT of features. R is natively very weak with high dimensional matrix
tdm.tfidf <- tm::removeSparseTerms(tdm.tfidf, 0.999)
# There is the memory-problem part
# - Native matrix isn't "sparse-compliant" in the memory
# - Sparse implementations aren't necessary compatible with clustering algorithms
tfidf.matrix <- as.matrix(tdm.tfidf)
# Cosine distance matrix (useful for specific clustering algorithms)
dist.matrix = proxy::dist(tfidf.matrix, method = "cosine")

truth.K = 20
clustering.kmeans <- kmeans(tfidf.matrix, truth.K)

cluster_kmeans = as.data.frame(clustering.kmeans[["cluster"]]) %>% 
  rownames_to_column() %>%
  rename(tweet_n = rowname) 

cluster_kmeans$tweet_n = as.numeric(cluster_kmeans$tweet_n)
tweets_primary_df_kmeans = inner_join(cluster_kmeans,tweets_primary_df)
saveRDS(tweets_primary_df_kmeans,"data/tweets_primary_kmeans_df.rds")

cluster_1_kmeans = tweets_primary_df_kmeans %>% filter(`clustering.kmeans[["cluster"]]` == 1)
sentiment_key_word_subsets(df = cluster_1_kmeans)
sentiment_key_word_subsets = function(df) {
  corpus = corpus(df$word)
  token_word = data.frame(text = corpus, stringsAsFactors = FALSE) %>% unnest_tokens(word, text)
  senti_word = inner_join(token_word, get_sentiments("nrc")) %>%
    count(sentiment)
  senti_word$percent = (senti_word$n / sum(senti_word$n)) * 100
  ggplot(senti_word, aes(sentiment, percent)) +
    geom_bar(aes(fill = sentiment), position = 'stack', stat = 'identity') +
    coord_flip() +
    theme(
      plot.background = element_rect(fill = "black", colour = NA),
      panel.background = element_rect(fill = "black", colour = NA),
      legend.position = "none",
      axis.text = element_text(color = "white"),
      text = element_text(
        size = 16,
        family = "times",
        colour = "white"
      ),
    )
}

topic_frequent_emo_lex_trigger = function(topic_n)
{
  topic = tweets_primary_df_kmeans %>%
    filter(`clustering.kmeans[["cluster"]]` == topic_n)
  topic1_emo_lex = as.data.frame(unlist(topic$emo_lex_trigger)) %>% 
    rename(word = 1) %>% 
    count(word, sort = TRUE)
  DT::datatable(topic1_emo_lex, options = list(pageLength = 25))
}
topic_frequent_emo_lex_trigger(topic_n = 5)
