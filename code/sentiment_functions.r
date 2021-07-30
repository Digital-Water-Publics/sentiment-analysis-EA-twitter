# Sentiment Functions -----------------------------------------------------
## NRC Sentiment Functions
### 1. General sentiment function and viz
general_nrc_sentiment = function(corpus) {
  #token df
  token = data.frame(text = topic_sub$word, stringsAsFactors = FALSE) %>%
    unnest_tokens(word, text)
  
  #Matching sentiment words from the 'NRC' sentiment lexicon
  senti = inner_join(token, get_sentiments("nrc")) %>%
    count(sentiment) %>%
    filter(sentiment != "positive") %>%
    filter(sentiment != "negative") %>%
    arrange(sentiment) 
  
  senti$percent = (senti$n / sum(senti$n)) * 100
  
  #Plotting the sentiment summary
  ggplot(senti, aes(sentiment, percent)) +
    geom_bar(aes(fill = sentiment), position = 'stack', stat = 'identity') +
    ggtitle("(NRC EMO-LEX) \nEmotional Sentiment for Environment Agency \nmentions on Twitter") +
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
# test general_nrc_sentiment(corpus = clean_tweets)

### 2. Entity (word) sentiment function and viz
entity_nrc_sentiment = function(word) {
  corpus = corpus(tweets_primary_df$word)
  corpus = (corpus_water = subset(corpus, grepl(word, texts(corpus))))
  token_word = data.frame(text = corpus, stringsAsFactors = FALSE) %>% unnest_tokens(word, text)
  senti_word = inner_join(token_word, get_sentiments("nrc")) %>%
    count(sentiment)
  senti_word$percent = (senti_word$n / sum(senti_word$n)) * 100
  ggplot(senti_word, aes(sentiment, percent)) +
    geom_bar(aes(fill = sentiment), position = 'stack', stat = 'identity') +
    ggtitle(paste(word, sep = " ", "word sentiment \nfrom Environment Agency Mentions")) +
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
entity_nrc_sentiment(word = "pollut")

## Polarity (pos-neg) Sentiment Functions
### 3. General sentiment polarity function and histogram
polarity_tweet_sentiment = function(corpus) {
  # get average sentiment score for each sentence
  sentiment_support <- sentiment_by(get_sentences(corpus$text))
  #plot the score distribution
  ggplot(sentiment_support, aes(ave_sentiment)) +
    geom_histogram(bins = 50) +
    labs(title = "Sentiment Histogram of Tweets", x = "Sentiment Score") +
    theme_bw() +
    theme(plot.title = element_text(
      size = 14,
      face = "bold",
      hjust = 0.5
    )) +
    geom_vline(xintercept = 0, color = "red")
}
#test polarity_tweet_sentiment(corpus = tweets_text)