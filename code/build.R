# Install and load all the wonderful packages that made this analysis possible --------------------------------------------------------
source("code/install_libs.R")

#logic gate to trigger twitter mine
mine_build = FALSE
if (mine_build) {
  # Mine the Twittersphere  -------------------------------------------------------
  source("code/twitter_mine.R")
}
# Helper methods --------------------------------------------------------
source("code/setup.r")


historical_sentiment = tweets_primary_df %>% group_by(date) %>% count(anticipation, fear, sadness, anger, disgust, surprise, trust, joy)

oo = tweets_primary_df %>% aggregate(
  cbind(
    tweets_primary_df$date,
    tweets_primary_df$anger,
    tweets_primary_df$anticipation,
    tweets_primary_df$disgust,
    tweets_primary_df$fear,
    tweets_primary_df$joy,
    tweets_primary_df$sadness,
    tweets_primary_df$surprise,
    tweets_primary_df$trust
  )
)
aa = aggregate(x = tweets_primary_df, by = list(tweets_primary_df$trust, tweets_primary_df$anger), FUN = "sum")


tweets_primary_df$anger = replace_na(tweets_primary_df$anger, 0)
tweets_primary_df$anticipation = replace_na(tweets_primary_df$anticipation, 0)
tweets_primary_df$disgust = replace_na(tweets_primary_df$disgust, 0)
tweets_primary_df$fear = replace_na(tweets_primary_df$fear, 0)
tweets_primary_df$joy = replace_na(tweets_primary_df$joy, 0)
tweets_primary_df$sadness = replace_na(tweets_primary_df$sadness, 0)
tweets_primary_df$surprise = replace_na(tweets_primary_df$surprise, 0)
tweets_primary_df$trust = replace_na(tweets_primary_df$trust, 0)

ooo = tweets_primary_df %>% 
  group_by(date) %>%
  summarise(across(anticipation:joy, ~ sum(.x, na.rm = FALSE)))

# N-grams -----------------------------------------------------------------
source("code/n_grams.r")
# Calculate colocation for sinlge phrases
suppressMessages(for (i in 1:nrow(nounphrase)) {
  bigram_plots(data = bigrams_filtered, word = nounphrase$phrasetoken[i])
})
# Calculate colocation for nounphrases
words = as.data.frame(c("river", "flood", "water", "waste", "pollution", "sewage", "bad"))
colnames(words) = "word"
suppressMessages(for (i in 1:nrow(words)) {
  bigram_plots(data = bigrams_filtered_3, word = words$word[i])
})

rm(bigrams_filtered, bigrams_filtered_3, words, nounphrase)
# Topic modelling -----------------------------------------------------------------
source("code/topic_modelling.r")
topic_model_tweet_corpus(tweet_vector = tweets_primary_df$word)




# Sentiment functions -----------------------------------------------------
source("code/sentiment_functions.r")

# Sentiment dataframe
source("code/sentiment.R")