# Install and load all the wonderful packages that made this analysis possible --------------------------------------------------------
source("code/install_libs.R")

#logic gate to trigger twitter mine
mine_build = FALSE
if (mine_build) {
  # Mine the Twittersphere  -------------------------------------------------------
  source("code/twitter_mine.R")
}
# Build set up including: general functions, data transformation --------------------------------------------------------
source("code/build_setup.r")

# N-grams -----------------------------------------------------------------
source("code/n_grams.r")

# Calculate collocation for bigram phrases
suppressWarnings(
  suppressMessages(for (i in 1:nrow(nounphrase)) {
    bigram_plots(data = bigrams_filtered, word = nounphrase$phrasetoken[i])
  }))

# Calculate colocation for nounphrases
words = as.data.frame(c("river", "flood", "water", "waste", "pollution", "sewage", "bad"))
colnames(words) = "word"
suppressWarnings(
  suppressMessages(for (i in 1:nrow(words)) {
    bigram_plots(data = bigrams_filtered_3, word = words$word[i])
  })
)
# remove dataframes
rm(bigrams_filtered, bigrams_filtered_3, words, nounphrase)


# Semantic Frequency ------------------------------------------------------

source("code/semantic_freq.r")

# OPTIONAL 
# Topic modelling -----------------------------------------------------------------
source("code/topic_modelling.r")
#topic_model_tweet_corpus(tweet_vector = tweets_primary_df$word)
# Networks ----------------------------------------------------------------
source("code/networks.R")

