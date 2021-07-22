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

# N-grams -----------------------------------------------------------------
source("code/n_grams.r")
# Calculate colocation for sinlge phrases 
suppressMessages(
  for (i in 1:nrow(nounphrase)) {
    bigram_plots(data = bigrams_filtered, word = nounphrase$phrasetoken[i])
  }
)
# Calculate colocation for nounphrases 
words = as.data.frame(c("river", "flood", "water", "waste", "pollution", "sewage", "bad"))
colnames(words) = "word"
suppressMessages(
  for (i in 1:nrow(words)) {
    bigram_plots(data = bigrams_filtered_3, word = words$word[i])
  }
)

rm(bigrams_filtered,bigrams_filtered_3,words,nounphrase)
# Topic modelling -----------------------------------------------------------------
source("code/topic_modelling.r")

# Sentiment functions -----------------------------------------------------
source("code/sentiment_functions.r")

# Sentiment dataframe
source("code/sentiment.R")