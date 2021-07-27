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






library(Hmisc)
oot = ooo %>% select(-c(date))
res2 <- rcorr(as.matrix(oot))
library(corrplot)
corrplot(res2, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

M = cor(oot)
corrplot(M, method = 'square', diag = FALSE, order = 'hclust', 
         addrect = 2, rect.col = 'black', rect.lwd = 3, tl.pos = 'd', bg = "gold2") 

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