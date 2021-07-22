# Install and load all the wonderful packages that made this analysis possible --------------------------------------------------------
source("code/install_libs.R")

#logic gate to trigger twitter mine
mine_build = FALSE
if (mine_build) {
  # Mine the Twittersphere  -------------------------------------------------------
  source("code/twitter_mine.R")
}
# Helper methods --------------------------------------------------------
source("code/helper_methods.r")

# Sentiment functions -----------------------------------------------------
source("code/sentiment_functions.r")

# Sentiment dataframe
source("code/sentiment.R")