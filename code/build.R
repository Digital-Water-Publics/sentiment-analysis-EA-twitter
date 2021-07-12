# Install and load all the wonderful packages that made this analysis possible --------------------------------------------------------
source("code/install_libs.R")

#logic gate to trigger twitter mine
mine_build = FALSE
if(mine_build){
# Mine the Twittersphere  -------------------------------------------------------
  source("code/twitter_mine.R")
}

tweets_2021 = read.csv("data/tweets_2021.csv")
tweets_2021_single = subset(tweets_2021,
                            select = text)

# Helper methods --------------------------------------------------------
source("code/helper_methods.r")

# Sentiment functions -----------------------------------------------------
source("code/sentiment_functions.r")

# Sentiment dataframe
source("code/sentiment.R")

