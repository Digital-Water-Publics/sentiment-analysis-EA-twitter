# Install and load all the wonderful packages that made this analysis possible --------------------------------------------------------
source("code/install_libs.R")
# Mine the Twittersphere  -------------------------------------------------------
source("code/twitter_mine.R")
# Sentiment functions -----------------------------------------------------
source("code/sentiment.r")

# git add -A
# git commit -am 'Update sites'
# git push



tt = as.data.frame(tweets_text$text)  
#Analyze sentiment
sentiment <- analyzeSentiment(tt)
#Extract dictionary-based sentiment according to the QDAP dictionary
sentiment2 <- sentiment$SentimentQDAP
#View sentiment direction (i.e. positive, neutral and negative)
tt$sentiment <- convertToDirection(sentiment$SentimentQDAP)
