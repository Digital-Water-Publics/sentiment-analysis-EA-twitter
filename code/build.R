# Install and load all the wonderful packages that made this analysis possible --------------------------------------------------------
source("code/install_libs.R")

#logic gate to trigger twitter mine
mine_build = FALSE
if(mine_build){
# Mine the Twittersphere  -------------------------------------------------------
  source("code/twitter_mine.R")
}

# Helper methods --------------------------------------------------------
source("code/helper_methods.r")

# Sentiment functions -----------------------------------------------------
source("code/sentiment_functions.r")

# Sentiment dataframe
source("code/sentiment.R")

twee = read.csv("data/parsed_tweets_POS_stop_words.csv")

twee_sub = twee %>%
  rename(word = token ) %>% 
  inner_join(get_sentiments("nrc"), by = "word")

emo_lex_trigger_freq = twee_sub %>% group_by(doc_id,word) %>% summarise(count=n())
emo_lex_senti_freq = twee_sub %>% group_by(doc_id,sentiment) %>% summarise(count=n())