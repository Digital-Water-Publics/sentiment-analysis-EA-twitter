# Clean tweets ------------------------------------------------------------
clean_tweets = function(tweets_text) {
  tweets_text$text = gsub("http.+ |http.+$", " ", tweets_text$text)  # Remove html links
  tweets_text$text = gsub("http[[:alnum:]]*", "", tweets_text$text) # Remove html links
  tweets_text$text = gsub("[[:punct:]]", " ", tweets_text$text)  # Remove punctuation
  tweets_text$text = gsub("[ |\t]{2,}", " ", tweets_text$text)  # Remove tabs
  tweets_text$text = gsub("^ ", "", tweets_text$text)  # Leading blanks
  tweets_text$text = gsub(" $", "", tweets_text$text)  # Lagging blanks
  tweets_text$text = gsub(" +", " ", tweets_text$text) # General spaces
  tweets_text$text = gsub("amp", " ", tweets_text$text) # remove amp
  tweets_text$text = gsub("RT", " ", tweets_text$text) # remove amp
}
# Test clean_tweets(tweets_text = tt) 


