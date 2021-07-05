tweets_text_single = as.data.frame(subset(tweets,
                                            select = c("text")))
# Clean tweets ------------------------------------------------------------
tidy_tweets_option = yes

if(tweets_text_single){
  tweets_text_single$text = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", tweets_text_single$text)  # Remove the "RT" (retweet) and usernames
  tweets_text_single$text = gsub("http.+ |http.+$", " ", tweets_text_single$text)  # Remove html links
  tweets_text_single$text = gsub("http[[:alnum:]]*", "", tweets_text_single$text) # Remove html links
  tweets_text_single$text = gsub("[[:punct:]]", " ", tweets_text_single$text)  # Remove punctuation
  tweets_text_single$text = gsub("[ |\t]{2,}", " ", tweets_text_single$text)  # Remove tabs
  tweets_text_single$text = gsub("^ ", "", tweets_text_single$text)  # Leading blanks
  tweets_text_single$text = gsub(" $", "", tweets_text_single$text)  # Lagging blanks
  tweets_text_single$text = gsub(" +", " ", tweets_text_single$text) # General spaces
  tweets_text_single$text = gsub("amp", " ", tweets_text_single$text) # remove amp
  tweets_text_single$text = gsub("can", " ", tweets_text_single$text) # remove can
  tweets_text_single$text = gsub("will", " ", tweets_text_single$text) # remove will
  tweets_text_single$text = gsub("EnvAgency", " ", tweets_text_single$text) # remove envagency
  tweets_text_single$text = gsub("EnvAgencyYNE", " ", tweets_text_single$text) # remove envagency
  tweets_text_single$text = gsub("EnvAgencyNW", " ", tweets_text_single$text) # remove envagency
  tweets_text_single$text = gsub("EnvAgencySE", " ", tweets_text_single$text) # remove envagency
  tweets_text_single$text = gsub("EnvAgencySW", " ", tweets_text_single$text) # remove envagency
  tweets_text_single$text = gsub("EnvAgencyAnglia", " ", tweets_text_single$text) # remove envagency
  tweets_text_single$text = gsub("EnvAgencyMids", " ", tweets_text_single$text) # remove envagency
  tweets_text_single$text = gsub("got", " ", tweets_text_single$text) # remove got
  tweets_text_single$text = gsub("way", " ", tweets_text_single$text) # remove way
  tweets_text_single$text = gsub("one", " ", tweets_text_single$text) # remove one
} else{
  message("tweets are not cleaned")
}
