if(exists("tweets_text_single")) {
  for (i in 1:nrow(tweets_text_single)){
    env_mentions = iconv(tweets_text_single$text, to = "ASCII", sub = " ") # Convert to basic ASCII text to avoid silly characters
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
    tweets_text_single$text = removeWords(tweets_text_single$text, stopwords("english"))
    tweets_text_single$text = stripWhitespace(tweets_text_single$text)
  }
}
#Clean tweets for frequency analysis

#Clean tweets for sentiment
clean_tweets_sentiment = function(x) {
  x %>%
    # Remove URLs
    str_remove_all(" ?(f|ht)(tp)(s?)(://)(.*)[.|/](.*)") %>%
    # Remove mentions e.g. "@my_account"
    str_remove_all("@[[:alnum:]_]{4,}") %>%
    # Remove hashtags
    str_remove_all("#[[:alnum:]_]+") %>%
    # Replace "&" character reference with "and"
    str_replace_all("&amp;", "and") %>%
    # Remove puntucation, using a standard character class
    str_remove_all("[[:punct:]]") %>%
    # Remove "RT: " from beginning of retweets
    str_remove_all("^RT:? ") %>%
    # Replace any newline characters with a space
    str_replace_all("\\\n", " ") %>%
    # Make everything lowercase
    str_to_lower() %>%
    # Remove any trailing whitespace around the text
    str_trim("both")
}

#Subset data to only include tweet
tweets_text_single = as.data.frame(subset(tweets,
                                          select = c("text")))
