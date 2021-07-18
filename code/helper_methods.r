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

if (file.exists("data/ea_mentions_2017_2021_cleaned.csv")) {
  clean_tweets = read.csv("data/ea_mentions_2017_2021_cleaned.csv")
} else {
  tweets = read.csv("data/ea_mentions_2017_2021.csv")
  clean_tweets = tweets %>% tweets$text %>% clean_tweets_sentiment()
  clean_tweets =  as.vector(clean_tweets)
  clean_tweets = removeWords(clean_tweets, words = stopwords("english"))
  
  write.csv(clean_tweets, "data/ea_mentions_2017_2021_cleaned.csv")
}
senti_polarity_tweets = function(tweets){
  #Calculate tweet polarity sentiment
  tweets$senti = sentiment(
    tweets$text,
    senti_dt = lexicon::hash_sentiment_nrc,
    emojis_dt = lexicon::hash_sentiment_emojis,
    hyphen = "",
    amplifier.weight = 0.8,
    n.before = Inf,
    n.after = Inf,
    question.weight = 1,
    adversative.weight = 0.25,
    neutral.nonverb.like = FALSE,
    missing_value = 0
  )
  
}
