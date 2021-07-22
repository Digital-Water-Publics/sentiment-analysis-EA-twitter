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

if(file.exists("data/primary_dataframe.rds")) {
  tweets_primary_df = readRDS("data/primary_dataframe.rds")
} else {
  #Clean tweets
  #tweets = tweets_no_ea$word %>% clean_tweets_sentiment()
  #name tweets and merge with NRC
  tweets = read.csv("data/ea_mentions_all.csv")
  tweets = tweets %>% rename(word = text)
  #nrc = tweets %>%
  # inner_join(get_sentiments("nrc"), by = "word")
  #Create table
  #table = as.data.frame(table(nrc$sentiment))

  #Read parsed tweets with stop words
  twee = read.csv("data/parsed_tweets_POS_stop_words.csv")

  #Join data with NRC lex
  twee_sub = twee %>%
    rename(word = token) %>%
    inner_join(get_sentiments("nrc"), by = "word")

  #Group emo-lex trigger words
  emo_lex_trigger_freq = twee_sub %>%
    group_by(doc_id, word) %>%
    summarise(count = n()) %>%
    select(doc_id, word)

  # Convert words to matrix
  o = split(emo_lex_trigger_freq$word, emo_lex_trigger_freq$doc_id)

  o = strsplit(as.character(o), "_")

  kl = cbind(o)
  kl = as.data.frame(kl)

  df = tibble::rownames_to_column(kl, "doc_id")
  df = as.data.frame(df)
  df$doc_id = as.numeric(df$doc_id)

  tweets$doc_id = seq.int(nrow(tweets))
  #text = "text"
  #tweets$doc_id = paste(text,tweets$index, sep = "")

  tweets = left_join(tweets, df) %>% rename(emo_lex_trigger = o)
  #tweetsTest$sent_score = tweetsTest$senti$sentiment
  #tweetsTest = tweetsTest %>% select(-c(senti))

  saveRDS(tweetsTest, file = "data/primary_dataframe.rds")
  readRDS("data/primary_dataframe.rds")

  #Group emo-lex sentiment freq
  emo_lex_senti_freq = twee_sub %>%
    group_by(doc_id, sentiment) %>%
    summarise(count = n()) %>%
    pivot_wider(names_from = sentiment, values_from = count)


  #Paste doc_id to column
  tweets$doc_id = seq.int(nrow(tweets))
  tweets$doc_id = paste("text", tweets$doc_id, sep = "")

  tweetsTest = right_join(tweets, emo_lex_senti_freq)

  ll = readRDS("data/EA_accounts.rds")

  tweets_no_ea = tweets_primary_df %>% anti_join(ll, by = "user_username")
  ea = as.data.frame(
    c(
      "EnvAgency",
      "EnvAgencyYNE",
      "EnvAgencyNW",
      "EnvAgencySE",
      "EnvAgencySW",
      "EnvAgencyMids",
      "EnvAgencyAnglia"
    )
  )
  colnames(ea) = "user_username"
  tweets_no_ea_1 = tweets_no_ea %>% anti_join(ea, by = "user_username")
  write_rds(tweets_no_ea_1, "data/primary_dataframe.rds")
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
