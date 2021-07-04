if (file.exists("data/tweet_sentiment_scores.csv")) {
  tweets = read.csv("data/tweet_sentiment_scores.csv")
} else {
  # Twitter Mine ------------------------------------------------------------
  tweets2019 = get_all_tweets(
    query = c(
      "EnvAgency",
      "EnvAgencyYNE",
      "EnvAgencyNW",
      "EnvAgencySE",
      "EnvAgencySW",
      "envagencymids",
      "EnvAgencyAnglia"
    ),
    is_retweet = FALSE,
    lang = "en",
    start_tweets = "2019-06-01T00:00:00Z",
    end_tweets = "2020-06-01T00:00:00Z",
    bearer_token = get_bearer(),
    data_path = "data6/",
    bind_tweets = TRUE,
    context_annotations = FALSE,
    page_n = 500,
    n = Inf
  )
  # Clean data ------------------------------------------------------------
  tweets2019 %>% select(
    text,
    author_id,
    in_reply_to_user_id,
    conversation_id,
    possibly_sensitive,
    created_at,
    public_metrics,
    geo
  ) %>% rename(
    
  )
    
    
    
    as.data.frame(subset(
    tweets2019,
    select = c(
      "text",
      "author_id",
      "in_reply_to_user_id",
      "conversation_id",
      "possibly_sensitive",
      "created_at",
      "public_metrics",
      "geo"
    )
  )) %>%
    
  
  tweets2019 %>%
    rename(
      tweets2019, 
    )
  names(tweets_text)<-make.names(names(tweets_text))
  names(tweets_text1)<-make.names(names(tweets_text1))
  tt1 = rbind(tweets_text,tweets_text1)
  # Write csv --------------------------------------------------------------
  write.csv(tt, "data/tweet_sentiment_scores_2019-2021.csv")
}
a = read.csv("tweet_sentiment_scores_2019-2021.csv")
