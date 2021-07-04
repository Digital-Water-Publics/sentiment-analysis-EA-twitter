if (file.exists("data/tweet_sentiment_scores.csv")) {
  tweets = read.csv("data/tweet_sentiment_scores.csv")
} else {
  # Twitter Mine ------------------------------------------------------------
  tweets = get_all_tweets(
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
    start_tweets = "2020-06-01T00:00:00Z",
    end_tweets = "2021-06-01T00:00:00Z",
    bearer_token = get_bearer(),
    data_path = "data6/",
    bind_tweets = TRUE,
    context_annotations = FALSE,
    page_n = 500,
    n = Inf
  )
  
  # Clean data ------------------------------------------------------------
  tweets_text = as.data.frame(subset(
    tweets,
    select = c("text", "author_id", "possibly_sensitive", "created_at")
  ))
  # Write csv --------------------------------------------------------------
  write.csv(tweets_text, "data/tweet_sentiment_scores.csv")
}
