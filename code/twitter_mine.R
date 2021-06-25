# Twitter key ------------------------------------------------------------
bearer_token = "AAAAAAAAAAAAAAAAAAAAAMNaJwEAAAAAJt1NQBowbyVGaYV%2Bl7vAjpOz2Es%3DEu27lmCWSm9kZhWdEUlczZsvVAZRbDj4buLuR5tbttjr6BGzPY"
# Twitter Mine ------------------------------------------------------------
tweets = get_all_tweets(
    "@EnvAgency OR @EnvAgencyYNE OR @EnvAgencyNW OR @EnvAgencySE OR @EnvAgencySW OR @envagencymids OR @EnvAgencyAnglia lang:en -is:retweet",
    "2021-01-01T00:00:00Z",
    "2021-01-05T00:00:00Z",
    bearer_token,
    data_path = "data/",
    bind_tweets = TRUE
)
# Clean data ------------------------------------------------------------
tweets_text = as.data.frame(
    subset(tweets,
         select = c("text","author_id","possibly_sensitive","created_at"))
  )

tt = as.data.frame(
  subset(tweets,
         select = c("text"))
)

