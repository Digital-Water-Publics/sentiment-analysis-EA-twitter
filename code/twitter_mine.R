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
  start_tweets = "2017-06-01T00:00:00Z", #TODO discuss with Helge - should this go back to 2016?
  end_tweets = "2022-02-01T00:00:00Z",
  bearer_token = get_bearer(),
  data_path = "data6/",
  bind_tweets = TRUE,
  context_annotations = FALSE,
  page_n = 500,
  n = Inf
)

# Clean and write csv --------------------------------------------------------------
dd_tt = bind_tweets(data_path = "data6/", output_format = "tidy")
write.csv(dd_tt, "data/ea_mentions_2017_2021_new.csv")
