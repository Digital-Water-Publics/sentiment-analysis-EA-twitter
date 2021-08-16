dd_tt$clean_tweet = clean_tweets_sentiment(dd_tt$text)
# Create temportal and doc_id flags
dd_tt$time = substr(dd_tt$created_at, start = 12, stop = 19)
dd_tt$date = substr(dd_tt$created_at, start = 1, stop = 10)
dd_tt$date = as.Date(dd_tt$date)
dd_tt$document = seq.int(nrow(dd_tt))
dd_tt$year = substr(dd_tt$created_at, 1, 4)

emotions = dd_tt %>%
  unnest_tokens(word, clean_tweet) %>%
  anti_join(stop_words, by = "word") %>%
  filter(!grepl('[0-9]', word)) %>%
  inner_join(nrc_data, by = c("word" = "term"))  %>%
  group_by(document, sentiment) %>%
  summarize(freq = n()) %>%
  ungroup()

#Create wider table of emotions
emotions = emotions %>%
  pivot_wider(values_from = freq,
              names_from = sentiment)
#Replace NAs as 0 
emotions = emotions %>% mutate_at(
  vars(anger, anticipation, disgust, fear, joy, sadness, surprise, trust),
  ~ replace_na(., 0)
)
#Merge emotions to df
dd_tt = left_join(dd_tt, emotions)

dd_tt = dd_tt %>% mutate_at(
  vars(anger, anticipation, disgust, fear, joy, sadness, surprise, trust),
  ~ replace_na(., 0)
)
saveRDS(dd_tt,"data/tweets_june-july.RDS")
june_tweets = readRDS("data/tweets_june-july.RDS")


#Calculate polarity of tweets
dd_tt$senti_nrc = sentiment(
  dd_tt$clean_tweet,
  polarity_dt =lexicon::hash_sentiment_sentiword,
  hyphen = " ",
  amplifier.weight = 0.8,
  n.before = Inf,
  n.after = Inf,
  question.weight = 1,
  adversative.weight = 0.5,
  neutral.nonverb.like = TRUE,
  missing_value = 0
)
