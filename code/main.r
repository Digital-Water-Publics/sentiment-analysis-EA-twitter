#Read dataset and EA accounts
ea_accounts = readRDS("data/EA_accounts.rds")
all_tweets = read.csv("data/ea_mentions_all.csv")
#Remove EA accounts from dataset
df_no_ea = all_tweets %>% anti_join(ea_accounts)
#Create new column of clean tweets
df_no_ea$clean_tweet = clean_tweets_sentiment(df_no_ea$text)
# Create temportal and doc_id flags
df_no_ea$time = substr(df_no_ea$created_at, start = 12, stop = 19)
df_no_ea$date = substr(df_no_ea$created_at, start = 1, stop = 10)
df_no_ea$date = as.Date(df_no_ea$date)
df_no_ea$document = seq.int(nrow(df_no_ea))
df_no_ea$year = substr(df_no_ea$created_at, 1, 4)
#Calculate polarity of tweets
df_no_ea$senti_nrc = sentiment(
  df_no_ea$clean_tweet,
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

# Get NRC emo-lex data
nrc_data = lexicon::nrc_emotions %>%
  gather("sentiment", "flag", anger:trust,-term) %>%
  filter(flag == 1)
### pull emotion words and aggregate by document and emotion terms
emotions = df_no_ea %>%
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
df_no_ea = left_join(df_no_ea, emotions)
#Replace Nas to 0
df_no_ea = df_no_ea %>% mutate_at(
  vars(anger, anticipation, disgust, fear, joy, sadness, surprise, trust),
  ~ replace_na(., 0)
)

#Subsetting tweets
trust_tweets = df_no_ea %>% filter(trust > 0)

trust_tweets$sentdirection = SentimentAnalysis::convertToBinaryResponse(trust_tweets$senti_nrc$sentiment)
table(trust_tweets$sentdirection)

trust_tweets$sentdirection = str_replace(trust_tweets$sentdirection,"positive","trust")
trust_tweets$sentdirection = str_replace(trust_tweets$sentdirection,"negative","distrust")

trust_time = trust_tweets %>% group_by(date) %>% count(sentdirection)

ggplot(trust_time,aes(x=date,y=n,)) + geom_line() +
  coord_cartesian(clip = "off") +
  guides(fill = guide_legend(label.position = "bottom", nrow = 1)) +
  facet_wrap(~sentdirection) +
  labs(
    title = "Trust vs Distrust",
    subtitle = "A comparison of polarity of trust triggered tweets",
    caption = "Data: Twitter API"
  ) +
  theme_minimal(base_size = 16, base_family = "Roboto") +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    legend.key.width = unit(10, "mm"),
    legend.key.height = unit(2, "mm"),
    legend.text = element_text(family = "Roboto Condensed", size = rel(0.6)),
    plot.title.position = "panel",
    plot.caption.position = "plot",
    plot.title = element_text(size = rel(1.9), hjust = 0.5, family = "Roboto Condensed", face = "bold"),
    plot.caption = element_text(hjust = 0.5, size = rel(0.7), color = "grey60"),
    plot.subtitle = element_text(hjust = 0.5, color = "grey60"),
    plot.background = element_rect(fill = "grey95", color = "grey95"),
    plot.margin = margin(t = 10, r = 20, b = 10),
    axis.text.y = element_text(family = "JetBrains Mono"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey70", size = 0.4, linetype = "solid"),
    panel.spacing.x = unit(2, "line"),
    panel.spacing.y = unit(1, "line")
  )

