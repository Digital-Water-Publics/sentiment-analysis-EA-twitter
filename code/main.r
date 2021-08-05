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
df_no_ea$year = substr(df_no_ea$created_at,1,4)
#Calculate polarity of tweets
df_no_ea$senti = sentiment(
  df_no_ea$clean_tweet,
  polarity_dt = lexicon::hash_sentiment_senticnet,
  hyphen = " ",
  amplifier.weight = 0.8,
  n.before = Inf,
  n.after = Inf,
  question.weight = 1,
  adversative.weight = 0.25,
  neutral.nonverb.like = TRUE,
  missing_value = 0
)
# Get NRC emo-lex data
nrc_data = lexicon::nrc_emotions %>%  
  gather("sentiment", "flag", anger:trust, -term) %>% 
  filter(flag==1)
### pull emotion words and aggregate by document and emotion terms
emotions = df_no_ea %>% 
  unnest_tokens(word, clean_tweet) %>%                           
  anti_join(stop_words, by = "word") %>%                  
  filter(!grepl('[0-9]', word)) %>%
  inner_join(nrc_data, by=c("word"="term"))  %>%
  group_by(document, sentiment) %>%
  summarize( freq = n()) %>%
  ungroup() 
emotions = emotions %>%
  pivot_wider(emotions,id_cols = document,names_from = sentiment,values_from = freq)
#Merge emotions with dataset
df_no_ea = left_join(df_no_ea,emotions)
emotions = emotions %>% mutate_at(vars(anger, anticipation, disgust, fear, joy, sadness, surprise, trust),~ replace_na(.,0))


