if(file.exists("data/primary_dataframe.rds")){
  tweets_primary_df = readRDS("data/primary_dataframe.rds")
} else {
  #Clean tweets
  tweets = tweets$word %>% clean_tweets_sentiment()
  #name tweets and merge with NRC
  tweets = tweets %>% rename(word = text)
  nrc = tweets %>%
    inner_join(get_sentiments("nrc"), by = "word")
  #Create table
  table = as.data.frame(table(nrc$sentiment))
  
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
    select(doc_id,word)
  
  # Convert words to matrix
  o = split(emo_lex_trigger_freq$word,emo_lex_trigger_freq$doc_id)
  
  o = strsplit(as.character(o), "_")
  
  kl = cbind(o) 
  
  df = tibble::rownames_to_column(kl, "doc_id")
  
  tweetsTest = right_join(tweetsTest,df) %>% rename(emo_lex_trigger = o)
  tweetsTest$sent_score = tweetsTest$senti$sentiment
  tweetsTest = tweetsTest %>% select(-c(senti))
  
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
  
  tweetsTest = right_join(tweets,emo_lex_senti_freq)
}

users = tweets_primary_df %>%
  group_by(user_username) %>%
  summarise(count = n()) %>%
  arran
