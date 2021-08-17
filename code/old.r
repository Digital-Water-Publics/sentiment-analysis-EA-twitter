# OLD ---------------------------------------------------------------------

# #Clean tweets for sentiment
# remove_known_ea_accounts = function(){
#   ea_accounts = readRDS("data/EA_accounts.rds")
#   all_tweets = read.csv("data/ea_mentions_all.csv")
#   
#   df_no_ea = all_tweets %>% anti_join(ea_accounts)
#   df_no_ea$clean_tweet = clean_tweets_sentiment(df_no_ea$text)
#   
#   df_no_ea$time = substr(df_no_ea$created_at, start = 12, stop = 19)
#   df_no_ea$date = substr(df_no_ea$created_at, start = 1, stop = 10)
#   df_no_ea$date = as.Date(df_no_ea$date)
#   
#   df_no_ea$document = seq.int(nrow(df_no_ea))
#   df_no_ea$year = substr(df_no_ea$created_at,1,4)
#   saveRDS(no_ea_df_senti,"data/removed_ea_accounts_all.RDS")
#   
#   
# }
# 
# 

# if(file.exists("data/primary_dataframe.rds")) {
#   tweets_primary_df = readRDS("data/primary_dataframe.rds")
#   #Turn sentiment NA's to 0
#   tweets_primary_df = tweets_primary_df %>% mutate_at(vars(anger, anticipation, disgust, fear, joy, sadness, surprise, trust),~ replace_na(.,0))
#   # Create seperate date column 
#   tweets_primary_df$time = substr(tweets_primary_df$created_at, start = 12, stop = 19)
#   tweets_primary_df$date = substr(tweets_primary_df$created_at, start = 1, stop = 10)
#   tweets_primary_df$date = as.Date(tweets_primary_df$date)
#   
#   tweets_primary_df$document = seq.int(nrow(tweets_primary_df))
# } else {
#   #Clean tweets
#   #tweets = tweets_no_ea$word %>% clean_tweets_sentiment()
#   #name tweets and merge with NRC
#   tweets = read.csv("data/ea_mentions_all.csv")
#   tweets = tweets %>% rename(word = text)
#   #nrc = tweets %>%
#   # inner_join(get_sentiments("nrc"), by = "word")
#   #Create table
#   #table = as.data.frame(table(nrc$sentiment))
# 
#   #Read parsed tweets with stop words
#   twee = read.csv("data/parsed_tweets_POS_stop_words.csv")
# 
#   #Join data with NRC lex
#   twee_sub = twee %>%
#     rename(word = token) %>%
#     inner_join(get_sentiments("nrc"), by = "word")
# 
#   #Group emo-lex trigger words
#   emo_lex_trigger_freq = twee_sub %>%
#     group_by(doc_id, word) %>%
#     summarise(count = n()) %>%
#     select(doc_id, word)
# 
#   # Convert words to matrix
#   o = split(emo_lex_trigger_freq$word, emo_lex_trigger_freq$doc_id)
# 
#   o = strsplit(as.character(o), "_")
# 
#   kl = cbind(o)
#   kl = as.data.frame(kl)
# 
#   df = tibble::rownames_to_column(kl, "doc_id")
#   df = as.data.frame(df)
#   df$doc_id = as.numeric(df$doc_id)
# 
#   tweets$doc_id = seq.int(nrow(tweets))
#   #text = "text"
#   #tweets$doc_id = paste(text,tweets$index, sep = "")
# 
#   tweets = left_join(tweets, df) %>% rename(emo_lex_trigger = o)
#   #tweetsTest$sent_score = tweetsTest$senti$sentiment
#   #tweetsTest = tweetsTest %>% select(-c(senti))
# 
#   saveRDS(tweetsTest, file = "data/primary_dataframe.rds")
#   readRDS("data/primary_dataframe.rds")
# 
#   #Group emo-lex sentiment freq
#   emo_lex_senti_freq = twee_sub %>%
#     group_by(doc_id, sentiment) %>%
#     summarise(count = n()) %>%
#     pivot_wider(names_from = sentiment, values_from = count)
# 
# 
#   #Paste doc_id to column
#   tweets$doc_id = seq.int(nrow(tweets))
#   tweets$doc_id = paste("text", tweets$doc_id, sep = "")
# 
#   tweetsTest = right_join(tweets, emo_lex_senti_freq)
# 
#   ll = readRDS("data/EA_accounts.rds")
# 
#   tweets_no_ea = tweets_primary_df %>% anti_join(ll, by = "user_username")
#   ea = as.data.frame(
#     c(
#       "EnvAgency",
#       "EnvAgencyYNE",
#       "EnvAgencyNW",
#       "EnvAgencySE",
#       "EnvAgencySW",
#       "EnvAgencyMids",
#       "EnvAgencyAnglia"
#     )
#   )
#   colnames(ea) = "user_username"
#   tweets_no_ea_1 = tweets_no_ea %>% anti_join(ea, by = "user_username")
#   write_rds(no_ea_df_senti, "data/primary_dataframe.rds")
# }
# 
# senti_polarity_tweets = function(tweets){
#   #Calculate tweet polarity sentiment
#   tweets$senti = sentiment(
#     tweets$text,
#     senti_dt = lexicon::hash_sentiment_nrc,
#     emojis_dt = lexicon::hash_sentiment_emojis,
#     hyphen = "",
#     amplifier.weight = 0.8,
#     n.before = Inf,
#     n.after = Inf,
#     question.weight = 1,
#     adversative.weight = 0.25,
#     neutral.nonverb.like = FALSE,
#     missing_value = 0
#   )
# }
# 
# sentiment_history = tweets_primary_df %>%
#   group_by(date) %>%
#   summarise(across(anticipation:joy, ~ sum(.x, na.rm = FALSE))) %>%
#   select(-c(positive, negative)) 
# run = FALSE
# if(run){
#   cols_to_plot = c("anger","anticipation","fear","sadness","disgust","surprise","trust","joy")
#   for (i in seq_along(cols_to_plot)) {
#     p = ggplot(ooo, aes(x = date, y = joy)) +
#       geom_line() +
#       xlab("")
#     
#     p + scale_x_date(date_breaks = "1 month", date_labels = "%b")
#     plots_folder = "plots/"
#     ggsave(paste(plots_folder, "joy", "_historical.png", sep = ""),width = 20)
#   }
#   
#   sentiment_history_nodate = sentiment_history %>% select(-c(date))
#   M = cor(sentiment_history_nodate)
#   corrplot(M, method = 'square', diag = FALSE, order = 'hclust', hclust.method = "ward.D2",
#            addrect = 2, rect.col = 'black', rect.lwd = 3, tl.pos = 'd', bg = "gold2") 
#   
#   # ooo %>% tidyr::gather("id", "value", 2:9) %>%
#   #   ggplot(., aes(date, value)) +
#   #   geom_line() +
#   #   facet_wrap( ~ id)
# } else {
#   print("not now")
# }