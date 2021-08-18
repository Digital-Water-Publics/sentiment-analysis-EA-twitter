# OLD ---------------------------------------------------------------------
# Create dataframe containing all sentiment scores ------------------------
# mock df
# TODO to look at adding more computationally linguistics at the entity level
# df = data.frame (
#   text = c("this is a disgusting"),
#   word_count = 20,
#   emo_lex = c("disgusting"),
#   emotion = c("disgust"),
#   key_emotion = c("disgust"),
#   av_sentiment_score = 0,
#   SentimentGI = 0,
#   NegativityGI = 0,
#   PositivityGI = 0,
#   SentimentHE = 0,
#   NegativityHE = 0,
#   PositivityHE = 0,
#   SentimentLM = 0,
#   NegativityLM = 0,
#   PositivityLM = 0,
#   RatioUncertaintyLM = 0,
#   SentimentQDAP = 0,
#   NegativityQDAP = 0,
#   PositivityQDAP = 0
# )
# #Start counter
# counter = 0
# tweet_vector = as.vector(t(sample_data$text))
# 
# #Function to create senti results
# senti_df = function(tweets) {
#   #Set first tweet
#   text = sample_data$text[i]
#   text = as.data.frame(text)
#   #tokenize text
#   token = data.frame(text = text, stringsAsFactors = FALSE) %>%
#     unnest_tokens(word, text)
#   #match sentiment words from the 'NRC' sentiment lexicon
#   senti = inner_join(token, get_sentiments("nrc"))
#   #save specific words in tweets with emotional lex pair
#   emo_words = as.character(unique(senti$word))
#   st = paste(emo_words, collapse = ", ")
#   text$emo_lex = st
#   #save specific emotion based on word
#   emotion = as.character(unique(senti$sentiment))
#   et = paste(emotion, collapse = " ")
#   text$emotion = et
#   #save key/most frequent emotions
#   x = c(senti$sentiment)
#   tt = table(x)
#   names = names(tt[tt == max(tt)])
#   nt = paste(names, collapse = " ")
#   text$key_emotion = nt
#   #word count
#   text$word_count = sentiment$WordCount
#   #Calculate various sentiment scores
#   sentiment_support = sentiment_by(get_sentences(tweets[i]))
#   text$av_sentiment_score = sentiment_support$ave_sentiment
#   sentiment = analyzeSentiment(tweets[i])
#   text$SentimentGI = sentiment$SentimentGI
#   text$NegativityGI = sentiment$NegativityGI
#   text$PositivityGI = sentiment$PositivityGI
#   text$SentimentHE = sentiment$SentimentHE
#   text$NegativityHE = sentiment$NegativityHE
#   text$PositivityHE = sentiment$PositivityHE
#   text$SentimentLM = sentiment$SentimentLM
#   text$NegativityLM = sentiment$NegativityLM
#   text$PositivityLM = sentiment$PositivityLM
#   text$RatioUncertaintyLM = sentiment$RatioUncertaintyLM
#   text$SentimentQDAP = sentiment$SentimentQDAP
#   text$NegativityQDAP = sentiment$NegativityQDAP
#   text$PositivityQDAP = sentiment$PositivityQDAP
#   #Update and print counter
#   counter <<- counter + 1
#   print(counter)
#   #Bind DF
#   df <<- rbind(text, df)
# }
# ##Loop function based on number of rows
# for (i in 1:nrow(sample_data)){senti_df(tweets = sample_data$text)}
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
