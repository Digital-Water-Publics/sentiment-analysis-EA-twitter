# for first use run: spacy_install()
# & spacy_initialize()
#sample of data
sample_data = sample_n(tweets, 1)
pos_emotions = c("positive", "trust", "joy", "surprise")
neg_emotions = c("anger",
                 "fear",
                 "sadness",
                 "disgust",
                 "negative",
                 "anticipation")

tweets = read.csv("data/ea_mentions_2017_2021.csv")
tweets$text = tweets$text %>% clean_tweets_sentiment()

#anger, fear, anticipation, trust, surprise, sadness, joy, and disgust
#Create entity string dataframe
entity_sentiment_string <- function(data) {
  out <- tryCatch({
    #Get sentiment polarity for tweet
    sentiment_polarity = analyzeSentiment(sample_data$text)
    
    #Parse tweet
    parsedtxt = spacy_parse(
      sample_data$word,
      pos = TRUE,
      lemma = TRUE,
      dependency = TRUE,
      nounphrase = TRUE,
      multithread = TRUE
    ) %>%
      rename(word = token) %>%
      left_join(get_sentiments("nrc")) %>%
      filter(entity != "TIME_B", entity != "TIME_I")
    
    sample_data = sample_data %>%
      left_join(get_sentiments("nrc"))
    
    adj = as.vector(filter(parsedtxt$pos))
    t =  sentiment(
      sample_data$word,
      senti_dt = lexicon::hash_sentiment_nrc,
      emojis_dt = lexicon::hash_sentiment_emojis,
      hyphen = "",
      amplifier.weight = 0.8,
      n.before = Inf,
      n.after = Inf,
      question.weight = 1,
      adversative.weight = 0.25,
      neutral.nonverb.like = FALSE,
      missing_value = 0
    )
    
    senti_triggers = filter(parsedtxt, sentiment != is.na(sentiment)) %>% select(word)
    unique(senti_triggers)
    # Condition to catch if tweet has no emo-lex association
    suppressWarnings(if (all(is.na(parsedtxt$sentiment))) {
      message("tweet doesn't contain emo-lex")
      message("calculating sentiment polarity instead")
    } else {
      message("tweet contains sentiment emo-lex")
      # Get sentiment with largest count
      emotion =  names(table(parsedtxt$sentiment))[as.vector(table(parsedtxt$sentiment)) ==
                                                     max(table(parsedtxt$sentiment))]
      #Pasting settings
      emotion = paste(emotion, collapse = " OR ")
      emotion_direction = " @"
      emotion_phrase = paste(emotion, emotion_direction, sep = "")
      message("calculating sentiment at entity level")
      #Extract unique noun phrases and merge into DF
      nounphrase_entity = spacy_extract_nounphrases(sample_data$word, output = "data.frame")
      nounphrase_entity = names(table(nounphrase_entity$text))[as.vector(table(nounphrase_entity$text)) ==
                                                                 unique(table(nounphrase_entity$text))]
      nounphrase_entity_df = as.data.frame(nounphrase_entity)
      
      #Loop of DF to paste senitmnet direction
      for (i in 1:nrow(nounphrase_entity_df)) {
        nounphrase_entity_df$nounphrase_entity[i] = paste(emotion_phrase,
                                                          nounphrase_entity_df$nounphrase_entity[i])
      }
      
      token = data.frame(text = tweet, stringsAsFactors = FALSE) %>%
        unnest_tokens(word, text)
      #match sentiment words from the 'NRC' sentiment lexicon
      senti = inner_join(token, get_sentiments("nrc"))
    })
    
    
  },
  error = function(cond) {
    message(paste("Data has no nounphrase:", data))
    message(cond)
    # Choose a return value in case of error
    return(NA)
  },
  warning = function(cond) {
    message("Here's the original warning message:")
    message(cond)
    # Choose a return value in case of warning
    return(NULL)
  },
  finally = {
    spacy_finalize()
    message("Terminating spacyr session")
  })
  return(out)
}
## TEST entity_sentiment_string(data = tweets_2021_single)

# Logical rules for entity
# 1. Merge sentiment with tweet token
# 2. Apply nounphrase method
#----------------------------------------------------------------------
# IF beg_root & PRON = entity [IF results > 1 !include entity = DATE]
# ELSE nounphrase max(length)
# IF NULL use polarity translation e.g. "negative @ supply network" ? @helge
#----------------------------------------------------------------------
# 3. Add new column string e.g. "angry @ supply network"
# 4. Add polarity sentiment score
# 5. Export


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


o = split(emo_lex_trigger_freq$word,emo_lex_trigger_freq$doc_id)
  o = strsplit(as.character(o), "_")
as.data.frame(o)

op = cbind(o) 

df <- tibble::rownames_to_column(kl, "doc_id")

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
