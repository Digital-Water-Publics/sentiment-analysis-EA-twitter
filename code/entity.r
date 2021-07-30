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

tweets = read.csv("data/ea_mentions_all.csv")
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
      sample$word,
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
#k =5 =6 get topics 3 and 4
tweets_primary_df_topic3 = tweets_primary_df %>% filter(`key_topic topic(k=5)` == 3) %>% select(word)
sample5_3 = sample_n(tweets_primary_df_topic3,500)
kable(sample5_3) %>% kableExtra::kable_material_dark()

tweets_primary_df_topic4 = tweets_primary_df %>% filter(`key_topic topic(k=5)` == 4) %>% select(word)
sample5_4 = sample_n(tweets_primary_df_topic4,500)
kable(sample5_4) %>% kableExtra::kable_material_dark()

tweets_primary_df_topic6 = tweets_primary_df %>% filter(`key_topic topic(k=6)` == 3) %>% select(word)
sample6_3 = sample_n(tweets_primary_df_topic6, 500)
kable(sample6_3) %>% kableExtra::kable_material_dark()

tweets_primary_df_topic7 = tweets_primary_df %>% filter(`key_topic topic(k=6)` == 4) %>% select(word)
sample6_4 = sample_n(tweets_primary_df_topic7,500)
kable(sample6_4) %>% kableExtra::kable_material_dark()

