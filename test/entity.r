# for first use run: spacy_install()
# & spacy_initialize()
#sample of data
sample_data = sample_n(tweets_text_single,1)

sample_data = as.data.frame(sample_data %>% clean_tweets_sentiment()) %>% rename(text = "sample_data %>% clean_tweets_sentiment()")
  
#Parse text and calculate computational linguistics using spacy.io API
parsedtxt = spacy_parse(
  sample_data$text,
  pos = TRUE,
  tag = TRUE,
  lemma = TRUE,
  entity = TRUE,
  dependency = TRUE,
  nounphrase = TRUE,
  multithread = TRUE
) %>%
  rename(word = token) %>%
  left_join(get_sentiments("nrc"))

# Get sentiment with largest count
emotion =  names(table(parsedtxt$sentiment))[as.vector(table(parsedtxt$sentiment)) ==
                                               max(table(parsedtxt$sentiment))]
emotion = paste(emotion, collapse = " OR ")

emotion_direction = " @"

emotion_phrase = paste(emotion,emotion_direction, sep = "")

nounphrase_entity = spacy_extract_nounphrases(sample_data$text, output = "data.frame")

nounphrase_entity = names(table(nounphrase_entity$text))[as.vector(table(nounphrase_entity$text)) == 
                            unique(table(nounphrase_entity$text))]

nounphrase_entity_df = as.data.frame(nounphrase_entity)

for (i in 1:nrow(nounphrase_entity_df)){
   nounphrase_entity_df$nounphrase_entity[i] = paste(emotion_phrase, nounphrase_entity_df$nounphrase_entity[i])
}


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
