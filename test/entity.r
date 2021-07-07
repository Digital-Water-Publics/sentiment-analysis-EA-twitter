# for first use run: spacy_install()
# & spacy_initialize()
#sample of data
sample_data = sample_n(tweets_text_single,
                       1)

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
st = paste(emotion, collapse = ", ")
parsedtxt$key_emotion = st
t = spacy_extract_nounphrases(sample_data$text, output = "data.frame")

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
