# for first use run: spacy_install()
# & spacy_initialize()

#sample of data
sample_data = sample_n(tweets_text_single,
                       1)

#Parse text and calculate computational linguistics using spacy.io API
parsedtxt = spacy_parse(
  a$`sample_data$text %>% clean_tweets()`,
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
emotion = names(table(parsedtxt$sentiment))[as.vector(table(parsedtxt$sentiment)) ==
                                              max(table(parsedtxt$sentiment))]

t = spacy_extract_nounphrases(a$`sample_data$text %>% clean_tweets()`, output = "list")



# Logical rules for entity
# 1. Calculate key sentiment (sentiment with the largest number of counts)
# 2. Get the nounphrase
# 3. Add the key sentiment, @ and the  nounphrase to create string e.g. "angry @ supply network"
# 4. Add polarity sentiment score
#
