########################### Steps
######################################################
########################### Get sample of tweets
################### ###################################
########################### Parse tweets
######################################################
########################### Filter words by noun
########################### Join with emo_lex
########################### Create n_gram based on emo_lex_trigger
########################### Calculate senitment of phrase
########################### Calculate
########################### Join with emo_lex
########################### Create n_gram based on emo_lex_trigger
########################### Calculate senitment of phrase
neg = data.frame(sentiment = c("anger","sadness","disgust","fear","anticpation"))
pos = data.frame(sentiment = c("joy", "trust", "surprise"))

sample = sample_n(tweets_primary_df, 1)
sample_tweets = sample_n(tweets_primary_df, 500)

for (i in 1:nrow(sample_tweets)) {
  
}
#Parse tweet and filter by noun and emo-lex
o = spacy_parse(sample$word, pos = TRUE, nounphrase = TRUE) %>%
  unnest_tokens(word, token) %>%
  dplyr::filter(pos == "NOUN") %>%
  inner_join(get_sentiments("nrc")) %>%
  filter(sentiment != "positive") %>%
  filter(sentiment != "negative")

# Collect senti results
senti = as.data.frame(unique(o$sentiment)) %>%
  rename("sentiment" = "unique(o$sentiment)")

# Use emo_lex trigger as key for ngram
word = unique(o$word)
bigrams <- sample %>%
  unnest_tokens(bigram, word, token = "ngrams", n = 4)  %>%
  separate(bigram, c("word1", "word2", "word3", "word4"), sep = " ") %>%
  filter(word1 == word) %>%
  count(word1, word2, word3, word4, sort = TRUE)

#Calculate polarit score
polarity = sentiment(
  string,
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
#Filter emotional sentiment by emo lex
if (nrow(senti) > 1) {
  print("Sentiment includes greater than 1 emo_lex")
  if (polarity$sentiment > 0.6) {
    main_senti = senti %>% inner_join(pos)
    main_senti = paste(main_senti$sentiment, collapse = "/ ")
  } else {
    main_senti = senti %>% inner_join(neg)
    main_senti = paste(main_senti$sentiment, collapse = "/ ")
  }
} else {
  print("Sentiment contains only 1 emo_lex")
  main_senti = senti
}

string = paste0(main_senti,sep = " - ", bigrams$word1,sep=" ", bigrams$word2, sep=" ", bigrams$word3, sep=" ", bigrams$word4)
