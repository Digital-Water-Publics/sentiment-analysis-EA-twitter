# Calculate word freq of entire corpus of tweets
word_freq = tweets_text_single %>%
  unnest_tokens(text, text) %>%
  count(text, sort = TRUE)

#Write csv
#write.csv(word_freq, "data/word_freq.csv")
