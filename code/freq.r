word_freq = tweets_text_single %>%
  unnest_tokens(text, text) %>%
  count(text, sort = TRUE)
