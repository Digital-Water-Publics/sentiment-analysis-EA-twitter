# Calculate ngrams for single words----------------------------------------------
if (file.exists("data/bigrams_filtered.RDS")) {
  bigrams_filtered = readRDS("data/bigrams_filtered.RDS")
} else {
  ntweet_bigrams = tweets_primary_df %>%
    unnest_tokens(biagram, word, token = "ngrams", n = 2)
  
  bigrams_separated = ntweet_bigrams %>%
    separate(biagram, c("word1", "word2"), sep = " ")
  
  bigrams_filtered = bigrams_separated %>%
    filter(!word1 %in% stop_words$word) %>%
    filter(!word2 %in% stop_words$word)
  
  saveRDS(bigrams_filtered, "data/bigrams_filtered.RDS")
}

# Calculate ngrams for nounphrases ----------------------------------------
nounphrase = as.data.frame(c(
  "water industry",
  "water companies",
  "raw sewage",
  "water quality"
))
colnames(nounphrase) = "nounphrase"
for (i in 1:nrow(nounphrase)) {
  nounphrase$phrasetoken[i] = str_replace_all(nounphrase$nounphrase[i], fixed(" "), "")
}

if (file.exists("data/bigrams_filtered_nounphrases.RDS")) {
  bigrams_filtered_3 = readRDS("data/bigrams_filtered_nounphrases.RDS")
} else {
  #create temp df and replace all nounphrases with thier squished tokens
  n_gram_df = tweets_primary_df
  for (i in 1:nrow(nounphrase)) {
    n_gram_df$word = str_replace_all(n_gram_df$word,
                                     nounphrase$nounphrase[i],
                                     nounphrase$phrasetoken[i])
  }
  ntweet_bigrams_nounphrase = n_gram_df %>%
    unnest_tokens(biagram, word, token = "ngrams", n = 2)
  
  bigrams_separated_nounphrase = ntweet_bigrams_nounphrase %>%
    separate(biagram, c("word1", "word2"), sep = " ")
  
  bigrams_separated_nounphrase = bigrams_separated_nounphrase %>%
    filter(!word1 %in% stop_words$word) %>%
    filter(!word2 %in% stop_words$word)
  
  saveRDS(bigrams_separated_nounphrase,
          "data/bigrams_filtered_nounphrases.RDS")
}

# Create function to show ngram frequency ---------------------------------
bigram_plots = function(data, word) {
  plots_folder = "plots/"
  first_word = "1st_word"
  second_word = "2nd_word"
  
  #word n_gram as first place
  word_graph_word_1 = data %>%
    filter(word1 == word) %>%
    count(word2, sort = TRUE)
  
  #Plot
  word_graph_word_1 = word_graph_word_1[(1:25), ]
  
  ggplot(word_graph_word_1, aes(x = reorder(word2, n), y = n)) +
    geom_bar(stat = "identity") +
    xlab(paste(word, first_word, sep = " ")) +
    coord_flip()
  
  ggsave(paste(plots_folder, word, "first_word.png", sep = ""))
  
  #word n_gram as second place
  word_graph_word_2 = data %>%
    filter(word2 == word) %>%
    count(word1, sort = TRUE)
  
  #Plot
  word_graph_word_2 = word_graph_word_2[(1:25), ]
  ggplot(word_graph_word_2, aes(x = reorder(word1, n), y = n)) +
    geom_bar(stat = "identity") +
    xlab(paste(word, second_word, sep = " ")) +
    coord_flip()
  
  ggsave(paste(plots_folder, word, "second_word.png", sep = ""))
}


