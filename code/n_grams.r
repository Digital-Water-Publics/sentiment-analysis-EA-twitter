# Split corpus into 2 ngrams ----------------------------------------------
if (file.exists("data/bigrams_filtered.RDS")) {
  bigrams_filtered = readRDS("data/bigrams_filtered.RDS")
} else {
  ntweet_bigrams = tweets_primary_df %>%
    unnest_tokens(biagram, word, token = "ngrams", n = 3)
  
  ntweet_bigrams_test =  ntweet_bigrams %>% filter(str_detect(biagram, "water industry"))
  
  bigrams_separated = ntweet_bigrams %>%
    separate(biagram, c("word1", "word2", "word3"), sep = " ")
  
  bigrams_filtered = bigrams_separated %>%
    filter(!word1 %in% stop_words$word) %>%
    filter(!word2 %in% stop_words$word)
  
  saveRDS(bigrams_filtered, "data/bigrams_filtered.RDS")
}
# Create function to show ngram frequency ---------------------------------

bigram_plots = function(word) {
  plots_folder = "plots/"
  first_word = "1st_word"
  second_word = "2nd_word"
  
  #word n_gram as first place
  word_graph_word_1 = bigrams_filtered %>%
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
  word_graph_word_2 = bigrams_filtered %>%
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

bigram_plots(word = "river")
bigram_plots(word = "flood")
bigram_plots(word = "water")

bigram_plots(word = "waste")
bigram_plots(word = "pollution")
bigram_plots(word = "sewage")

