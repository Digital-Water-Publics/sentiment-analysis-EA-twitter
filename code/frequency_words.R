# Word frequency ----------------------------------------------------------
#Set up Spacy
spacy_initialize()


#Subset to only include tweet
if (file.exists("data/parsed_tweets_POS_stop_words.csv")) {
  text = read.csv("data/parsed_tweets_POS_stop_words.csv")
} else {
  text = tweets_no_ea %>% select(text) %>% anti_join(stop_words, by = c("text" = "word"))
  write.csv(text, "data/parsed_tweets_POS_stop_words.csv")
  }

if (file.exists("data/parsed_tweets_POS.csv")) {
  #Read parsed text csv
  parsedtxt = read.csv("data/parsed_tweets_POS.csv")
} else {
  #Parse tweets into dataframe
  parsedtxt = spacy_parse(
    tweets_primary_df$word,
    pos = TRUE,
    tag = TRUE,
    lemma = TRUE,
    entity = TRUE,
    dependency = TRUE,
    nounphrase = TRUE,
    multithread = TRUE
  )
  #Write csv
  write.csv(parsedtxt, "data/parsed_tweets_POS_stop_words.csv")
}
# Frequency analysis for nouns --------------------------------------------
if (file.exists("data/noun_freq.csv")) {
  noun_freq = read.csv("data/noun_freq.csv")
  nounFreq_30 = noun_freq[(1:30), ]

  n = ggplot(nounFreq_30, aes(x = reorder(lemma, n), y = n)) +
    geom_bar(stat = "identity") +
    coord_flip()
} else {
  parsed_sub = parsedtxt %>%
    filter(pos == "NOUN") %>%
    select(lemma) %>%
    mutate(lemma = str_remove_all(lemma, regex(" ?(f&ht)(tp)(s?)(://)(.*)[.&/](.*)"))) %>%
    mutate(lemma = str_remove_all(lemma, regex("@[[:alnum:]_]{4,}"))) %>%
    mutate(lemma = str_remove_all(lemma, regex("#[[:alnum:]_]+"))) %>%
    mutate(lemma = str_remove_all(lemma, regex("[[:punct:]]"))) %>%
    mutate(lemma = str_remove_all(lemma, regex("^RT:? "))) %>%
    mutate(lemma = str_replace(lemma, "amp", "and")) %>%
    anti_join(stop_words, by = c("lemma" = "word")) %>%
    mutate(lemma = str_to_lower(lemma)) %>%
    count(lemma) %>%
    arrange(desc(n))
  head(parsed_sub)
  #Write csv
  write.csv(parsed_sub, "data/noun_freq.csv")
}

# Frequency analysis for adjectives---------------------------------------------------------------
if (file.exists("data/adj_freq.csv")) {
  adjFreq = read.csv("data/adj_freq.csv")
  adjFreq = adjFreq[(1:30), ]

  ggplot(adjFreq, aes(x = reorder(lemma, n), y = n)) +
    geom_bar(stat = "identity") +
    coord_flip()
} else {
  parsed_sub_adj = parsedtxt %>%
    filter(pos == "ADJ") %>%
    select(lemma) %>%
    mutate(lemma = str_remove_all(lemma, regex(" ?(f&ht)(tp)(s?)(://)(.*)[.&/](.*)"))) %>%
    mutate(lemma = str_remove_all(lemma, regex("@[[:alnum:]_]{4,}"))) %>%
    mutate(lemma = str_remove_all(lemma, regex("#[[:alnum:]_]+"))) %>%
    mutate(lemma = str_remove_all(lemma, regex("[[:punct:]]"))) %>%
    mutate(lemma = str_remove_all(lemma, regex("^RT:? "))) %>%
    mutate(lemma = str_replace(lemma, "amp", "and")) %>%
    anti_join(stop_words, by = c("lemma" = "word")) %>%
    mutate(lemma = str_to_lower(lemma)) %>%
    count(lemma) %>%
    arrange(desc(n))
  head(parsed_sub_adj)
  #Write csv
  write.csv(parsed_sub_adj, "data/adj_freq.csv")
}


# Extract nounphrases -----------------------------------------------------
if (file.exists("data/nounphrase_freq.csv")) {
  nounphrase_text_sub_head = nounphrase_text_sub[(1:30),]

  ggplot(nounphrase_text_sub_head, aes(x = reorder(text, n), y = n)) +
    geom_bar(stat = "identity") +
    coord_flip()
} else {
  
  nounphrase_text = spacy_extract_nounphrases(clean_tweets,
                                              output = c("data.frame"),
                                              multithread = TRUE)

  nounphrase_text_sub = nounphrase_text %>%
    filter(length > 2) %>%
    select(text) %>%
    mutate(text = str_remove_all(text, regex(" ?(f&ht)(tp)(s?)(://)(.*)[.&/](.*)"))) %>%
    mutate(text = str_remove_all(text, regex("@[[:alnum:]_]{4,}"))) %>%
    mutate(text = str_remove_all(text, regex("#[[:alnum:]_]+"))) %>%
    mutate(text = str_remove_all(text, regex("[[:punct:]]"))) %>%
    mutate(text = str_remove_all(text, regex("^RT:? "))) %>%
    mutate(text = str_replace(text, "amp", "and")) %>%
    anti_join(stop_words, by = c("text" = "word")) %>%
    mutate(text = str_to_lower(text)) %>%
    # Remove any trailing whitespace around the text
    mutate(text = str_trim(text, "both")) %>%
    count(text) %>%
    arrange(desc(n))
  #test
  head(nounphrase_text_sub)
  write.csv(nounphrase_text_sub, "data/nounphrase_freq.csv")
}


if (file.exists("data/entity_text_freq.csv")) {
  entity_text_sub = read.csv("data/entity_text_freq.csv")
  entity_type_sub = read.csv("data/entity_type_freq.csv")
} else {
  #Extract entities
  entity_text = spacy_extract_entity(clean_tweets,
                                     output = c("data.frame"),
                                     multithread = TRUE)

  entity_text_sub = entity_text %>%
    select(text) %>%
    mutate(text = str_remove_all(text, regex(" ?(f&ht)(tp)(s?)(://)(.*)[.&/](.*)"))) %>%
    mutate(text = str_remove_all(text, regex("@[[:alnum:]_]{4,}"))) %>%
    mutate(text = str_remove_all(text, regex("#[[:alnum:]_]+"))) %>%
    mutate(text = str_remove_all(text, regex("[[:punct:]]"))) %>%
    mutate(text = str_remove_all(text, regex("^RT:? "))) %>%
    mutate(text = str_replace(text, "amp", "and")) %>%
    anti_join(stop_words, by = c("text" = "word")) %>%
    mutate(text = str_to_lower(text)) %>%
    mutate(text = str_trim(text, "both")) %>%
    count(text) %>%
    arrange(desc(n))

  head(entity_text_sub)

  write.csv(entity_text_sub, "data/entity_text_freq.csv")

  entity_type_sub = entity_text %>%
    count(ent_type) %>%
    arrange(desc(n))

  write.csv(entity_type_sub, "data/entity_type_freq.csv")
}
#Termiante spacy session
spacy_finalize()

if (file.exists("data/emo_lex_freq.csv")) {
  emo_lex_freq = read.csv("data/emo_lex_freq.csv")
} else {
  tt = unlist(tweets_no_ea_1$emo_lex_trigger)
  tt = as.data.frame(tt)
  tt = as.data.frame(table(tt))
  tt = tt %>% arrange(desc(Freq))
  write.csv(tt, "data/emo_lex_freq.csv")
  tt = tt[(1:30), ]

  ggplot(tt, aes(x = reorder(tt, Freq), y = Freq)) +
    geom_bar(stat = "identity") +
    coord_flip()

}
