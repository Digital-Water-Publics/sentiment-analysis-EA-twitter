# Word frequency ----------------------------------------------------------
#Read data
tweets = read.csv("data/ea_mentions_2017_2021.csv")
#Subset to only include tweet
if (file.exists("data/parsed_tweets_POS_stop_words.csv")){
  text = read.csv("data/parsed_tweets_POS_stop_words.csv")
} else {
  text = tweets %>% select(text) %>% anti_join(stop_words,by = c("text" = "word"))
}

if(file.exists("data/parsed_tweets_POS.csv")){
  #Read parsed text csv
  parsedtxt = read.csv("data/parsed_tweets_POS.csv")
} else {
  #Parse tweets into dataframe
  parsedtxt = spacy_parse(
    text$text,
    pos = TRUE,
    tag = TRUE,
    lemma = TRUE,
    entity = TRUE,
    dependency = TRUE,
    nounphrase = TRUE,
    multithread = TRUE
  )
   #Write csv
  write.csv(parsedtxt,"data/parsed_tweets_POS_stop_words.csv")
}
# Frequency analysis for nouns --------------------------------------------
if(file.exists("data/noun_freq.csv")){
  noun_freq = read.csv("data/noun_freq.csv")
  nounFreq_30 = noun_freq[(1:30),]
  
  n = ggplot(nounFreq_30, aes(x = reorder(lemma, n), y = n)) +
    geom_bar(stat = "identity") +
    coord_flip()
} else {
  parsed_sub = text %>% 
    filter(pos == "NOUN") %>% 
    select(lemma) %>% 
    mutate(lemma = str_remove_all(lemma, regex(" ?(f|ht)(tp)(s?)(://)(.*)[.|/](.*)"))) %>%
    mutate(lemma = str_remove_all(lemma, regex("@[[:alnum:]_]{4,}"))) %>%
    mutate(lemma = str_remove_all(lemma, regex("#[[:alnum:]_]+"))) %>%
    mutate(lemma = str_remove_all(lemma, regex("[[:punct:]]"))) %>%
    mutate(lemma = str_remove_all(lemma, regex("^RT:? "))) %>%
    mutate(lemma = str_replace(lemma, "amp", "and")) %>%
    anti_join(stop_words, by = c("lemma" = "word")) %>%
    mutate(lemma = str_to_lower(lemma)) %>%
    count(lemma) %>%
    arrange(desc(n))
  #Remove weird blank space which is fucking up my query - TODO add this somehow into the query
  parsed_sub = parsed_sub[-c(1),]
  #Write csv
  write.csv(parsed_sub, "data/noun_freq.csv")
}
#Set upn Spacy
spacy_initialize()
# Frequency analysis for adjectives---------------------------------------------------------------
if(file.exists("data/adj_freq.csv")){
  adjFreq = read.csv("data/adj_freq.csv")
  adjFreq = adjFreq[(1:30),]
  
  ggplot(adjFreq, aes(x = reorder(lemma, n), y = n)) +
    geom_bar(stat = "identity") +
    coord_flip()
} else {
  parsed_sub_adj = text %>% 
    filter(pos == "ADJ") %>% 
    select(lemma) %>% 
    mutate(lemma = str_remove_all(lemma, regex(" ?(f|ht)(tp)(s?)(://)(.*)[.|/](.*)"))) %>%
    mutate(lemma = str_remove_all(lemma, regex("@[[:alnum:]_]{4,}"))) %>%
    mutate(lemma = str_remove_all(lemma, regex("#[[:alnum:]_]+"))) %>%
    mutate(lemma = str_remove_all(lemma, regex("[[:punct:]]"))) %>%
    mutate(lemma = str_remove_all(lemma, regex("^RT:? "))) %>%
    mutate(lemma = str_replace(lemma, "amp", "and")) %>%
    anti_join(stop_words, by = c("lemma" = "word")) %>%
    mutate(lemma = str_to_lower(lemma)) %>%
    count(lemma) %>%
    arrange(desc(n))
  
  #Remove weird blank space which is fucking up my query - TODO add this somehow into the query
  parsed_sub_adj = parsed_sub_adj[-c(1),]
  head(parsed_sub_adj)
  #Write csv
  write.csv(parsed_sub_adj, "data/adj_freq.csv")
}


# Extract nounphrases -----------------------------------------------------
if(file.exists("data/nounphrase_freq.csv")){
  
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
    mutate(text = str_remove_all(text, regex(" ?(f|ht)(tp)(s?)(://)(.*)[.|/](.*)"))) %>%
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


#Extract entities 
entity_text = spacy_extract_entity(text$text,
                                   output = c("data.frame"),
                                   multithread = TRUE)
