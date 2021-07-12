# Word frequency ----------------------------------------------------------
#Read data
tweets = read.csv("data/ea_mentions_2017_2021.csv")
#Subset to only include tweet
text = tweets %>% select(text)
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
  write.csv(parsedtxt,"data/parsed_tweets_POS.csv")
}

# Frequency analysis for nouns --------------------------------------------
parsed_sub = parsedtxt %>% 
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


# Adjective ---------------------------------------------------------------
parsed_sub_adj = parsedtxt %>% 
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
#Write csv
write.csv(parsed_sub_adj, "data/adj_freq.csv")

#Extract nounphrases
nounphrase_text = spacy_extract_nounphrases(text$text,
                                            output = c("data.frame"),
                                            multithread = TRUE)

#Extract entities 
entity_text = spacy_extract_entity(text$text,
                                   output = c("data.frame"),
                                   multithread = TRUE)