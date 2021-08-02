#Filter for water industry and plot over time
water_indsustry_subset = filter(
  tweets_primary_df,
  grepl('\\bwater\\b.*\\bcompany\\b', word) |
    grepl('\\bwater\\b.*\\bcompanies\\b', word) |
    grepl('\\bwater\\b.*\\bindustry\\b', word) |
    grepl('\\bwater\\b.*\\bindustries\\b', word) |
    grepl('\\bwater\\b.*\\butility\\b', word) |
    grepl('\\bwater\\b.*\\butilities\\b', word)
) %>%
  mutate(word, gsub(" water company", "", word)) %>%
  mutate(word, gsub(" water companies", "",word)) %>%
  mutate(word,  gsub(" water industry", "", word)) %>%
  mutate(word, gsub(" water industries", "", word)) %>%
  mutate(word, gsub(" water utility", "", word)) %>%
  mutate(word, gsub(" water utilites", "", word))

corpus = corpus(water_indsustry_subset$word)
token_word = data.frame(text = corpus, stringsAsFactors = FALSE) %>% unnest_tokens(word, text)
senti_word = right_join(token_word, get_sentiments("nrc")) %>%
  filter(sentiment == "disgust") %>%
  count(word, sort = TRUE)

kable(senti_word) %>% kableExtra::kable_material_dark()

# %>%
#   select(date, word) %>%
#   group_by(date) %>%
#   tally(sort = TRUE) %>%
#   ggplot(aes(x = date, y = n)) +
#   ggtitle("Water industry et al. mentions") +
#   geom_line()
# water_indsustry_subset +scale_x_date(date_breaks = "3 month", date_labels =  "%b") 

# pollution frequency
pollution_subset = filter(
  tweets_primary_df,
  grepl('\\bsewage\\b', word) |
    grepl('\\bpollution\\b', word) |
    grepl('\\bpollute\\b', word) |
    grepl('\\bpolluting\\b', word) |
    grepl('\\bpolluter\\b', word) |
    grepl('\\bpolluters\\b', word) |
    grepl('\\bwastewater\\b', word) |
    grepl('\\bwaste\\b.*\\bwater\\b', word)) 

pollution_subset$word = gsub(" sewage", "", pollution_subset$word)
pollution_subset$word = gsub(" pollution", "", pollution_subset$word)
pollution_subset$word = gsub(" pollute", "", pollution_subset$word)
pollution_subset$word = gsub(" polluters", "", pollution_subset$word)
pollution_subset$word = gsub(" polluter", "", pollution_subset$word)
pollution_subset$word = gsub(" polluting", "", pollution_subset$word)
pollution_subset$word = gsub(" wastewater", "", pollution_subset$word)
pollution_subset$word = gsub(" waste water", "", pollution_subset$word)
pollution_subset$word = gsub(" waste", "", pollution_subset$word)

corpus = corpus(pollution_subset$word)
token_word = data.frame(text = corpus, stringsAsFactors = FALSE) %>% unnest_tokens(word, text)
senti_word = right_join(token_word, get_sentiments("nrc")) %>%
  filter(sentiment == "disgust") %>%
  count(word, sort = TRUE)

kable(senti_word) %>% kableExtra::kable_material_dark()
# %>%
#   select(date, word) %>%
#   group_by(date) %>%
#   tally(sort = TRUE) %>%
#   ggplot(aes(x = date, y = n)) +
#   ggtitle("Pollution et al. mentions") +
#   geom_line()
# 
# pollution_subset +scale_x_date(date_breaks = "3 month", date_labels =  "%b") 

#Flooding
flood_subset = filter(
  tweets_primary_df,
  grepl('\\bflood\\b', word) |
    grepl('\\bheavy\\b.*\\brain\\b', word)
) %>%
  select(date, word) %>%
  group_by(date) %>%
  tally(sort = TRUE) %>%
  ggplot(aes(x = date, y = n)) +
  ggtitle("Flood et al. mentions") +
  geom_line()
flood_subset +scale_x_date(date_breaks = "3 month", date_labels =  "%b") 
