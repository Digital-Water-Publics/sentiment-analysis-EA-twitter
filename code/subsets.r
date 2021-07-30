tweets_primary_df = readRDS("data/tweets_primary_df_topics_best.rds")
#subset tweets to only include topic 1

plot_cluster_senti_freq = function(topic_n)
{
  k6_topic_1_pollution = tweets_primary_df %>% 
    filter(`key_topic topic(k=5)` == topic_n) %>%
    summarise(across(anticipation:joy, ~ sum(.x, na.rm = TRUE))) %>%
    select(-c(positive, negative)) %>%
    gather()
  ggplot(k6_topic_1_pollution, aes(x = reorder(key, value), y = value)) +
    geom_bar(stat = "identity") +
    coord_flip()
}
plot_cluster_senti_freq(topic_n = 1)
plot_cluster_senti_freq(topic_n = 2)
plot_cluster_senti_freq(topic_n = 3)
plot_cluster_senti_freq(topic_n = 4)
plot_cluster_senti_freq(topic_n = 5)

asdas = tweets_primary_df %>%
  filter(`key_topic topic(k=6)` == 1) %>%
  filter(trust > 0) %>%
  select(word,trust,emo_lex_trigger,`key_topic topic(k=6)`)

subset_by_topic = function(topic_n)
{
  topic_sub = tweets_primary_df %>%
    filter(`key_topic topic(k=5)` == topic_n) %>%
    select(`key_topic topic(k=5)`,word, date)
  
  sub_sample = sample_n(topic_sub,500)
  
  write.csv(sub_sample, paste("data/topic",n,"sub.csv", sep = "_"))
  kable(sub_sample) %>% kableExtra::kable_material_dark()
}
subset_by_topic(topic_n = 5)

topics=tweets_primary_df[2:6]
kable(summary(topics)) %>% kableExtra::kable_material_dark()

plot_topic_history_freq = function(topic_n)
{
  tweets_primary_df %>%
    filter(`key_topic topic(k=5)` == topic_n) %>%
    group_by(date) %>%
    tally(sort = TRUE) %>%
    ggplot(aes(x=date, y=n)) +
    geom_line()
  ggsave(paste("data/", topic_n, "topic_history.png", sep = "_"))
}
plot_topic_history_freq(topic_n = 5)

  



#Create sample of tweets
k6_topic_1_sample = sample_n(k6_topic_1_pollution,500)
kable(k6_topic_1_sample) %>% kableExtra::kable_material_dark()

#-------------------------------------------------------------------------#
k6_topc_3_flooding = tweets_primary_df %>% filter(`key_topic topic(k=6)` == 2)
#Filter results that include disgust
k6_topc_3_flooding_summary = k6_topc_3_flooding %>%
  group_by(word) %>%
  summarise(across(anticipation:joy, ~ sum(.x, na.rm = FALSE))) %>%
  select(-c(positive, negative)) %>%
  filter(disgust != 123123123)



k6_senti_sums_topic_3 = gather(k6_senti_sums_topic_3)
ggplot(k6_senti_sums_topic_3, aes(x = reorder(key, value), y = value)) +
  geom_bar(stat = "identity") +
  coord_flip()

#Create sample of tweets
k6_topic_1_sample = sample_n(k6_topc_3_flooding_summary,500)
kable(k6_topic_1_sample) %>% kableExtra::kable_material_dark()
#-------------------------------------------------------------------------#
k6_topc_5_mystery = tweets_primary_df %>% filter(`key_topic topic(k=6)` == 5)
k6_topc_5_mystery_summary = k6_topc_5_mystery %>%
  group_by(word) %>%
  summarise(across(anticipation:joy, ~ sum(.x, na.rm = FALSE))) %>%
  select(-c(positive, negative)) %>%
  filter(disgust != 190312093)


k6_senti_sums_topic_5 = gather(k6_senti_sums_topic_5)
ggplot(k6_senti_sums_topic_5, aes(x = reorder(key, value), y = value)) +
  geom_bar(stat = "identity") +
  coord_flip()

k6_topic_5_sample = sample_n(k6_topc_5_mystery_summary,500)
kable(k6_topic_5_sample) %>% kableExtra::kable_material_dark()


#TODO
##### Historical trends for topics // frequency of tweets in topic X dates

