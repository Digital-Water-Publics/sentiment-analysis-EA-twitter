## Load additional required packages
require(tidyverse)
require(tidytext)
require(gplots)
require(SnowballC)
require(sqldf)
theme_set(theme_bw(12))
### pull emotion words and aggregate by year and emotion terms

emotions <- tweets_primary_df %>%
  unnest_tokens(word, word) %>%                           
  anti_join(stop_words, by = "word") %>%                  
  filter(!grepl('[0-9]', word)) %>%
  left_join(get_sentiments("nrc"), by = "word") %>%
  filter(!(sentiment == "negative" | sentiment == "positive")) %>%
  group_by(year, sentiment) %>%
  summarize( freq = n()) %>%
  mutate(percent=round(freq/sum(freq)*100)) %>%
  select(-freq) %>%
  spread(sentiment, percent, fill=0) %>%
  ungroup()
## Normalize data 
sd_scale <- function(x) {
  (x - mean(x))/sd(x)
}
emotions[,c(2:9)] <- apply(emotions[,c(2:9)], 2, sd_scale)
emotions <- as.data.frame(emotions)
rownames(emotions) <- emotions[,1]
emotions3 <- emotions[,-1]
emotions3 <- as.matrix(emotions3)
## Using a heatmap and clustering to visualize and profile emotion terms expression data

heatmap.2(
  emotions3,
  dendrogram = "both",
  scale      = "none",
  trace      = "none",
  key        = TRUE,
  col    = colorRampPalette(c("green", "yellow", "red"))
)

## pull emotions words for selected heatmap groups and apply stemming
set.seed(456)
emotions_final <- tweets_primary_df  %>%
  unnest_tokens(word, word) %>% 
  filter(!grepl('[0-9]', word)) %>%                          
  left_join(get_sentiments("nrc"), by = "word") %>%
  filter(!(sentiment == "negative" | sentiment == "positive" | sentiment == "NA")) %>%
  mutate(word = wordStem(word)) %>%
  ungroup()
group1 <- emotions_final %>% 
  subset(year==2016| year==2017 ) %>%
  select(-year, -sentiment) %>%
  unique()
set.seed(456)
group4 <- emotions_final %>% 
  subset(year==2018 | year==209) %>%
  select(-year, -sentiment) %>%
  unique()
set.seed(456)
group5 <- emotions_final %>%
  subset(year==2001 | year==2008 ) %>%
  select(-year, -sentiment) %>%
  unique()