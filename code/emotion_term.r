## Load additional required packages
require(tidyverse)
require(tidytext)
require(gplots)
require(SnowballC)
require(sqldf)
theme_set(theme_bw(12))
### pull emotion words and aggregate by year and emotion terms

emotions <- df_no_ea %>%
  unnest_tokens(word, clean_tweet) %>%                           
  anti_join(stop_words, by = "word") %>%                  
  filter(!grepl('[0-9]', word)) %>%
  left_join(get_sentiments("nrc"), by = "word") %>%
  filter(!(sentiment == "negative" | sentiment == "positive")) %>%
  group_by(date, sentiment) %>%
  summarize( freq = n()) %>%
  mutate(percent=round(freq/sum(freq)*100)) %>%
  select(-freq) 
## Normalize data 
# sd_scale <- function(x) {
#   (x - mean(x))/sd(x)
# }
# emotions[,c(2:9)] <- apply(emotions[,c(2:9)], 2, sd_scale)
# emotions <- as.data.frame(emotions)
# rownames(emotions) <- emotions[,1]
# emotions3 <- emotions[,-1]
# emotions3 <- as.matrix(emotions3)
# ## Using a heatmap and clustering to visualize and profile emotion terms expression data
# heatmap.2(
#   emotions3,
#   Rowv = FALSE,
#   dendrogram = "both",
#   scale      = "none",
#   trace      = "none",
#   key        = TRUE,
#   col    = colorRampPalette(c("green", "yellow", "red"))
# )
# emotions %>% pivot_wider(names_from = "year")
# Basic stream graph: just give the 3 arguments
# year, sentiment, value 
# Basic stream graph: just give the 3 arguments
# year, sentiment, value 
streamgraph(emotions, key="sentiment", value="percent", date="date", height="300px", width="1000px")  %>%
  sg_legend(show=TRUE, label="sentiment: ")  %>% sg_axis_x(tick_interval = 12) %>%   sg_axis_y(0)#
