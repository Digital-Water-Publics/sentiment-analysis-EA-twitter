# #We will try the approach for 1 tweet; Finally we will convert this into a function
# x <- "what a shame you are choosing not to regulate walleys quarry we put wealth before health should be the strap line you have ruined our community"
# x <- as.String(x)
# # Before POS tagging, we need to do Sentence annotation followed by word annotation
# wordAnnotation <- annotate(x, list(Maxent_Sent_Token_Annotator(), Maxent_Word_Token_Annotator()))
# 
# # POS tag the words & extract the "words" from the output
# POSAnnotation <- annotate(x, Maxent_POS_Tag_Annotator(), wordAnnotation)
# POSwords <- subset(POSAnnotation, type == "word")
# # Extract the tags from the words
# tags <- sapply(POSwords$features, '[[', "POS")
# 
# # Create a data frame with words and tags
# tokenizedAndTagged <- data.frame(Tokens = x[POSwords], Tags = tags)
# 
# # Define a flag(tags_mod) for pos tags - Flag set to 1 if it contains the POS tag we are interested in else 0
# # In this case we only want Noun and Adjective tags (NN, JJ)
# # Note that this will also capture variations such as NNP, NNPS etc
# tokenizedAndTagged$Tags_mod = grepl("NN|JJ", tokenizedAndTagged$Tags)
# 
# # Initialize a vector to store chunk indexes
# chunk = vector()  
# 
# # Iterate thru each word and assign each one to a group
# # if the word doesn’t belong to NN|JJ tags (i.e. tags_mod flag is 0) assign it to the default group (0)
# # If the ith tag is in “NN|JJ” (i.e. tags_mod flag is 1) assign it to group i-1 if the (i-1)th tag_mod flag is also 1; else assign it to a new group
# chunk[1] = as.numeric(tokenizedAndTagged$Tags_mod[1])
# for (i in 2:nrow(tokenizedAndTagged)) {
#   
#   if(!tokenizedAndTagged$Tags_mod[i]) {
#     chunk[i] = 0
#   } else if (tokenizedAndTagged$Tags_mod[i] == tokenizedAndTagged$Tags_mod[i-1]) {
#     chunk[i] = chunk[i-1]
#   } else {
#     chunk[i] = max(chunk) + 1
#   }
#   
# }
# 
# # Split and chunk words
# text_chunk <- split(as.character(tokenizedAndTagged$Tokens), chunk)
# tag_pattern <- split(as.character(tokenizedAndTagged$Tags), chunk)
# names(text_chunk) <- sapply(tag_pattern, function(x) paste(x, collapse = "-"))
# 
# # Extract chunks matching pattern
# # We will extract JJ-NN chunks and two or more continuous NN tags 
# # "NN.-NN" -> The "." in this regex will match all variants of NN: NNP, NNS etc
# res = text_chunk[grepl("JJ-NN|NN.-NN", names(text_chunk))]
# res

total_words_count <- tweets_primary_df %>%
  unnest_tokens(word, word) %>%  
  anti_join(stop_words, by = "word") %>%                  
  filter(!grepl('[0-9]', word)) %>%
  group_by(year) %>%
  summarize(total= n()) %>%
  ungroup()

nrc_data = lexicon::nrc_emotions %>%  
  gather("sentiment", "flag", anger:trust, -term) %>% 
  filter(flag==1)

emotion_words_count <- tweets_primary_df %>% 
  unnest_tokens(word, word) %>%                           
  anti_join(stop_words, by = "word") %>%                  
  filter(!grepl('[0-9]', word)) %>%
  inner_join(nrc_data, by=c("word"="term"))  %>%
  group_by(year) %>%
  summarize(emotions= n()) %>%
  ungroup()

emotions_to_total_words <- total_words_count %>%
  left_join(emotion_words_count, by="year") %>%
  mutate(percent_emotions=round((emotions/total)*100,1)) %>%
  drop_na()

ggplot(emotions_to_total_words, aes(x=year, y=percent_emotions)) +
  geom_line(size=1) +
  scale_y_continuous(limits = c(0, 35), breaks = c(0, 5, 10, 15, 20, 25, 30, 35)) +
  xlab("Year") + 
  ylab("Emotion terms / total words (%)") + theme(legend.position="none") +
  ggtitle("Proportion of emotion words usage \n in EA mentions on Twitter")

### pull emotion words and aggregate by year and emotion terms
emotions <- tweets_primary_df %>% 
  unnest_tokens(word, word) %>%                           
  anti_join(stop_words, by = "word") %>%                  
  filter(!grepl('[0-9]', word)) %>%
  inner_join(nrc_data, by=c("word"="term"))  %>%
  group_by(year, sentiment) %>%
  summarize( freq = n()) %>%
  mutate(percent=round(freq/sum(freq)*100)) %>%
  select(-freq) %>%
  ungroup()
### need to convert the data structure to a wide format
emo_box = emotions %>%
  spread(sentiment, percent, fill=0) %>%
  ungroup()
### color scheme for the box plots (This step is optional)
cols  <- colorRampPalette(brewer.pal(7, "Set3"), alpha=TRUE)(8)
boxplot2(emo_box[,c(2:9)], col=cols, lty=1, shrink=0.8, textcolor="red",        xlab="Emotion Terms", ylab="Emotion words count (%)", main="Distribution of emotion words count in tweets directed at the EA (2016 - 2021")

## yearly line chart
ggplot(emotions, aes(x=year, y=percent, color=sentiment, group=sentiment)) +
  geom_line(size=1) +
  geom_point(size=0.5) +
  xlab("Year") +
  ylab("Emotion words count (%)") +
  ggtitle("Emotion words expressed in tweets \n directed at the EA")

### calculate overall averages and standard deviations for each emotion term
overall_mean_sd <- emotions %>%
  group_by(sentiment) %>%
  summarize(overall_mean=mean(percent), sd=sd(percent))
### draw a bar graph with error bars
ggplot(overall_mean_sd, aes(x = reorder(sentiment, -overall_mean), y=overall_mean)) +
  geom_bar(stat="identity", fill="darkgreen", alpha=0.7) + 
  geom_errorbar(aes(ymin=overall_mean-sd, ymax=overall_mean+sd), width=0.2,position=position_dodge(.9)) +
  xlab("Emotion Terms") +
  ylab("Emotion words count (%)") +
  ggtitle("Emotion words expressed in tweets \n directed at the EA (2016 – 2021)") + 
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  coord_flip( )
## Hi / Low plots compared to the 40-years average
emotions_diff <- emotions  %>%
  left_join(overall_mean_sd, by="sentiment") %>%
  mutate(difference=percent-overall_mean)

ggplot(emotions_diff, aes(x=year, y=difference, colour=difference>0)) +
  geom_segment(aes(x=year, xend=year, y=0, yend=difference),
               size=1.1, alpha=0.8) +
  geom_point(size=1.0) +
  xlab("Emotion Terms") +
  ylab("Net emotion words count (%)") +
  ggtitle("Emotion words expressed in  tweets \n directed at the EA (2016 – 2021)") + 
  theme(legend.position="none") +
  facet_wrap(~sentiment, ncol=4)

trust_tweets = tweets_primary_df %>% filter(trust > 0) %>% select(word, sent_score)


