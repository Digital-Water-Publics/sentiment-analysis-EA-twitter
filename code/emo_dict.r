###### Generate dictionary for each emotion classification
anger_tweets = tweets_primary_df %>% filter(anger > 0) %>% select(word, sent_score)
anticpation_tweets = tweets_primary_df %>% filter(anticipation > 0) %>% select(word, sent_score)  
disgust_tweets = tweets_primary_df %>% filter(disgust > 0) %>% select(word, sent_score)
fear_tweets = tweets_primary_df %>% filter(fear > 0) %>% select(word, sent_score)
joy_tweets = tweets_primary_df %>% filter(joy > 0) %>% select(word, sent_score)  
sadness_tweets = tweets_primary_df %>% filter(sadness > 0) %>% select(word, sent_score)  
surprise_tweets = tweets_primary_df %>% filter(surprise > 0) %>% select(word, sent_score)  
trust_tweets = tweets_primary_df %>% filter(trust > 0) %>% select(word, sent_score)  

df.list = list(anger_tweets,anticpation_tweets,disgust_tweets,fear_tweets,joy_tweets,sadness_tweets,surprise_tweets,trust_tweets)

for(i in df.list){
  
}

tt = analyzeSentiment(trust_tweets$word)
dict <- generateDictionary(trust_tweets$word, trust_tweets$sent_score)
summary(disgust_dict)
write(dict, file="trust_tweet.dict")
disgust_dict = read("disgust_tweet.dict")
summary(trust_dict)
