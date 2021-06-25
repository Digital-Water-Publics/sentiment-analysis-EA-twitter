# Sentiment Functions -----------------------------------------------------
## NRC Sentiment Functions
### 1. General sentiment function and viz
general_nrc_sentiment = function(corpus){
  #token df
  token = data.frame(text=tt, stringsAsFactors = FALSE) %>% 
    unnest_tokens(word, text)
  
  #Matching sentiment words from the 'NRC' sentiment lexicon
  senti = inner_join(token, get_sentiments("nrc")) %>%
    count(sentiment) %>%
    arrange(sentiment)
  
  senti$percent = (senti$n/sum(senti$n))*100
  
  #Plotting the sentiment summary
  ggplot(senti, aes(sentiment, percent)) +
    geom_bar(aes(fill = sentiment), position = 'stack', stat = 'identity')+
    ggtitle("(NRC EMO-LEX) \nEmotional Sentiment for Environment Agency \nmentions on Twitter")+
    coord_flip() +
    coord_polar() +
    theme(
      plot.background = element_rect(fill = "black", colour = NA),
      panel.background = element_rect(fill = "black", colour = NA),
      legend.position="none",
      axis.text=element_text(color="white"),
      text = element_text(size=16,  family="times", colour = "white"),
    ) 
}
general_nrc_sentiment(corpus = tweets_text) # test
### 2. Entity (word) sentiment function and viz
entity_nrc_sentiment = function(word) {
  corpus = corpus(tweets_text$text)
  corpus = (corpus_water = subset(corpus, grepl(word, texts(corpus))))
  token_word = data.frame(text=corpus, stringsAsFactors = FALSE) %>% unnest_tokens(word, text)
  senti_word = inner_join(token_word, get_sentiments("nrc")) %>%
    count(sentiment)
  senti_word$percent = (senti_word$n/sum(senti_word$n))*100
  ggplot(senti_word, aes(sentiment, percent)) +
    geom_bar(aes(fill = sentiment), position = 'stack', stat = 'identity')+
    ggtitle(paste(word, sep = " ","word sentiment \nfrom Environment Agency Mentions"))+
    coord_flip() +
    coord_polar() +
    theme(
      plot.background = element_rect(fill = "black", colour = NA),
      panel.background = element_rect(fill = "black", colour = NA),
      legend.position="none",
      axis.text=element_text(color="white"),
      text = element_text(size=16,  family="times", colour = "white"),
    ) 
}
sentimentPerWord(word = "river") #test
#TODO Add a tweet-level sentiment, highlighting the different words in the text NS 25/6


### 3. Entity (tweet) sentiment function and viz
entity_tweet_nrc_sentiment = function(word){
  
  for (i in 1:nrow(tweets_text_single)) {
    text = tweets_text_single$text[i]
      text = as.data.frame(text)
    #tokenize text
    token = data.frame(text = text, stringsAsFactors = FALSE) %>%
      unnest_tokens(word, text)
    
    #match sentiment words from the 'NRC' sentiment lexicon
    senti = inner_join(token, get_sentiments("nrc")) 
    
    #save specific words in tweets with emotional lex pair
    emo_words = as.character(
      unique(senti$word)
    ) 
    st = paste(emo_words,collapse=", ")
    text$emo_lex = st
    #save specific emotion based on word
    emotion = as.character(unique(senti$sentiment))
    et = paste(emotion,collapse = " ")
    text$emotion = et
    #save skey/most frequent emotions
    x = c(senti$sentiment)
    tt = table(x)
    names = names(tt[tt==max(tt)])
    nt = paste(names,collapse = " ")
    text$key_emotion = nt
  }
  
  tweets_text_single = subset(
    tweets_text, select = "text"
  )
  text = "A short video explaining the steps some residents across the South West have taken to reduce risk to their homes and Be Flood Ready"
  text = as.data.frame(text)
  
  #token df
  token = data.frame(text=text, stringsAsFactors = FALSE) %>% 
    unnest_tokens(word, text)
  

}

## Polarity (pos-neg) Sentiment Functions
### 1. General sentiment polarity function and histogram
polarity_tweet_sentiment = function(corpus){
  # get average sentiment score for each sentence
  sentiment_support <- sentiment_by(get_sentences(corpus$text))
  #plot the score distribution
  ggplot(sentiment_support,aes(ave_sentiment)) +
    geom_histogram(bins = 50) +
    labs(title = "Sentiment Histogram of Tweets", x = "Sentiment Score") +
    theme_bw() +
    theme(plot.title = element_text(size = 14, face = "bold",hjust = 0.5)) +
    geom_vline(xintercept = 0, color = "red")
}
polarity_tweet_sentiment(corpus = tweets_text) #test

### 2. QDAP sentiment polarity function
tweet_QDAP_sentiment = function(corpus){
  tt = as.data.frame(corpus$text)  
  #Analyze sentiment
  sentiment <- analyzeSentiment(tt)
  #Extract dictionary-based sentiment according to the QDAP dictionary
  sentiment2 <- sentiment$SentimentQDAP
  #View sentiment direction (i.e. positive, neutral and negative)
  tt$sentiment <- convertToDirection(sentiment$SentimentQDAP)
}
tweet_QDAP_sentiment(corpus = tt) #test

