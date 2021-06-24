# Sentiment Functions -----------------------------------------------------
  #1 General Sentiment function and viz
general_nrc_sentiment = function(corpus){
  #token df
  token = data.frame(text=tweets_text, stringsAsFactors = FALSE) %>% 
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
#test
general_nrc_sentiment(corpus = tweets_text)

  #2 entity sentiment
entity_nrc_sentiment = function(corpus){
  #token df
  token = data.frame(text=tweets_text, stringsAsFactors = FALSE) %>% 
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
  #test
general_nrc_sentiment(corpus = tweets_text)




