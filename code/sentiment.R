# Create dataframe containing all sentiment scores ------------------------

# mock df
df = data.frame (
  text = c("this is a test tweet"),
  word_count = 20,
  emo_lex = c("test"),
  emotion = c("anticipation"),
  key_emotion = c("anticipation"),
  av_sentiment_score = 0,
  SentimentGI = 0,
  NegativityGI = 0,
  PositivityGI = 0,
  
  SentimentHE = 0,
  NegativityHE = 0,
  PositivityHE = 0,
  
  SentimentLM = 0,
  NegativityLM = 0,
  PositivityLM = 0,
  RatioUncertaintyLM = 0,
  
  SentimentQDAP = 0,
  NegativityQDAP = 0,
  PositivityQDAP = 0
)

sentiment_df = function(tweets) {
  for (i in 1:nrow(tweets)) {
    text = tweets$text[i]
    text = as.data.frame(text)
    #tokenize text
    token = data.frame(text = text, stringsAsFactors = FALSE) %>%
      unnest_tokens(word, text)
    
    #match sentiment words from the 'NRC' sentiment lexicon
    senti = inner_join(token, get_sentiments("nrc"))
    
    #save specific words in tweets with emotional lex pair
    emo_words = as.character(unique(senti$word))
    st = paste(emo_words, collapse = ", ")
    text$emo_lex = st
    #save specific emotion based on word
    emotion = as.character(unique(senti$sentiment))
    et = paste(emotion, collapse = " ")
    text$emotion = et
    #save skey/most frequent emotions
    x = c(senti$sentiment)
    tt = table(x)
    names = names(tt[tt == max(tt)])
    nt = paste(names, collapse = " ")
    text$key_emotion = nt
    
    sentiment_support = sentiment_by(get_sentences(tweets_text_single$text[i]))
    text$av_sentiment_score = sentiment_support$ave_sentiment
    
    sentiment = analyzeSentiment(tweets_text_single$text[i])
    
    text$SentimentGI = sentiment$SentimentGI
    text$NegativityGI = sentiment$NegativityGI
    text$PositivityGI = sentiment$PositivityGI
    
    text$SentimentHE = sentiment$SentimentHE
    text$NegativityHE = sentiment$NegativityHE
    text$PositivityHE = sentiment$PositivityHE
    
    text$SentimentLM = sentiment$SentimentLM
    text$NegativityLM = sentiment$NegativityLM
    text$PositivityLM = sentiment$PositivityLM
    text$RatioUncertaintyLM = sentiment$RatioUncertaintyLM
    
    text$SentimentQDAP = sentiment$SentimentQDAP
    text$NegativityQDAP = sentiment$NegativityQDAP
    text$PositivityQDAP = sentiment$PositivityQDAP
    text$word_count = sentiment$WordCount
    
    df = rbind(text,df)
  }
}
sentiment_df(tweets = tt)


