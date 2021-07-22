library(bigmemory)
#Create clean random sample of tweets
clean_tweets = sample_n(tweets_primary_df, 15000)
clean_tweets = clean_tweets$word %>% clean_tweets_sentiment()
clean_tweets =  as.vector(clean_tweets)
clean_tweets = removeWords(clean_tweets, words = stopwords("english"))
# Create data-frame into corpus object
corpus <- Corpus(VectorSource(clean_tweets))  
merge_dtm <- DocumentTermMatrix(corpus)

doc.lengths <- rowSums(big.matrix(DocumentTermMatrix(corpus)))
dtm <- DocumentTermMatrix(corpus[doc.lengths > 0])

#Method 1
doc.length = apply(merge_dtm, 1, sum)
merge_dtm = merge_dtm[doc.length > 0, ]
freq = colSums(as.matrix(merge_dtm))
length(freq)
ord = order(freq, decreasing = TRUE)
freq[head(ord, n = 20)]

#LDA model with 10 topics selected
lda_10 = LDA(
  merge_dtm,
  k = 4,
  method = 'Gibbs',
  control = list(
    nstart = 5,
    seed = list(1505, 99, 36, 56, 88),
    best = TRUE,
    thin = 500,
    burnin = 4000,
    iter = 2000
  )
)

top10terms_10 = as.matrix(terms(lda_10, 10))
topicprob_10 = as.matrix(lda_10@gamma)
