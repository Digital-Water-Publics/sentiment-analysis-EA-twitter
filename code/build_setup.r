# #### Cleaning function #### -----------------------------------------------------
clean_tweets_sentiment = function(x) {
  x %>%
    # Remove URLs
    str_remove_all(" ?(f|ht)(tp)(s?)(://)(.*)[.|/](.*)") %>%
    # Remove mentions e.g. "@my_account"
    str_remove_all("@[[:alnum:]_]{4,}") %>%
    # Remove hashtags
    str_remove_all("#[[:alnum:]_]+") %>%
    # Replace "&" character reference with "and"
    str_replace_all("&amp;", "and") %>%
    # Remove puntucation, using a standard character class
    str_remove_all("[[:punct:]]") %>%
    # Remove "RT: " from beginning of retweets
    str_remove_all("^RT:? ") %>%
    # Replace any newline characters with a =space
    str_replace_all("\\\n", " ") %>%
    # Make everything lowercase
    str_to_lower() %>%
    # Remove any trailing whitespace around the text
    str_trim("both")
}

# #### Clean and transform DF #### -----------------------------------------------------

if (file.exists("data/df_no_ea.RDS")) {
  df_no_ea = readRDS("data/df_no_ea.RDS")
} else {
  #Read dataset and EA accounts
  ea_accounts = readRDS("data/EA_accounts.rds")
  all_tweets = read.csv("data/ea_mentions_all.csv")
  #Remove EA accounts from dataset
  df_no_ea = all_tweets %>% anti_join(ea_accounts)
  #Create new column of clean tweets
  df_no_ea$clean_tweet = clean_tweets_sentiment(df_no_ea$text)
  # Create temportal and doc_id flags
  df_no_ea$time = substr(df_no_ea$created_at, start = 12, stop = 19)
  df_no_ea$date = substr(df_no_ea$created_at, start = 1, stop = 10)
  df_no_ea$date = as.Date(df_no_ea$date)
  df_no_ea$document = seq.int(nrow(df_no_ea))
  df_no_ea$year = substr(df_no_ea$created_at, 1, 4)
  #Calculate polarity of tweets
  df_no_ea$senti_nrc = sentiment(
    df_no_ea$clean_tweet,
    polarity_dt =lexicon::hash_sentiment_sentiword,
    hyphen = " ",
    amplifier.weight = 0.8,
    n.before = Inf,
    n.after = Inf,
    question.weight = 1,
    adversative.weight = 0.5,
    neutral.nonverb.like = TRUE,
    missing_value = 0
  )
  
  # Get NRC emo-lex data
  nrc_data = lexicon::nrc_emotions %>%
    gather("sentiment", "flag", anger:trust,-term) %>%
    filter(flag == 1)
  ### pull emotion words and aggregate by document and emotion terms
  emotions = df_no_ea %>%
    unnest_tokens(word, clean_tweet) %>%
    anti_join(stop_words, by = "word") %>%
    filter(!grepl('[0-9]', word)) %>%
    inner_join(nrc_data, by = c("word" = "term"))  %>%
    group_by(document, sentiment) %>%
    summarize(freq = n()) %>%
    ungroup()
  #Create wider table of emotions
  emotions = emotions %>%
    pivot_wider(values_from = freq,
                names_from = sentiment)
  #Replace NAs as 0 
  emotions = emotions %>% mutate_at(
    vars(anger, anticipation, disgust, fear, joy, sadness, surprise, trust),
    ~ replace_na(., 0)
  )
  #Merge emotions to df
  df_no_ea = left_join(df_no_ea, emotions)
  #Replace Nas to 0
  df_no_ea = df_no_ea %>% mutate_at(
    vars(anger, anticipation, disgust, fear, joy, sadness, surprise, trust),
    ~ replace_na(., 0)
  )
  
  
  saveRDS(df_no_ea,"data/df_no_ea.RDS")
}


# General plots  ----------------------------------------------------------
theme_set(theme_bw(12))

summary_plots = function(x) {
  total_words_count = x %>%
    unnest_tokens(word, clean_tweet) %>%
    anti_join(stop_words, by = "word") %>%                  
    filter(!grepl('[0-9]', word)) %>%
    group_by(year) %>%
    summarize(total= n()) %>%
    ungroup()
  
  emotion_words_count = x %>%
    unnest_tokens(word, clean_tweet) %>%
    anti_join(stop_words, by = "word") %>%                  
    filter(!grepl('[0-9]', word)) %>%
    inner_join(nrc_data, by=c("word"="term"))  %>%
    group_by(year) %>%
    summarize(emotions= n()) %>%
    ungroup()
  
  emotions_to_total_words <- total_words_count %>%
    left_join(emotion_words_count, by="year") %>%
    mutate(percent_emotions=round((emotions/total)*100,1))
  
  ggplot(emotions_to_total_words, aes(x = year, y = percent_emotions)) +
    geom_line(size = 1) +
    scale_y_continuous(limits = c(0, 35),
                       breaks = c(0, 5, 10, 15, 20, 25, 30, 35)) +
    xlab("Year") +
    ylab("Emotion terms / total words (%)") + theme(legend.position = "none") +
    ggtitle("Proportion of emotion words usage \n in EA mentions on Twitter")
  
  ### pull emotion words and aggregate by year and emotion terms
  emotions = x %>%
    unnest_tokens(word, clean_tweet) %>%
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
    spread(sentiment, percent, fill = 0) %>%
    ungroup()
  ### color scheme for the box plots (This step is optional)
  cols  = colorRampPalette(brewer.pal(7, "Set3"), alpha = TRUE)(8)
  boxplot2(
    emo_box[, c(2:9)],
    col = cols,
    lty = 1,
    shrink = 0.8,
    textcolor = "red",
    xlab = "Emotion Terms",
    ylab = "Emotion words count (%)",
    main = "Distribution of emotion words count in tweets directed at the EA (2016 - 2021"
  )
  
  ## yearly line chart
  ggplot(emotions,
         aes(
           x = year,
           y = percent,
           color = sentiment,
           group = sentiment
         )) +
    geom_line(size = 1) +
    geom_point(size = 0.5) +
    xlab("Year") +
    ylab("Emotion words count (%)") +
    ggtitle("Emotion words expressed in tweets \n directed at the EA")
  
  ### calculate overall averages and standard deviations for each emotion term
  overall_mean_sd = emotions %>%
    group_by(sentiment) %>%
    summarize(overall_mean = mean(percent), sd = sd(percent))
  ### draw a bar graph with error bars
  ggplot(overall_mean_sd, aes(x = reorder(sentiment,-overall_mean), y =
                                overall_mean)) +
    geom_bar(stat = "identity",
             fill = "darkgreen",
             alpha = 0.7) +
    geom_errorbar(
      aes(ymin = overall_mean - sd, ymax = overall_mean + sd),
      width = 0.2,
      position = position_dodge(.9)
    ) +
    xlab("Emotion Terms") +
    ylab("Emotion words count (%)") +
    ggtitle("Emotion words expressed in tweets \n directed at the EA (2016 – 2021)") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    coord_flip()
  ## Hi / Low plots compared to the 40-years average
  emotions_diff = emotions  %>%
    left_join(overall_mean_sd, by = "sentiment") %>%
    mutate(difference = percent - overall_mean)
  
  ggplot(emotions_diff,
         aes(x = year, y = difference, colour = difference > 0)) +
    geom_segment(aes(
      x = year,
      xend = year,
      y = 0,
      yend = difference
    ),
    size = 1.1,
    alpha = 0.8) +
    geom_point(size = 1.0) +
    xlab("Emotion Terms") +
    ylab("Net emotion words count (%)") +
    ggtitle("Emotion words expressed in  tweets \n directed at the EA (2016 – 2021)") +
    theme(legend.position = "none") +
    facet_wrap( ~ sentiment, ncol = 4)
  
}

summary_plots(df_no_ea)


# Sentiment Functions -----------------------------------------------------
## NRC Sentiment Functions
### 1. General sentiment function and viz
general_nrc_sentiment = function(corpus) {
  #token df
  token = data.frame(text = topic_sub$word, stringsAsFactors = FALSE) %>%
    unnest_tokens(word, text)
  
  #Matching sentiment words from the 'NRC' sentiment lexicon
  senti = inner_join(token, get_sentiments("nrc")) %>%
    count(sentiment) %>%
    filter(sentiment != "positive") %>%
    filter(sentiment != "negative") %>%
    arrange(sentiment) 
  
  senti$percent = (senti$n / sum(senti$n)) * 100
  
  #Plotting the sentiment summary
  ggplot(senti, aes(sentiment, percent)) +
    geom_bar(aes(fill = sentiment), position = 'stack', stat = 'identity') +
    ggtitle("(NRC EMO-LEX) \nEmotional Sentiment for Environment Agency \nmentions on Twitter") +
    coord_flip() +
    theme(
      plot.background = element_rect(fill = "black", colour = NA),
      panel.background = element_rect(fill = "black", colour = NA),
      legend.position = "none",
      axis.text = element_text(color = "white"),
      text = element_text(
        size = 16,
        family = "times",
        colour = "white"
      ),
    )
}
# test general_nrc_sentiment(corpus = clean_tweets)

### 2. Entity (word) sentiment function and viz
entity_nrc_sentiment = function(word) {
  corpus = corpus(df_no_ea$word)
  corpus = (corpus_water = subset(corpus, grepl(word, texts(corpus))))
  token_word = data.frame(text = corpus, stringsAsFactors = FALSE) %>% unnest_tokens(word, text)
  senti_word = inner_join(token_word, get_sentiments("nrc")) %>%
    count(sentiment)
  senti_word$percent = (senti_word$n / sum(senti_word$n)) * 100
  ggplot(senti_word, aes(sentiment, percent)) +
    geom_bar(aes(fill = sentiment), position = 'stack', stat = 'identity') +
    ggtitle(paste(word, sep = " ", "word sentiment \nfrom Environment Agency Mentions")) +
    coord_flip() +
    theme(
      plot.background = element_rect(fill = "black", colour = NA),
      panel.background = element_rect(fill = "black", colour = NA),
      legend.position = "none",
      axis.text = element_text(color = "white"),
      text = element_text(
        size = 16,
        family = "times",
        colour = "white"
      ),
    )
}
#entity_nrc_sentiment(word = "pollut")

## Polarity (pos-neg) Sentiment Functions
### 3. General sentiment polarity function and histogram
polarity_tweet_sentiment = function(corpus) {
  # get average sentiment score for each sentence
  sentiment_support <- sentiment_by(get_sentences(corpus$text))
  #plot the score distribution
  ggplot(sentiment_support, aes(ave_sentiment)) +
    geom_histogram(bins = 50) +
    labs(title = "Sentiment Histogram of Tweets", x = "Sentiment Score") +
    theme_bw() +
    theme(plot.title = element_text(
      size = 14,
      face = "bold",
      hjust = 0.5
    )) +
    geom_vline(xintercept = 0, color = "red")
}
#test polarity_tweet_sentiment(corpus = tweets_text)
