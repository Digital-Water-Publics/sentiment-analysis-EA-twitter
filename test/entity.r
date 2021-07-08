# for first use run: spacy_install()
# & spacy_initialize()
#sample of data
sample_data = sample_n(tweets_text_single, 1)
#clean sample
sample_data = as.data.frame(sample_data %>% clean_tweets_sentiment()) %>% rename(text = "sample_data %>% clean_tweets_sentiment()")

#Create entity string dataframe
entity_sentiment_string <- function(data) {
  out <- tryCatch({
    sample_data = data
    parsedtxt = spacy_parse(
      sample_data$text,
      pos = TRUE,
      tag = TRUE,
      lemma = TRUE,
      entity = TRUE,
      dependency = TRUE,
      nounphrase = TRUE,
      multithread = TRUE
    ) %>%
      rename(word = token) %>%
      left_join(get_sentiments("nrc"))
    
    # Get sentiment with largest count
    emotion =  names(table(parsedtxt$sentiment))[as.vector(table(parsedtxt$sentiment)) ==
                                                   max(table(parsedtxt$sentiment))]
    emotion = paste(emotion, collapse = " OR ")
    
    emotion_direction = " @"
    
    emotion_phrase = paste(emotion, emotion_direction, sep = "")
    
    nounphrase_entity = spacy_extract_nounphrases(sample_data$text, output = "data.frame")
    
    nounphrase_entity = names(table(nounphrase_entity$text))[as.vector(table(nounphrase_entity$text)) ==
                                                               unique(table(nounphrase_entity$text))]
    
    nounphrase_entity_df = as.data.frame(nounphrase_entity)
    
    for (i in 1:nrow(nounphrase_entity_df)) {
      nounphrase_entity_df$nounphrase_entity[i] = paste(emotion_phrase,
                                                        nounphrase_entity_df$nounphrase_entity[i])
    }
    
    # The return value of `readLines()` is the actual value
    # that will be returned in case there is no condition
    # (e.g. warning or error).
    # You don't need to state the return value via `return()` as code
    # in the "try" part is not wrapped inside a function (unlike that
    # for the condition handlers for warnings and error below)
  },
  error = function(cond) {
    message(paste("Data has no nounphrase:", data))
    message(cond)
    # Choose a return value in case of error
    return(NA)
  },
  warning = function(cond) {
    message(paste("URL caused a warning:", url))
    message("Here's the original warning message:")
    message(cond)
    # Choose a return value in case of warning
    return(NULL)
  },
  finally = {
    # NOTE:
    # Here goes everything that should be executed at the end,
    # regardless of success or error.
    # If you want more than one expression to be executed, then you
    # need to wrap them in curly brackets ({...}); otherwise you could
    # just have written 'finally=<expression>'
    message(paste("Processed data:", url))
  })
  return(out)
}
## TEST entity_sentiment_string(data = sample_data)

# Logical rules for entity
# 1. Merge sentiment with tweet token
# 2. Apply nounphrase method
#----------------------------------------------------------------------
# IF beg_root & PRON = entity [IF results > 1 !include entity = DATE]
# ELSE nounphrase max(length)
# IF NULL use polarity translation e.g. "negative @ supply network" ? @helge
#----------------------------------------------------------------------
# 3. Add new column string e.g. "angry @ supply network"
# 4. Add polarity sentiment score
# 5. Export
