#Clean tweets for sentiment
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
    # Replace any newline characters with a space
    str_replace_all("\\\n", " ") %>%
    # Make everything lowercase
    str_to_lower() %>%
    # Remove any trailing whitespace around the text
    str_trim("both")
}

#Subset data to only include tweet
if(exists("tweets")){
  tweets_text_single = as.data.frame(subset(tweets,
                                            select = c("text")))
}
#Time conversion
twitter_to_POSIXct = function(x, timezone = Sys.timezone()){
  
  x %>% 
    strsplit("\\s+") %>% 
    unlist %>% 
    t %>% 
    as.data.frame(stringsAsFactors = FALSE) %>% 
    set_colnames(c("week_day", "month_abb", 
                   "day", "hour", "tz", 
                   "year")) %>%
    mutate(month_num = which(month.abb %in% month_abb)) %>% 
    mutate(date_str = paste0(year, "-", month_num, "-", day, " ", 
                             hour)) %>% 
    mutate(date = format(as.POSIXct(date_str, tz = tz), 
                         tz = timezone)) %>%
    pull(date)
  
}

