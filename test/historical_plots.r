if(exists("tweets")){
tweets$created_at = substr(tweets$created_at, start = 1, stop = 10)
#Filter tweets by year 
  #2021
  tweets_2021 = tweets %>% 
    filter(substr(created_at, 1, 4) == "2021")
  write.csv(tweets_2021,"data/tweets_2021.csv")
  #2020
  tweets_2020 = tweets %>% 
    filter(substr(created_at, 1, 4) == "2020")
  write.csv(tweets_2020, "data/tweets_2020.csv")
  #2019
  tweets_2019 = tweets %>% 
    filter(substr(created_at, 1, 4) == "2019")
  write.csv(tweets_2019,"data/tweets_2019.csv")
  #2018
  tweets_2018 = tweets %>% 
    filter(substr(created_at, 1, 4) == "2018")
  write.csv(tweets_2018,"data/tweets_2018.csv")
  #2017
  tweets_2017 = tweets %>% 
    filter(substr(created_at, 1, 4) == "2017")
  write.csv(tweets_2017, "data/tweets_2017.csv")
  
} else {
  tweets = read.csv("data/ea_mentions_2017_2021.csv")
  tweets$created_at = substr(tweets$created_at, start = 1, stop = 10)
  als = tweets %>% 
    filter(substr(created_at, 1, 4) == "2020")
}

tweets$year = substr(tweets$created_at,1,4)

ggplot(tweets, aes(x=year, y=tweets$senti$sentiment, fill=year)) + # fill=name allow to automatically dedicate a color for each group
geom_violin()


# Calculate proportion of each level
proportion <- table(tweets$year)/nrow(tweets)

#Draw the boxplot, with the width proportionnal to the occurence !
boxplot(
  tweets$senti$sentiment ~ tweets$year ,
  width = proportion ,
  col = c("honeydew2" , "seagreen2"),
  main = "Sentiment polarity score 2016-2021",
  xlab = "Year",
  ylab = "Polarity Score"
)
tweets_primary_df$time = substr(tweets_primary_df$created_at, start = 12, stop = 19)
tweets_primary_df$date = substr(tweets_primary_df$created_at, start = 1, stop = 10)
tweets_primary_df$date = as.Date(tweets_primary_df$date)

ggplot(tweets_primary_df, aes(x=date, y=sent_score)) +
  geom_line() + 
  xlab("")

ggplot(tweets_primary_df, aes(x = date, y = sent_score)) + 
  geom_area(aes(color = year, fill = year), 
            alpha = 0.5, position = position_dodge(0.8)) +
  scale_color_manual(values = c("#2DE1FC", "#2AFC98", "#09E85E", "#16C172", "#214F4B","#355F5B")) +
  scale_fill_manual(values = c("#2DE1FC", "#2AFC98", "#09E85E", "#16C172", "#214F4B","#355F5B")) 
