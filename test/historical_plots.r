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

