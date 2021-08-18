ea = read.csv("data/ea_mentions_all.csv")
tweet_dfm <- tokens(ea$text, remove_punct = TRUE) %>%
  dfm()

dfm = dfm(ea$text)


tag_dfm <- dfm_select(tweet_dfm, pattern = "#*")
toptag <- names(topfeatures(tag_dfm, 50))
head(toptag)

tag_fcm <- fcm(tag_dfm)
head(tag_fcm)
topgat_fcm <- fcm_select(tag_fcm, pattern = toptag)
textplot_network(topgat_fcm, min_freq = 0.1, edge_alpha = 0.8, edge_size = 5,  edge_color = "slategray",
                 vertex_color = "black")


user_dfm <- dfm_select(tweet_dfm, pattern = "@*")
topuser <- names(topfeatures(user_dfm, 50))
head(topuser)
user_fcm <- fcm(user_dfm)
head(user_fcm)
user_fcm <- fcm_select(user_fcm, pattern = topuser)
textplot_network(user_fcm, min_freq = 0.1, edge_color = "orange", edge_alpha = 0.8, edge_size = 5)

