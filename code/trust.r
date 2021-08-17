#### Exploring notions of trust #### ------------------------------------
trust_tweets = df_no_ea %>% filter(trust > 0)

trust_tweets$sentdirection = SentimentAnalysis::convertToBinaryResponse(trust_tweets$senti_nrc$sentiment)
table(trust_tweets$sentdirection)

trust_tweets$sentdirection = str_replace(trust_tweets$sentdirection,"positive","trust")
trust_tweets$sentdirection = str_replace(trust_tweets$sentdirection,"negative","distrust")

trust_time = trust_tweets %>% group_by(date) %>% count(sentdirection)

ggplot(trust_time,aes(x=date,y=n,)) + geom_line() +
  coord_cartesian(clip = "off") +
  guides(fill = guide_legend(label.position = "bottom", nrow = 1)) +
  facet_wrap(~sentdirection) +
  labs(
    title = "Trust vs Distrust",
    subtitle = "A comparison of polarity of trust triggered tweets",
    caption = "Data: Twitter API"
  ) +
  theme_minimal(base_size = 16, base_family = "Roboto") +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    legend.key.width = unit(10, "mm"),
    legend.key.height = unit(2, "mm"),
    legend.text = element_text(family = "Roboto Condensed", size = rel(0.6)),
    plot.title.position = "panel",
    plot.caption.position = "plot",
    plot.title = element_text(size = rel(1.9), hjust = 0.5, family = "Roboto Condensed", face = "bold"),
    plot.caption = element_text(hjust = 0.5, size = rel(0.7), color = "grey60"),
    plot.subtitle = element_text(hjust = 0.5, color = "grey60"),
    plot.background = element_rect(fill = "grey95", color = "grey95"),
    plot.margin = margin(t = 10, r = 20, b = 10),
    axis.text.y = element_text(family = "JetBrains Mono"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey70", size = 0.4, linetype = "solid"),
    panel.spacing.x = unit(2, "line"),
    panel.spacing.y = unit(1, "line")
  )
