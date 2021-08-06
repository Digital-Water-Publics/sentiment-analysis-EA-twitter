require(tidytext)
require(RColorBrewer)
require(gplots)
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
