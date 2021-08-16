library(sf)
# read data
floods = read_sf("../../Documents/EA_RecordedFloodOutlines_SHP_Full/data/Recorded_Flood_Outlines.shp")
# create year column
floods$year = substr(floods$start_date,1,4)
floods$year = as.numeric(floods$year)

# filter based on study timeline 
floods_study_year = floods %>% filter(year > 2015) %>%
  filter(year < 2022) %>% 
  filter(hfm_status == "considered and accepted") %>%
  filter(flood_caus != "unknown") %>%
  filter(fluvial_f == "True") 

floods_ok = floods_study_year %>% select(name,start_date,end_date) %>% st_drop_geometry() %>% distinct(name, .keep_all = TRUE)


floods_study_year$name = unique(floods_study_year$name)


# create day events of sentiment scores
sentiment_history = df_no_ea %>%
  group_by(date) %>%
  summarise(across(anticipation:surprise, ~ sum(.x, na.rm = FALSE)))


floods_study_year$start_date = as.Date(floods_study_year$start_date)
sentiment_history$date = as.Date(sentiment_history$date)
df_no_ea$start_date = format(df_no_ea$date, "%Y-%m-%d")

floods_study_year %>% left_join(sentiment_history) %>%
  filter(between(date, start_date, end_date)) %>%
           group_by(date) %>%
           mutate(sum_amount = sum(amount))



tt %>% filter(between(start_date,end_date)) %>%
  group_by(start_date) %>%
  mutate(sum_amount = sum(amount))

tt = tt %>% distinct(name) %>% pull()

## calculate sentiment count for the start to end date of each flood incident
## use n to calculate frequency of tweets during event
## plot map of rivers and their popularity 


