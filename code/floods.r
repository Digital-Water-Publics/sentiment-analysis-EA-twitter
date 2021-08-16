library(sf)
# read data
floods = read_sf(
  "../../Documents/EA_RecordedFloodOutlines_SHP_Full/data/Recorded_Flood_Outlines.shp"
)
# create year column
floods$year = substr(floods$start_date, 1, 4)
floods$year = as.numeric(floods$year)

# filter based on study timeline
floods_study_year = floods %>% filter(year > 2015) %>%
  filter(year < 2022) %>%
  filter(hfm_status == "considered and accepted") %>%
  filter(flood_caus != "unknown") %>%
  filter(fluvial_f == "True")

floods_ok = floods_study_year %>% select(name, start_date, end_date) %>%
  st_drop_geometry() %>%
  distinct(name, .keep_all = TRUE)

# create day events of sentiment scores
sentiment_history = df_no_ea %>%
  group_by(date) %>%
  summarise(across(anticipation:surprise, ~ sum(.x, na.rm = FALSE)))

# create a total sentiment score for each flood event
flood_event_sentiment = floods_ok %>% crossing(sentiment_history)  %>%
  mutate(across(c(start_date, end_date, date), ymd),
         interval = interval(start_date, end_date)) %>%
  filter(date %within% interval) %>%
  group_by(interval) %>%
  mutate(sum_anger = sum(anger)) %>%
  mutate(sum_anticipation = sum(anticipation)) %>%
  mutate(sum_joy = sum(joy)) %>%
  mutate(sum_trust = sum(trust)) %>%
  mutate(sum_fear = sum(fear)) %>%
  mutate(sum_sadness = sum(sadness)) %>%
  mutate(sum_disgust = sum(disgust)) %>%
  mutate(sum_surprise = sum(surprise)) %>%
  select(
    name,
    sum_anger,
    sum_anticipation,
    sum_joy,
    sum_trust,
    sum_fear,
    sum_sadness,
    sum_disgust,
    sum_surprise
  ) %>%
  distinct(name, .keep_all = TRUE) %>%
  arrange(interval)

# create table
kableExtra::kable(flood_event_sentiment) %>% kableExtra::kable_material_dark()

# join with spatial data
flood_event_sentiment_sf = right_join(flood_event_sentiment,floods_study_year) %>%
  st_as_sf()

# plot data
tmap_mode("view")
tm_basemap("Stamen.Toner") +
tm_shape(flood_event_sentiment_sf) + 
  tm_polygons("sum_trust",  palette = "RdYlBu")
