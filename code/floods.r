floods = read_sf("../../Documents/EA_RecordedFloodOutlines_SHP_Full/data/Recorded_Flood_Outlines.shp")

floods$year = substr(floods$start_date,1,4)

floods$year = as.numeric(floods$year)

floods_study_year = floods %>% filter(year > 2015) %>%
  filter(year < 2022) %>% 
  filter(hfm_status == "considered and accepted") %>%
  filter(flood_caus != "unknown") %>%
  filter(fluvial_f == "True")


floods_study_year$start_date = as.Date(floods_study_year$start_date)
df_no_ea$start_date = as.Date(df_no_ea$start_date)
df_no_ea$start_date = format(df_no_ea$date, "%Y-%m-%d")
df_no_ea$start_date = as.numeric(df_no_ea$start_date)



df_foods = right_join(df_no_ea,floods_study_year) 

## calculate sentiment count for the start to end date of each flood incident
## use n to calculate frequency of tweets during event
## plot map of rivers and their popularity 