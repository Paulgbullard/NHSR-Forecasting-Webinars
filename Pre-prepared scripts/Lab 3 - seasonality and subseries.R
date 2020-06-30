#---- Lab Session 3--------
# use seasonal and subseries plots to check wether series contain seasonality 
ae_daily %>% gg_season(n_attendance) 
ae_daily %>% gg_subseries(n_attendance)

ae_hourly %>% gg_season(n_attendance) 
ae_hourly %>% gg_season(n_attendance,period = "day")# change period to period = "week"
ae_hourly %>% gg_subseries(n_attendance)

# How do you create a seasonal plot for the monthly and quartely series series
ae_hourly %>% index_by(year_month=yearmonth(arrival_1h)) %>%
  summarise(n_attendance=sum(n_attendance)) %>% 
  gg_season()# replace gg_season with gg_subseries()

#Question: is there any seasonality in the daily time series? what about hourly and monthly?
