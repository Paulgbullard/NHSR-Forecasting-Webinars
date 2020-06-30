# -------Lab Session 2------------
# time plot
# create a daily series to work with a single time series, in tsibble you can work many time series, go to lab session 12 for more information

#this will give you total number of attendance each day
ae_daily <- ae_hourly %>% 
  index_by(year_day=as_date(arrival_1h)) %>% 
  summarise(n_attendance=sum(n_attendance))

#if you want to play with different timeseries, try the following too

#this will give you daily attendance for each combination of gender and type_injury
ae_daily_key <- ae_hourly %>% group_by(gender, type_injury) %>% 
  index_by(year_day=as_date(arrival_1h)) %>% 
  summarise(n_attendance=sum(n_attendance))

# this gives you total monthly attendance for each combination of gender and type_injury
ae_monthly_key <- ae_hourly %>% group_by(gender, type_injury) %>% 
  index_by(month=yearmonth(arrival_1h)) %>% 
  summarise(n_attendance=sum(n_attendance))

# this gives you total monthly attendance for each date
ae_monthly <- ae_hourly %>% 
  index_by(month=yearmonth(arrival_1h)) %>% 
  summarise(n_attendance=sum(n_attendance))

# this gives you total monthly attendance for each quarter
ae_quarterly <- ae_hourly %>% 
  index_by(quarter=yearquarter(arrival_1h)) %>% 
  summarise(n_attendance=sum(n_attendance))

##-----time series graphics
ae_daily %>% autoplot(n_attendance) # create a time plot of daily data
# you can use filter_index or head() and tail() to plot a subset of data , try ?filter_index
ae_daily %>% tsibble::filter_index("2016-02") %>% autoplot(n_attendance)
ae_daily %>% head(n=100) %>% autoplot()
ae_daily %>% tail(n=100) %>% autoplot()
ae_hourly %>% autoplot(n_attendance) # create a time plot of hourly data

#you can also plot monthly time series
ae_monthly %>% 
  autoplot(n_attendance) +
  labs(y = "attendances", x="Month",
       title = "Monthly A&E attendance",
       subtitle = "UK hospital")