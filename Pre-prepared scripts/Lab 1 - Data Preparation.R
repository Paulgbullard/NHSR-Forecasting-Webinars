library(fpp3)
library(readr)

#------- Lab Session 1---------------
#1.1-Import data into R
ae_uk_original <- readr::read_csv("data/ae_uk.csv", 
                                  col_types = cols( 
                                    arrival_time=col_datetime(format = "%d/%m/%Y %H:%M"),
                                    gender=col_character(),
                                    type_injury=col_character()))

#2.1- check duplications and fix it
ae_uk_original %>% duplicated() %>% sum()#check duplicates
ae_wd <- ae_uk_original %>% dplyr::distinct()# remove duplicates and get a distinct tibble
# nrow(ae_uk_original)-nrow(ae_wd) check the number of duplication if you want
#3.1- create tsibble
ae_tsb <- ae_wd %>% 
  as_tsibble(key = c(gender,type_injury), index = arrival_time, regular=FALSE)
# if you start working with a irregular index, you need to  use `regular=FALSE` in as_tsibble
# regularise an irregular index, create a new tsibble  
ae_hourly <- ae_tsb %>% group_by(gender,type_injury) %>% 
  index_by(arrival_1h = lubridate::floor_date(arrival_time, "1 hour")) %>% 
  summarise(n_attendance=n())

# 4.1. check implicit NA / gaps in time
has_gaps(ae_hourly)#check gaps
scan_gaps(ae_hourly)# show mw gaps
count_gaps(ae_hourly)# coun gaps
# if there is any gap, then fill it with zero
ae_hourly <- ae_tsb %>% group_by(gender, type_injury) %>% 
  index_by(arrival_1h = lubridate::floor_date(arrival_time, "1 hours")) %>% 
  summarise(n_attendance=n()) %>% fill_gaps(n_attendance=0L) %>% ungroup()
#you can use `index_by()` and `summarise()` to regularise index
# ae_hourly is a tsibble with regular space of 1 hour, you can change it to any interval,e.g. "2 hours","3 hours", etc or create any granular level from the hourly series such as daily, weekly , etc
# create a daily series to work with a single time series, in tsibble you can work many time series, go to lab session 12 for more information
ae_daily <- ae_hourly %>% 
  index_by(year_day=as_date(arrival_1h)) %>% 
  summarise(n_attendance=sum(n_attendance))
