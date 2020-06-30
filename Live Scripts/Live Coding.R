library(fpp3)
library(readr)

#------- Lab Session 1---------------
#1.1-Import data into R
ae_uk_original <- readr::read_csv("Data/ae_uk.csv", 
                                  col_types = cols( 
                                    arrival_time=col_datetime(format = "%d/%m/%Y %H:%M"),
                                    gender=col_character(),
                                    type_injury=col_character()))

#2.1- check duplications and fix it
ae_uk_original %>% 
  duplicated() %>% 
  sum()#check duplicates
ae_wd <- ae_uk_original %>% 
  dplyr::distinct(.)# remove duplicates and get a distinct tibble

nrow(ae_uk_original)-nrow(ae_wd) #check the number of duplication if you want

#3.1- create tsibble
ae_tsb <- ae_wd %>% 
  as_tsibble(key = c(gender,type_injury),
             index = arrival_time, 
             regular=FALSE)
# if you start working with a irregular index, you need to  use `regular=FALSE` in as_tsibble
# regularise an irregular index, create a new tsibble  
ae_hourly <- ae_tsb %>% 
  group_by(gender,type_injury) %>% 
  index_by(arrival_1h = lubridate::floor_date(arrival_time, "1 hour")) %>% 
  summarise(n_attendance=n())

# 4.1. check implicit NA / gaps in time
has_gaps(ae_hourly)#check gaps

scan_gaps(ae_hourly)# show mw gaps

count_gaps(ae_hourly)# coun gaps

# if there is any gap, them fill it with zero
ae_hourly <- ae_tsb %>% 
  group_by(gender, type_injury) %>% 
  index_by(arrival_1h = lubridate::floor_date(arrival_time, "1 hours")) %>% 
  summarise(n_attendance=n()) %>% 
  fill_gaps(n_attendance=0L) %>% 
  ungroup()
#you can use `index_by()` and `summarise()` to regularise index
# ae_hourly is a tsibble with regular space of 1 hour, you can change it to any interval,e.g. "2 hours","3 hours", etc or create any granular level from the hourly series such as daily, weekly , etc
# create a daily series to work with a single time series, in tsibble you can work many time series, go to lab session 12 for more information
ae_daily <- ae_hourly %>% 
  index_by(year_day=as_date(arrival_1h)) %>% 
  summarise(n_attendance=sum(n_attendance))

ae_daily_keys <- ae_hourly %>% group_by(gender, type_injury) %>% 
  index_by(year_day=as_date(arrival_1h)) %>% 
  summarise(n_attendance=sum(n_attendance))

ae_weekly <- ae_hourly %>% 
  index_by(weekly=yearweek(arrival_1h)) %>% 
  summarise(n_attendance=sum(n_attendance))

ae_weekly_keys <- ae_hourly %>% group_by(gender, type_injury) %>% 
  index_by(weekly=yearweek(arrival_1h)) %>% 
  summarise(n_attendance=sum(n_attendance))

ae_monthly <- ae_hourly %>% 
  index_by(monthly=yearmonth(arrival_1h)) %>% 
  summarise(n_attendance=sum(n_attendance))

ae_monthly_keys <- ae_hourly %>% group_by(gender, type_injury) %>% 
  index_by(monthly=yearmonth(arrival_1h)) %>% 
  summarise(n_attendance=sum(n_attendance))

ae_quarterly <- ae_hourly %>% 
  index_by(quarter=yearquarter(arrival_1h)) %>% 
  summarise(n_attendance=sum(n_attendance))

ae_quarterly_keys <- ae_hourly %>% group_by(gender, type_injury)
  index_by(quarter=yearquarter(arrival_1h)) %>% 
  summarise(n_attendance=sum(n_attendance))

ae_daily %>%autoplot()
ae_weekly %>% autoplot()
ae_monthly %>% autoplot()

#save ae_daily
write_rds(ae_daily, "Data/ae_daily.rds")
write_rds(ae_hourly, "Data/ae_hourly.rds")

#######
#Live coding here:
#######

#seasons

ae_daily %>% gg_season(n_attendance)
ae_daily %>% gg_season(n_attendance, period = "week")
ae_daily %>% gg_season(n_attendance, period = "month")
ae_daily %>% gg_subseries(n_attendance, period = "week")

#ACF

ae_daily %>% gg_lag(n_attendance, lags = c(1:14), geom = "point")
ae_daily %>% ACF(lag_max = 14)
ae_daily %>% ACF(lag_max = 21) %>% autoplot()

#Show all...
ae_daily %>% gg_tsdisplay()

#Significant ACF? Small p-value means significant.
ae_daily %>% features(n_attendance, ljung_box, dof = 0)

#########################
#Simple forecasting methods
#fit model
ae_fit <- ae_daily %>% 
  model(mean = MEAN(n_attendance),
        naive = NAIVE(n_attendance),
        snaive = SNAIVE(n_attendance, lag = "week"),
        drift = RW(n_attendance ~ drift())
  )

model2 <-  ae_daily_keys %>% 
  model(mean = MEAN(n_attendance),
        naive = NAIVE(n_attendance),
        snaive = SNAIVE(n_attendance, lag = "week"),
        drift = RW(n_attendance ~ drift())
  )

#view mable (model table)
model1  

#View some information
model1 %>%  select(simpleaverage) %>%  glance()

#Forecast!
all_forecast <- ae_fit %>% 
  forecast(h = 42)

all_forecast %>% autoplot(filter_index(ae_daily, "2016" ~ .))

#Residual diagnostics

ae_fit %>% augment() %>% filter(.model == "snaive") %>% select(.resid) %>% ACF() %>% autoplot()

ae_fit %>% select(mean) %>% gg_tsresiduals()
ae_fit %>% select(naive) %>% gg_tsresiduals()
ae_fit %>% select(snaive) %>% gg_tsresiduals()
ae_fit %>% select(drift) %>% gg_tsresiduals()

#Time series cross validation

f_horizon <- 42

ae_daily_test <- ae_daily %>% slice((n()-(f_horizon-1)):n())
ae_daily_train <- ae_daily %>% slice(1:(n()-f_horizon))

train_XV <- ae_daily_train %>% 
  slice(1:(n()-f_horizon)) %>% 
  stretch_tsibble(.init = 5 * 365, .step = 7)

ae_fit_XV <- train_XV %>% 
  model(mean = MEAN(n_attendance),
        naive = NAIVE(n_attendance),
        snaive = SNAIVE(n_attendance, lag = "week"),
        drift = RW(n_attendance ~ drift()),
        #automatic_ets=ETS(n_attendance),
        my_ets=ETS(n_attendance ~ error("A")+trend("A", alpha = 0.2)+season("M", gamma = 0.2))
  )

all_forecast_XV <- ae_fit_XV %>% 
  forecast(h = 42)


fc_accuracy <- all_forecast_XV %>% 
  accuracy(ae_daily_train, measures = list(point_accuracy_measures, interval_accuracy_measures))

fc_accuracy %>% select(.model, RMSE, MAE, winkler)


###### Follow along coding

ae_monthly

f_horizon = 6

test <- ae_monthly %>% slice((n() - (f_horizon-1)):n())
train <- ae_monthly %>% slice(1:(n()-f_horizon))
nrow(ae_monthly) ==(nrow(test)+nrow(train))

train_XV <- train %>% 
  slice(1:(n()-f_horizon)) %>% 
  stretch_tsibble(.init = 4*12, .step = 1)

test_fit <- train_XV %>% 
  model(ets1=ETS(n_attendance ~ error("A")+trend("N")+season("A")),
        ets2=ETS(n_attendance ~ error("A")+trend("N", alpha = 0.2)+season("N"))
  )

ae_fc <- test_xv %>% 
  forecast(h = f_horizon)

ae_acc <- ae_fc %>% 
  accuracy(train, measures = list(point_accuracy_measures, interval_accuracy_measures))

ae_acc %>% select(.model, RMSE, MAE, winkler) #ets1 has the smallest error

ae_fit1 <- train %>% model(ets1 = ETS(n_attendance ~ error("A")+trend("N")+season("A")))

ae_fit1 %>% gg_tsresiduals()

ae_fit1 %>% report()
ae_fit1 %>% components() %>% autoplot()

ae_fc1 <- ae_fit1 %>% forecast(h=f_horizon)

ae_fc1 %>% hilo(level=99) %>% mutate(lower = `99%`$lower,
                                     upper = `99%`$upper)

ae_fc1 %>% autoplot(ae_monthly)


#########
#ARIMA


ar1 <- arima.sim(n=1000, list(ar=.9))
acf(ar1)
pacf(ar1)

fit <- ae_daily %>% 
  model(arima = ARIMA(n_attendance)) #automaticall selects p,d,q,P,D,Q

fit %>% report()

fcst_arima <- fit %>% forecast(h = 42)
fcst_arima %>% autoplot()


##############
#Regression

library(GGally)

                        