# ---------Lab Session 9: regression-------------
# when doing time series forecasting, we need to have always one single tsibble containing all variables
temp <- read_csv("data/temp.csv") #import the temperature data
ae_daily <- ae_daily %>% rename(date=year_day)# rename year_day from ae_daily to ensuare we have a common variable with temp, 
#I need this to join dataset and create a single tsibble with all data we need two 
ae_uk_temp <- inner_join(ae_daily,temp, by="date")# join both ae_daily and temp to create one single tibble 
# when doing time series forecasting, we need to have always one single tsibble containing all variables
ae_uk_temp <- ae_uk_temp %>% filter(date <= ymd("2016-03-31"))# filter data if date is smaller than end of march 2016 
ae_uk_temp_tsbl <- ae_uk_temp %>% as_tsibble(index = date)# make it a tsibble, this time there is not key, but we have two measures for each index

se <- read_csv("Data/se.csv")# read a dataset containing dummay variables
se <- se %>% mutate(date=lubridate::dmy(date))# observe the date variable in se:
#is it a date object? what is it format? why do we need to use dmy()?
all_my_ae <- inner_join(ae_uk_temp_tsbl,se, by="date")# join dummy variable data to the previous datset


# check wether temp and n_attendance has a linear relationship
ggplot(ae_uk_temp_tsbl, aes(x=temp, y=n_attendance))+
  geom_point()
# you can also use GGally::ggpairs()
# we split the data into train and test as before
f_horizon <- 42
test <- ae_uk_temp_tsbl %>% slice((n()-f_horizon-1):n())
tarin <- ae_uk_temp_tsbl %>% slice(1:(n()-f_horizon))

# we train data using a regression model
# TSLM is a Time Series Linera Regression Model
# in the left hand side of TSLM(), we have the variable we want to forecast
# in the right hand side, we have all variable we use as predictor
# trend() is just a linear trend increasing with time, you may want to check trend(knots=)
fit <- tarin %>% 
  model(lr= TSLM(n_attendance ~ trend()+season(),
                 lr_temp= TSLM(n_attendance ~ trend()+season()+temp),
                 lr_se=TSLM(n_attendance ~ trend()+season()+temp+ `Black Friday`+`Christmas Day`+`Halloween Day`+`New Years Day`)
  )
  )
fit %>% select(lr_se) %>% report()# we can check the output of any regtression model
# which predictor is significant?
# what are s0,s1, s2,...

# we pass mable to forecast(), attention: here you have to pass test set to new_data=test,
# try this and see what you get: fcast <- fit %>% forecast(h="42 days") 
fcast <- fit %>% forecast(new_data=test) 
# we calculate accuracy by model
fcast %>% accuracy(ae_uk_temp_tsbl, by=".model") %>% 
  select(.model,RMSE,MAE)# we select RMSE,MAE for each model
fcast %>% autoplot(filter(ae_uk_temp_tsbl, year(date)>2015))# we can visualise forecast

## ----Time series cross validation with exogenious variables----
# how do we do TSCV when having exogenious variables? I provide the code without explaining it
# try to replicate and understand the code!
f_horizon <- 42
tarin_tr <- tarin %>% slice(1:(n()-f_horizon)) %>%
  stretch_tsibble(.init = 4*365, .step = 1)
m_date <- tarin %>% pull(date)
test_tr <- tarin %>% filter(date>m_date[4*365]) %>% 
  slide_tsibble(.size = f_horizon, .step = 1, .id = ".id")

fit_tr <- tarin_tr %>% 
  model(lr_temp= TSLM(n_attendance ~ trend()+season()+temp),
        lr= TSLM(n_attendance ~ season(),
                 lr_se=TSLM(n_attendance ~ trend()+season()+temp+ `Black Friday`+`Christmas Day`+`Halloween Day`+`New Years Day`)
        )
  )
fcast_tr <- fit_tr %>% forecast(new_data=test_tr, h="42 days")
acc <- fcast_tr %>% accuracy(test_tr)
