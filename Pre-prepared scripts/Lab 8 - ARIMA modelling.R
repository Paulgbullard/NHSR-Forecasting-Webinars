# ------------Lab Session 8: ARIMA-----------------

ae_daily %>% autoplot() # look at daily data, do they look like stationary data?
ae_daily %>% features(n_attendance,unitroot_kpss)# what is tye output of KPSS test? does it tell if data is non-stationary?
ae_daily %>% features(n_attendance,unitroot_ndiffs)# how may first differencing you need to make series stationary?
# this will tell us what would be the vale of d
ae_daily %>% features(n_attendance,unitroot_nsdiffs)## how may seasonal differencing you need to make series stationary?
# this will tell us what would be the vale of D
ae_daily %>% ACF(n_attendance, lag_max = 21) %>% autoplot()# what does the ACF tell us? what are significant lags?
ae_daily %>% PACF(n_attendance) %>% autoplot()# what is PACF?
# how using ACF and PACF help us to determine the order of p, q? look at the properties of MA(q) and AR(p) in slides
# fir an automatic ARIMA model to train data, how automatic ARIMA works? 
# 1. how parameters are estimated? using maximum likelihood estimation
#how p and q are determined? using Information Criteria
fit <- train %>%
  model(arima = ARIMA(n_attendance))
# you can try fitting different ARIMA models  such as:
#arima = ARIMA(n_attendance ~ pdq(3,1,0)+PDQ(1,0,0)),
#arima1=ARIMA(n_attendance ~ pdq(1,1,1)+PDQ(0,0,1)

fcst_arima <- fit %>% forecast(h=42)  # forecast for 42 days
fcst_arima %>% autoplot(filter(ae_daily, year(date)>2015))# visualise it
fit %>% select(arima) %>% report()# look at a sumamry of the model with parameters
# can you calculate forecast accuracy for the automatic arima? use accuracy()

#-----Simulare ARIMA process

library(forecast)
# generate an arima model, arima.sim() generate an ARIMA model, check ?sim.arima()
ar1 <- arima.sim(n=1000, list(ar=.9))
forecast::Acf(ar1)# this 
forecast::Pacf(ar1)

ma1 <- arima.sim(n=1000, list(ma=-.9))
forecast::Acf(ma1)
forecast::Pacf(ma1)