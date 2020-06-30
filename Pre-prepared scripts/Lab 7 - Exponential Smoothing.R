#-------------- Lab Session 7 EXPONENTIAL SMOOTHING------------------
# fitting ETS model
#E: Error, T:Trend, S: Seasonality
#N: No trend, no seasonality
#A  additive, Ad: additive dapmed
# M: multiplicative
# the function for ets models in fable is ETS
# if we don't provide arguments, then it is an automatic ETS
# check ?ETS
ets_model <- train %>%
  model(
    #automatic_ets = ETS(n_attendance),
    ses = ETS(n_attendance ~ error("A")+trend("N")+season("N")),
    holt_winter = ETS(n_attendance ~ error("A")+trend("A")+season("A")),
    ets_paramet = ETS(n_attendance ~ error("A")+trend("A", alpha=.2, phi=.2)+season("A", gamma=.3)))

glance(ets_model)# it provides you with a summary of mable,
ets_model %>% select(ets) %>% tidy()# provide 
ets_model %>% select(ets) %>% report()# provide a nice output of models with parameters estimated
# to forecast, we pass mable to forecast()
ets_model %>% select(ses) %>% components()

fcst_ets <- ets_model %>%
  forecast(h = "42 days") 
# we cam visualise the forecast
fcst_ets%>%
  autoplot(filter_index(ae_daily,"2016-02"~.), level = NULL)

# can you do TSCV with ETS? basically, you have already done most of the work
train_tr <- train %>% # split data into folds with increasing size
  slice(1:(n()-42)) %>%
  stretch_tsibble(.init = 4*365, .step = 1)
ets_tr <- train_tr %>% model(automatic_ets=ETS()) 
ets_fcst_tr <- ets_tr%>% forecast(h=test)
fc_accuracy <- ets_fcst_tr %>% accuracy(train,measures = list(
  point_accuracy_measures,
  interval_accuracy_measures
)) 
fc_accuracy %>% select(.model,RMSE, MAE,winkler)

#monthly time series-----

ae_monthly <- ae_hourly %>% 
  index_by(month=yearmonth(arrival_1h)) %>% 
  summarise(n_attendance=sum(n_attendance))
f_horizon <- 6
test <- ae_monthly %>% slice((n()-(f_horizon-1)):n())
train <- ae_monthly %>% slice(1:(n()-f_horizon))
nrow(ae_monthly)==(nrow(test)+nrow(train))
# specify, train and forecast for the following model
# ets1 with additive error, no trend, additive seasonality, ETS(n_attendance ~ error("A")+trend("N")+season("A"))
# ets2 with additive error, no trend, no seasonality and alpha=0.2, ETS(n_attendance ~ error("A")+trend("N", alpha=0.2)+season("N"))
train_tr <- train %>% slice(1:(n()-f_horizon)) %>% 
  stretch_tsibble(.init = 4*12, .step = 1)
ae_fit <- train_tr %>% 
  model(ets1=ETS(n_attendance ~ error("A")+trend("N")+season("A")),
        ets2=ETS(n_attendance ~ error("A")+trend("N", alpha=0.2)+season("N")))
ae_fc <- ae_fit %>% forecast(h=f_horizon)
ae_accuracy <- ae_fc %>% accuracy(train, measures=list(point_accuracy_measures, interval_accuracy_measures))
ae_accuracy %>% select(.model, RMSE, MAE, winkler)
ae_fit1 <- train %>% model(ets1=ETS(n_attendance ~ error("A")+trend("N")+season("A")))
ae_fit1 %>% gg_tsresiduals()
ae_fit1 %>% report()
ae_fit1 %>% components() %>% View()
ae_fit1 %>% glance()
#prediction intervals
ae_fc1 <- ae_fit1 %>% forecast(h=6)
ae_fc1 %>% hilo(level=c(90,99)) %>% unnest()
ae_fc1 %>%
  hilo(level = 99) %>%
  mutate(lower = `99%`$lower,
         upper = `99%`$upper)
ae_fc1 %>% autoplot(ae_monthly)
