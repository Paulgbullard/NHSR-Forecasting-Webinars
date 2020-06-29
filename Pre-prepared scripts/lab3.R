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
    automatic_ets = ETS(n_attendance),
    ses = ETS(n_attendance ~ error("A")+trend("N")+season("N")),
    holt_winter = ETS(n_attendance ~ error("A")+trend("A")+season("A")))

glance(ets_model)# it provides you with a summary of mable,
ets_model %>% select(ets) %>% tidy()# provide 
ets_model %>% select(ets) %>% report()# provide a nice output of models with parameters estimated
# to forecast, we pass mable to forecast()
fcst_ets <- ets_model %>%
  forecast(h = "42 days") 
# we cam visualise the forecast
fcst_ets%>%
  autoplot(filter_index(ae_daily,"2016-02"~.), level = NULL)
# calculate forecast accuracy for test set
fcst_ets %>% accuracy(test) %>% select(.model,RMSE,MAE)

# can you do TSCV with ETS? basically, you have already done most of the work
ets_tr <- train_tr %>% model(automatic_ets=ETS()) 
ets_fcst_tr <- ets_tr%>% forecast(h=test)