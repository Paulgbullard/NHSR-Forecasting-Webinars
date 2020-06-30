# -------------------------Lab Session 6 Time series cross validation-------------------------

f_horizon <- 42# forecast horizon
test <- ae_daily %>% slice((n()-(f_horizon-1)):n())# create test set equal to forecast horizon
train <- ae_daily %>% slice(1:(n()-f_horizon))# craete train set
nrow(ae_daily)==(nrow(test)+nrow(train))# check if split is correct, the result should be TRUE
ae_model <- train %>% model(SNAIVE(n_attendance))# specify and train SNAIVE method
ae_model %>% forecast(test) %>% # forecast and visualise it, you can provide the test set when using forecast() instead of h=42
  autoplot(filter_index(ae_daily,"2016"))

