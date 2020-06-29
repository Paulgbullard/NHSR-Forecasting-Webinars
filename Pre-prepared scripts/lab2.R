# ------Lab Session 5 fittig model, specify and estimate in fable-----
# start with simple benchmark method: average, naive, snaive, drift
# fitting: specify models and estimate parameters using model()

ae_fit <- ae_daily %>%
  model(
    mean = MEAN(n_attendance),
    naive = NAIVE(n_attendance),
    snaive = SNAIVE(n_attendance),
    drift = RW(n_attendance ~ drift())
  )
# ae_fit is called mable, it is model table, each row belongs to one time series and each column to a model
# to produce forecasts, we pass ae_fit, the mable objet into the forecast()
af_fc <- ae_fit %>% forecast(h="42 days")
# forecast needs the forecat horizon as an argument
af_fc %>% autoplot(filter_index(ae_daily,"2016"~.), level=NULL)
# we can plot generated forecasts using models, if you don;t want to plot prediction intervals, then level=NULL
ae_fit %>% augment() # what is the outpur of this line of code?
#you can extrat fitted values and residuals for each model usig augment()
# you can then use filter() to extract infoation for any model and select  .fitted or .resid
ae_fit %>% augment() %>% filter(.model=="snaive")%>% select(.fitted)
ae_fit %>% augment() %>% filter(.model=="mean")%>% select(.resid)

# -------------------------Lab Session 6 Time series cross validation-------------------------

f_horizon <- 42# forecast horizon
test <- ae_daily %>% slice((n()-(f_horizon-1)):n())# create test set equal to forecast horizon
train <- ae_daily %>% slice(1:(n()-f_horizon))# craete train set
nrow(ae_daily)==(nrow(test)+nrow(train))# check if split is correct, the result should be TRUE
ae_model <- train %>% model(SNAIVE(n_attendance))# specify and train SNAIVE method
ae_model %>% forecast(test) %>% # forecast and visualise it, you can provide the test set when using forecast() instead of h=42
  autoplot(filter_index(ae_daily,"2016"))

ae_model %>% gg_tsresiduals()# check residual for the fitted/trained model, do they look like white noise/random?
augment(ae_model) %>% features(.resid, ljung_box, lag=14,dof=0)
# ljung_box to check residuals,  do you reject the null hypothesis? H0: Time series are random

# using time series cross validation(TSCV)/ rolling forecast
# why do we use TSCV?
# how do we do TSCV in R? 1. create different folds/time series 2. model/train each fold, 3. forecast for each fold and given forecast horizon
#1. Create different timeseries using stretch_tsibble
train_tr <- train %>% # split data into folds with increasing size
  slice(1:(n()-42)) %>%
  stretch_tsibble(.init = 4*365, .step = 1)
# .init is the size of initial time series, .step is the increment step >=1
# what is the purpose of using slice(1:(n()-42))?
# how many time series/fold we create with this process? what is te new variable .id?
# 2. train models for each time series related to each fold
ae_mode_tr <- train_tr %>%
  model(
    mean = MEAN(n_attendance),
    naive = NAIVE(n_attendance),
    drift = RW(n_attendance ~ drift()),
    snaive = SNAIVE(n_attendance),
    est=ETS(n_attendance)
  )
ae_mode_tr# observe mable
# Produce forecast for h=42 or 42 days
ae_fc_tr <- ae_mode_tr %>% forecast(h=42)
ae_fc_tr#observe fable
View(ae_fc_tr[1:43,])# in ae_fc_tr( a fable object) each .id is representing the forecast for each fold/ series creates
# if you want to get the forecast generated for each fold/series, each model and across all forecast horizon, then
#group_by() ,id and ,model first and then create a new variable called h using row_number() for forecast horizon
# check ?row_number
ae_fc <- ae_fc_tr %>% 
  group_by(.id,.model) %>% 
  mutate(h=row_number()) %>% ungroup()
View(ae_fc[1:43,])# view the first 43 rows of ae_fc toobserve h

# ca;lculate forecast accuracy both point forecast and prediction interval using accuracy()
# accuracy() need both the forecast object/fable and all data in training test
# what happens if you use train_tr, instead of train in the following code
fc_accuracy <- ae_fc %>% accuracy(train,measures = list(
  point_accuracy_measures,
  interval_accuracy_measures
)) 
# you can specify which accuracy measure you want using measures = list()
# if use  ae_fc %>% accuracy(train), it calculates only point forecast
fc_accuracy %>% group_by(.model) %>% 
  summarise(RMSE=mean(RMSE),
            MAE=mean(MAE),
            winkler=mean(winkler))
# we can report the forecast accuracy using RMSE, MAE and winkler
# the accuracy measure is calculated for each fold/series create and then we average across all foled(using mean())
# this will give us an average accuracy measure across all folds