# ------Lab Session 5 fittig model, specify and estimate in fable-----
# start with simple benchmark method: average, naive, snaive, drift
#how to specify a model MODELNAME(forecastvariable ~ term1+term2+...+term n), if there is no term ignore ~ term1+term2+...+termn
#---fitting: specify models and estimate parameters using model()---

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
# you can then use filter() to extract information for any model and select  .fitted or .resid
#you can replace .model== with anymodel you have use in tye model(), line 5-11
ae_fit %>% augment() %>% filter(.model=="snaive")%>% select(.fitted)
ae_fit %>% augment() %>% filter(.model=="mean")%>% select(.resid)

#-----residual diagnostic------
ae_fit %>% filter(.model=="mean") %>% gg_tsresiduals()# check residual for the fitted/trained model, do they look like white noise/random?
ae_fit %>% filter(.model=="mean") %>% 
  augment() %>% features(.resid, ljung_box, lag=14,dof=0)
# ljung_box to check residuals,  do you reject the null hypothesis? H0: Time series are random