# -------------------------Lab Session 4-------------------------

ae_daily %>% gg_lag(n_attendance, lags = c(1:14), geom = "point")# create lag plots for 14 lags, from 1 to 14
ae_daily %>% ACF(lag_max = 14)# compute autocorrelation fucntion
ae_daily %>% ACF(n_attendance, lag_max = 14) %>% autoplot()# plot acf

ae_daily %>% gg_tsdisplay()# plot time plot, acf and season plot, check ?gg_tsdisplay

ae_daily %>% features(n_attendance, ljung_box, dof=0)# use ljung box to test wether ACF is significant, if p-value is amll, << 0.05 them there is a significant autocorrelation 

# what autocorrelation will tell us? whu=ich key features could be highlighted by ACF?

## ----make any graph using ggplot2 ----
#You can create any graph that helps you to better understand data!
# I recommed you to try geom_box() which is helpful to better understand the variations

# here I tried to seee if attendance of female is different over the weekend comparing to the weekday
weekend_an_weekday <- ae_hourly %>% group_by(gender) %>% 
  summarise(n_attendance=sum(n_attendance)) %>% 
  mutate(
    Date=lubridate::as_date(arrival_1h),
    hour=lubridate::hour(arrival_1h),
    Day = lubridate::wday(arrival_1h, label = TRUE),
    Weekend = (Day %in% c("Sun", "Sat"))) %>% 
  filter(gender=="female") 
weekend_an_weekday %>% ggplot(aes(x = hour, y = n_attendance)) +
  geom_line(aes(group=Date)) +
  facet_grid(Weekend ~., scales="free_y")
