hw2 <- read.csv('HW2_CSV.csv',header = T)
xt <- hw2$Xt
timeseries = ts(xt, start = c(2014,1),frequency = 12)
plot.ts(timeseries)

### b. ### 
library('forecast')
### apply additive trend-seasonal model
hw2_forecast <- ets(timeseries, model = 'AAA', alpha = 0.2, beta = 0.1, gamma = 0.1, damped = FALSE)
### RMSE = 15.27477
                                    
hw2_forecast$fitted
plot(hw2_forecast)
accuracy(hw2_forecast)
forecast(hw2_forecast, level=0.95, h=12)

### c. ###
hw2_forecast1 <- ets(timeseries, model = 'MAM', alpha = 0.2, beta = 0.1, gamma = 0.1, damped = FALSE)
### RMSE = 13.1621
hw2_forecast1$fitted
plot(hw2_forecast1)
accuracy(hw2_forecast1)
forecast(hw2_forecast1, level=0.95, h=12)
### d. ###
hw2_forecast2 <- ets(timeseries, model = 'MAM', lower = c(0.1, 0.1, 0.1, 0.8), upper = c(0.5, 0.5, 0.5, 0.9), damped = F)
fcast <- forecast(hw2_forecast2, level=0.95, h=12)
accuracy(hw2_forecast2) ### RMSE = 13.21017
plot(hw2_forecast2)
hw2_forecast4 <- ets(timeseries, model = 'AAA', lower = c(0.1, 0.1, 0.1, 0.8), upper = c(0.5, 0.5, 0.5, 0.9), damped = F)
plot(hw2_forecast4)
fcast1 <- forecast(hw2_forecast4, level=0.95, h=12)
accuracy(hw2_forecast4) ### RMSE = 14.23307


aust <- window(timeseries,start=2014)
fit1 <- hw(aust,
           alpha = 0.1571,
           beta = 0.1,
           gamma = 0.1,seasonal="multiplicative",
           damped = FALSE)
fit2 <- hw(aust,
           alpha = 0.4999, 
           beta  = 0.1001,
           gamma = 0.1008,
           seasonal="additive",
           damped = FALSE)
autoplot(aust) +
  autolayer(fit1, series="HW multiplicative forecasts", PI=FALSE) +
  autolayer(fit2, series="HW additive forecasts",
            PI=FALSE) 
 
aust <- window(timeseries,start=2014)
fit1 <- hw(aust,
           alpha = 0.1571,
           beta = 0.1,
           gamma = 0.1,seasonal="multiplicative",
           damped = FALSE)
fit2 <- hw(aust,
           alpha = 0.4999, 
           beta  = 0.1001,
           gamma = 0.1008,
           seasonal="additive",
           damped = FALSE)
autoplot(aust) +
  autolayer(fit1, series="HW multiplicative forecasts", PI=FALSE) +
  autolayer(fit2, series="HW additive forecasts",
            PI=FALSE) 

