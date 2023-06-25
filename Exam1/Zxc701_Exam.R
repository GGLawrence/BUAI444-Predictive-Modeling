
###Problem1

#Reading CSV File
drugdata <- read.csv('REVCO_GM.csv')
drugdata

#Stepwise Regression
REF <- ifelse(drugdata$REF. == 'YES', 1, 0)
REF
null<-lm(GM ~ 1,data=drugdata)
full<-lm(GM ~ COST + REF,data=drugdata)
stepreg<-step(null,scope=list(lower=null,upper=full),direction="both")
summary(stepreg)
coefficients(stepreg)


#Detecting Ouliers
stepreg$residuals
rstandard(stepreg)
plot_rstd<-plot(rstandard(stepreg))
cook<-cooks.distance(stepreg)
cook

#Testing Homoscedasticity
plot(stepreg$fitted.values, stepreg$residuals)
zres<-rstandard(stepreg)
plot(stepreg$fitted.values, zres)

#Testing Linearity

drugdata$REF <- ifelse(drugdata$REF. == 'YES', 1, 0)
drugdata$REF
plot(drugdata$COST, zres)
plot(drugdata$REF, zres)

#Test for Normality
hist(stepreg$residuals)
qqnorm(stepreg$residuals)
qqline(stepreg$residuals)
shapiro.test(stepreg$residuals)

#Test of Independence
#install.packages("lmtest")
library(lmtest)
dwtest(stepreg, alternative = "two.sided")




###Problem2

# Read Data
hoteldata <- read.csv("Hotel_occupancy.csv", header=T)
hoteldata
hoteldata$Rooms

#Define radioseries as a timeseries
hotelseries=ts(hoteldata$Rooms, frequency=12, start = c(2008,1))
hotelseries
plot.ts(hotelseries)

library("forecast")

#Additive Model
hotelforecasts<-ets(hotelseries,model="AAA", lower = c(0.1,0.1,0.1, 0.8), upper = c(0.5,0.5,0.5,0.95), damped=FALSE)
summary(hotelforecasts)
forecast(hotelforecasts, h=12, prediction.interval = TRUE, level=0.95)

#Multiplicative Model
hotelforecasts<-ets(hotelseries,model="MAM", lower = c(0.1,0.1,0.1, 0.8), upper = c(0.5,0.5,0.5,0.95), damped=FALSE)
summary(hotelforecasts)
forecast(hotelforecasts, h=12, prediction.interval = TRUE, level=0.95)

#ARIMA model
hotelauto<- auto.arima(hotelseries,
  max.p = 10,
  max.q = 10,
  max.P = 10,
  max.Q = 10,
  max.order = 10,
  max.d = 10,
  max.D = 10,
  seasonal = TRUE)

summary (hotelauto)
accuracy(hotelauto)
coef(hotelauto)
hotelfuture<-forecast(hotelauto, h=12)
hotelfuture
plot(hotelfuture)
str(hotelfuture)

hotelseriesarima<- Arima(hotelseries,order=c(0,0,4),seasonal = c(0,1,1), include.constant = T)
summary(hotelseriesarima)
accuracy(hotelseriesarima)

#Residual acf and pacf
acf(hotelauto$residuals, type="correlation", lag.max = 20)
pacf(hotelauto$residuals, lag.max =20)

#Box-Ljung Test
Box.test(hotelseries, lag = 3, type="Ljung-Box")

#Partial Autocorrelation
Pacf(hotelseries, lag.max=20)
Pacf(hotelseries,lag.max = 20, plot=F)









