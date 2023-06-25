getwd()
setwd('/Users/lijiaqi/C/R')
data <- read.csv('subscription.csv',header = T)
##### original model #####
FEB <- ifelse(data$month == 'FEB', 1, 0)
MAR <- ifelse(data$month == 'MAR', 1, 0)
APR <- ifelse(data$month == 'APR', 1, 0)
MAY <- ifelse(data$month == 'MAY', 1, 0)
JUN <- ifelse(data$month == 'JUN', 1, 0)
JUL <- ifelse(data$month == 'JUL', 1, 0)
AUG <- ifelse(data$month == 'AUG', 1, 0)
SEP <- ifelse(data$month == 'SEP', 1, 0)
OCT <- ifelse(data$month == 'OCT', 1, 0)
NOV <- ifelse(data$month == 'NOV', 1, 0)
DEC <- ifelse(data$month == 'DEC', 1, 0)
data_reg <- data.frame(tot_mem = data$tot_mem, tot_call = data$tot_call, avg_tem = data$avg_temp, 
                       deg_ht = data$deg_ht, pr_rain = data$pr_rain, pr_snow = data$pr_snow,
                       age = data$age,FEB = FEB, MAR = MAR, APR = APR, MAY = MAY,
                       JUN = JUN, JUL = JUL, AUG = AUG, SEP = SEP, OCT = OCT,
                       NOV = NOV, DEC = DEC)
full <- lm(tot_call~age+deg_ht+avg_tem+pr_rain+tot_mem+pr_snow+MAR+DEC+FEB+SEP+APR+MAY+AUG+OCT+NOV, data=data_reg)

null <- lm(tot_call~tot_mem, data = data_reg)
full <- lm(tot_call~tot_mem+age+deg_ht+avg_tem+pr_snow+MAR+DEC+FEB+SEP, data = data_reg)
stepreg <- step(null, scope=list(lower=null,upper=full),trace = 0)


#####linearity#####

a <- extractAIC(stepreg)
zres <- rstandard(stepreg)
plot(data_reg$pr_snow, zres)
plot(data_reg$age, zres)
plot(data_reg$deg_ht, zres)
plot(data_reg$avg_tem, zres)
cook <- cooks.distance(stepreg)
plot(cook, ylab = 'Cooks Distance')
par(mfrow = c(1,3))
#####homoscedasticity#####
plot(stepreg$fitted.values, stepreg$residuals)
zres <- rstandard(stepreg)
plot(stepreg$fitted.values, zres)
#####normality#####
hist(stepreg$residuals)
qqnorm(stepreg$residuals)
qqline(stepreg$residuals)
shapiro.test(stepreg$residuals)
#####independence#####
plot(data_reg$tot_call, stepreg$residuals)