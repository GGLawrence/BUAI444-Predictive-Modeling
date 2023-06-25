bencare <- read.csv('BenCare_2022_ASN10.csv')
summary(bencare)
bencare$age <- as.factor(bencare$age)
bencare$sex <- as.factor(bencare$sex)
bencare$educ <- as.factor(bencare$educ)

sex_male <- ifelse(bencare$sex == "1", 1, 0)
age_1 <- ifelse(bencare$age == "1", 1, 0)
age_2 <- ifelse(bencare$age == "2", 1, 0)
age_3 <- ifelse(bencare$age == "3", 1, 0)
age_4 <- ifelse(bencare$age == "4", 1, 0)
educ_1 <- ifelse(bencare$educ == "1", 1, 0)
educ_2 <- ifelse(bencare$educ == "2", 1, 0)
educ_3 <- ifelse(bencare$educ == "3", 1, 0)
bencare <- data.frame(LOYALTY = bencare$LOYALTY, Satis = bencare$Satis, VALUE = bencare$VALUE, 
                       Reputa = bencare$Reputa, Pricey = bencare$Pricey, ATRUST = bencare$ATRUST,
                      CTRUST = bencare$CTRUST, age_1 = age_1, age_2 = age_2, age_3 = age_3, age_4 = age_4,
                      sex_male = sex_male, educ_1 = educ_1, educ_2 = educ_2, educ_3 = educ_3)
##### center the numerical variables #####
library(dplyr)
bencare1 <- bencare2 %>% mutate_at(c(1:7), ~(scale(.) %>% as.vector))

bencare <- bencare[ -c(8:10) ]
##### initial model #####
ols <- lm(LOYALTY ~ Satis+VALUE+Reputa+Pricey+ATRUST+CTRUST+age_1+age_2+age_3+age_4+sex_male+educ_1+educ_2+educ_3
          , data = bencare)
plot(ols, which = 3)

##### final model #####
ols <- lm(LOYALTY~log(ATRUST)+I(CTRUST^2)+VALUE*Reputa+Satis, data = bencare2)
summary(ols)



ols1 <- lm(LOYALTY ~ (Satis+VALUE+Reputa+Pricey+ATRUST+CTRUST+age_1+age_2+age_3+age_4+sex_male+educ_1+educ_2+educ_3)^2
          , data = bencare)

##### remove the outliers #####
cook <- cooks.distance(ols)
influential <- cook[(cook>4/289)]
plot(cook, ylab = 'Cooks Distance')

library(dplyr)
names_of_influential <- names(influential)
outliers <- bencare[names_of_influential,]
bencare2 <-  bencare %>% anti_join(outliers)


##### diagonostics ######
par(mfrow = c(2, 2))
plot(ols)

plot(predict(ols), residuals(ols))
plot(hatvalues(ols))

anova(ols1)


plot(predict(ols), residuals(ols))
plot(hatvalues(ols))

library(car)
library(carData)
library(VIF)

vif(ols)
cor(bencare)


library(MASS)
rr.huber <- rlm(LOYALTY ~ Reputa+log(ATRUST)+Satis+VALUE:Pricey+I(CTRUST^2)
                   , data = bencare2)
summary(rr.huber)
hweights <- data.frame(Satis = bencare2$Satis, resid = rr.huber$resid, weight = rr.huber$w)
hweights2 <- hweights[order(rr.huber$w), ]
hweights2[1:15,]
rr.bisquare <- rlm(LOYALTY ~ Reputa+log(ATRUST)+Satis+VALUE:Pricey+I(CTRUST^2)
                   , data = bencare2, psi = psi.bisquare)
summary(rr.bisquare)
biweights <- data.frame(Satis = bencare2$Satis, resid = rr.huber$resid, weight = rr.bisquare$w)
biweights2 <- biweights[order(rr.bisquare$w), ]
biweights2[1:15, ]
library(MASS)
sqrt(vif(ols)) > 2
crPlots(ols)
library(gvlma)
gvmodel <- gvlma(ols)
summary(gvmodel)

plot(hatvalues(ols))
which.max(hatvalues(ols))
