#install packages
#install.packages("glmnet")
#install.packages("leaps")
#install.packages("forecast")
library(glmnet)
library(leaps)
library(forecast)
#Read data
Ben<-read.csv(file.choose())
#Find missing value
sum(is.na(Ben))
#Data preprocess Here, factor cannot be calculated because of the addition of standardized interaction items
#So first add the interaction item to Ben.norm, and then change the categorical such as age into factor
Ben<-Ben[,c(-11,-12)]
Ben.norm<-scale(Ben[,c(-10,-9,-8)])
Ben.norm<-cbind(Ben.norm,Ben[,c(8,9,10)])
Ben.norm$Rep.age<-Ben.norm$Reputa*Ben.norm$age
Ben.norm$Pricey.quad<-(Ben.norm$Pricey)^2
Ben$age<-as.factor(Ben$age)
Ben$sex<-as.factor(Ben$sex)
Ben$educ<-as.factor(Ben$educ)
Ben.norm$age<-as.factor(Ben.norm$age)
Ben.norm$sex<-as.factor(Ben.norm$sex)
Ben.norm$educ<-as.factor(Ben.norm$educ)
#Separate data for evaluate
set.seed(10)
train <- sample(1:nrow(Ben), nrow(Ben) / 1.25)
test <- (-train)
### K = 10 FOLD CROSS VALIDATION for subset regression
#This is a function customized by the teacher himself, in order to realize subset regression
predict.regsubsets <- function(object, newdata, id, ...) {
  form <- as.formula(object$call[[2]])
  mat <- model.matrix(form, newdata)
  coefi <- coef(object, id = id)
  xvars <- names(coefi)
  mat[, xvars] %*% coefi
}
set.seed(1)
k <- 10
n <- nrow(Ben.norm)
set.seed(1)
folds <- sample(rep(1:k, length = n))
cv.errors <- matrix(NA, k, 11,
                    dimnames = list(NULL, paste(1:11)))#
for (j in 1:k) {
  best.fit <- regsubsets(LOYALTY ~ .,
                         data = Ben.norm[folds != j, ],
                         nvmax = 11)
  for (i in 1:11) {
    pred <- predict(best.fit, Ben.norm[folds == j, ], id = i)
    cv.errors[j, i] <-
      mean((Ben.norm$LOYALTY[folds == j] - pred)^2)
  }
}
mean.cv.errors <- apply(cv.errors, 2, mean)
mean.cv.errors
which.min(mean.cv.errors)
#RUN 10 SUSBET REGRESSION FOR FULL DATA TO GET DETAILED ANALYSIS
reg.best <- regsubsets(LOYALTY ~ ., data = Ben.norm,
                       nvmax = 11)
coef(reg.best,10)
#Build subset regression model
Ben$educ4<-rep(0,length(Ben$educ))
Ben$educ4[Ben$educ==4]<-1
subsetlm<-lm(LOYALTY~Satis+VALUE+Reputa+CTRUST+educ4,data=Ben[train,])
summary(subsetlm)
predsubset<-predict(subsetlm,Ben[test,])
accuracy(predsubset,Ben[test,]$LOYALTY)
##K = 10 FOLD CROSS VALIDATION for ridge regression
#The training set is divided according to the ratio of 50:50 in the teacher's courseware
#This cv.glmnet function will output the optimal lamda for cross-check, that is, 10% for 50% of the data volume
set.seed(1)

x <- model.matrix(LOYALTY ~ ., Ben.norm)[, -1]
y <- Ben.norm$LOYALTY
train <- sample(1:nrow(x), nrow(x) / 2)
test <- (-train)
y.test <- y[test]
cv.out <- cv.glmnet(x[train, ], y[train], alpha = 0)
plot(cv.out)
bestlam <- cv.out$lambda.min
bestlam
###OBTAIN coefficients VALUES FOR BEST-LAMDA

out <- glmnet(x, y, alpha = 0)
predict(out, type = "coefficients", s = bestlam)
#BUild ridge model
ridgelm<-lm(LOYALTY~Satis+VALUE+ATRUST+CTRUST+Reputa,data=Ben[train,])
summary(ridgelm)
predridge<-predict(ridgelm,Ben[test,])
accuracy(predridge,Ben[test,]$LOYALTY)
##K = 10 FOLD CROSS VALIDATION for lasso regression
set.seed(222)
cv.out.lasso <- cv.glmnet(x[train, ], y[train], alpha = 1)
plot(cv.out)
bestlam.lasso <- cv.out.lasso$lambda.min
bestlam.lasso
###OBTAIN coefficients VALUES FOR BEST-LAMDA

out <- glmnet(x, y, alpha = 1, lambda = bestlam.lasso)
summary(out)
predict(out, type = "coefficients", s = bestlam.lasso)
#BUild lasso model
lassolm<-lm(LOYALTY~Satis+VALUE+CTRUST+Reputa,data=Ben)
summary(lassolm)
predrlasso<-predict(lassolm,Ben[test,])
accuracy(predrlasso,Ben[test,]$LOYALTY)
plot(cv.out.lasso$glmnet.fit, "lambda", label=FALSE)

