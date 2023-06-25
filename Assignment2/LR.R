setwd("/Users/lijiaqi/C/R")
bencare <- read.csv("BenCare_2022_C.csv")
bencare$LOYALTY <- as.factor(bencare$LOYALTY)
sex_male <- ifelse(bencare$sex == "1", 1, 0)
age_1 <- ifelse(bencare$age == "1", 1, 0)
age_2 <- ifelse(bencare$age == "2", 1, 0)
age_3 <- ifelse(bencare$age == "3", 1, 0)
age_4 <- ifelse(bencare$age == "4", 1, 0)
educ_1 <- ifelse(bencare$educ == "1", 1, 0)
educ_2 <- ifelse(bencare$educ == "2", 1, 0)
educ_3 <- ifelse(bencare$educ == "3", 1, 0)

bencare <- data.frame(LOYALTY = bencare$BLOY, Satis = bencare$Satis, VALUE = bencare$VALUE, 
                      Reputa = bencare$Reputa, Pricey = bencare$Pricey, ATRUST = bencare$ATRUST,
                      CTRUST = bencare$CTRUST, age_1 = age_1, age_2 = age_2, age_3 = age_3, age_4 = age_4,
                      sex_male = sex_male, educ_1 = educ_1, educ_2 = educ_2, educ_3 = educ_3)

cor(bencare[, -1])
glm.fits <- glm(
  LOYALTY ~ .,
  data = train.df, family = binomial
)

### logistic regression model ###
glm.fits1 <- glm(
  LOYALTY ~ Satis+VALUE+Reputa+sex_male+CTRUST+Reputa:sex_male+VALUE:Reputa,
  data = train.df, family = binomial
)
summary(glm.fits)
summary(glm.fits1)
glm.probs <- predict(glm.fits, type = "response")
glm.results1 <- ifelse(glm.probs > 0.5, 1, 0)

table(glm.results1, LOYALTY)
anova(glm.fits1, test="Chisq")

## split the train and test data
set.seed(1)  
train.index <- sample(c(1:dim(bencare)[1]), dim(bencare)[1]*0.6)  
train.df <- bencare[train.index, ]
valid.df <- bencare[-train.index, ]
train.df$LOYALTY <- as.factor(train.df$LOYALTY)
valid.df$LOYALTY <- as.factor(valid.df$LOYALTY)
glm.probs <- predict(glm.fits1, valid.df,
                     type = "response")
glm.results <- ifelse(glm.probs > 0.5, 1, 0)
misClassifier_Error <- mean(glm.results != valid.df$LOYALTY)
print(paste('Accuracy',1-misClassifier_Error))
glm.pred <- rep("Down", 122)
glm.pred[glm.probs > .5] <- "Up"


y <- ifelse(train.df$LOYALTY=='1',1, 0)
x <- data.matrix(train.df[, c('Satis', 'VALUE', 'Reputa', 'Pricey','ATRUST','CTRUST','age_1','age_2','age_3','age_4','sex_male'
                            ,'educ_1','educ_2','educ_3')])
library(glmnet)

#perform k-fold cross-validation to find optimal lambda value
cv_model <- cv.glmnet(x, y, alpha = 1,family = binomial,type.measure = 'mse')

#find optimal lambda value that minimizes test MSE
best_lambda <- cv_model$lambda.min
best_lambda


#produce plot of test MSE by lambda value
plot(cv_model) 
best_model <- glmnet(x, y, alpha = 1, lambda = best_lambda, family = binomial)
coef(best_model)

x_test <- data.matrix(valid.df[, c('Satis', 'VALUE', 'Reputa', 'Pricey','ATRUST','CTRUST','age_1','age_2','age_3','age_4','sex_male'
                                   ,'educ_1','educ_2','educ_3')])
y_test <- ifelse(valid.df$LOYALTY=='1',1, 0)
glm.probs2 <- predict(best_model, x_test, lambda = best_lambda,
                     type = "response")
x_test = as.data.frame(x_test)
glm.probs3 <- predict(glm.fits1, x_test,
                      type = "response")
## make ROC plot
library(ROCR)
pr <- prediction(glm.probs2, y_test)
pr <- prediction(glm.probs3, y_test)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

glm.results2 <- ifelse(glm.probs2 > 0.5, 1, 0)
glm.results3 <- ifelse(glm.probs3 > 0.5, 1, 0)
misClassifier_Error <- mean(glm.results2 != y_test)
misClassifier_Error1 <- mean(glm.results3 != y_test)

## make confusion matrix 
table(pred = glm.results2, true = y_test)
table(pred = glm.results3, true = y_test)

library(caret)
library(ggplot2)
library(lattice)

### normalized data
preproc.param <- train.df[,2:7] %>% 
  preProcess(method = c("center", "scale"))

train.transformed <- preproc.param %>% predict(train.df[,2:7])
test.transformed <- preproc.param %>% predict(valid.df[,2:7])
train.transform = as.data.frame(train.transformed)
test.transform = as.data.frame(test.transformed)
train.df[,2:7] <- train.transform
valid.df[,2:7] <- test.transform

### LDA model
library(MASS)
lda.fit <- lda(LOYALTY ~ .,
               data = train.df, family = binomial)
lda.fit
predictions <- lda.fit %>% predict(valid.df)
mean(predictions$class==valid.df$LOYALTY)

lda.fit1 <- lda(LOYALTY ~ VALUE+sex_male+Satis+age_1+age_4,
               data = train.df, family = binomial)
lda.fit1
predictions <- lda.fit1 %>% predict(valid.df)
mean(predictions$class==valid.df$LOYALTY)