library(ISLR)
library(MASS)
library(broom)
library(modelr)
library(tidyverse)
library(caret)
library(dplyr)
library(tibble)
library(glmnet)
data("College")
set.seed(1)
train=sample(c(TRUE,FALSE),nrow(College),rep=TRUE)
test=(!train)

College.train = College[train,]
College.test = College[test,]
lm.fit = lm(Apps~., data=College.train)
lm.pred = predict(lm.fit, College.test, type="response")
mean((lm.pred-College.test$Apps)^2)


set.seed(1)
#Set up matrices needed for the glmnet functions
train.mat = model.matrix(Apps~., data = College.train)
test.mat = model.matrix(Apps~., data = College.test)
#Choose lambda using cross-validation
cv.out = cv.glmnet(train.mat,College.train$Apps,alpha=0)
bestlam = cv.out$lambda.min
bestlam

#Fit a ridge regression
ridge.mod = glmnet(train.mat,College.train$Apps,alpha = 0)
#Make predictions
ridge.pred = predict(ridge.mod,s=bestlam,newx = test.mat)
#Calculate test error
mean((ridge.pred - College.test$Apps)^2)
#d
#Choose lambda using cross-validation
set.seed(1)
cv.out2 = cv.glmnet(train.mat,College.train$Apps,alpha=1)
bestlam2 = cv.out2$lambda.min
bestlam2
#Fit lasso model
lasso.mod = glmnet(train.mat,College.train$Apps,alpha=1)
#Make predictions
lasso.pred = predict(lasso.mod,s=bestlam2,newx=test.mat)
mean((lasso.pred - College.test$Apps)^2)


