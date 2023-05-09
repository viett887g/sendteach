# In the lab, a classification tree was applied to the Carseats data set after
# converting Sales into a qualitative response variable. Now we will seek to 
# predict Sales using regression trees and related approaches, treating the
# response as a quantitative variable.
#a: Split the data set into a training and test set.
library(tree)
library(ISLR)
attach(Carseats)
set.seed(1)
train <- sample(1:nrow(Carseats), nrow(Carseats)/2)
car.train <- Carseats[train, ]
car.test <- Carseats[-train, ]
# Part b: Fit a regression tree to the trianing set. Plot the tree, and interpret the results. What test MSE do you obtain?
car.tree <- tree(Sales ~ ., data = car.train)
plot(car.tree)
text(car.tree, pretty=0)
summary(car.tree)
car.pred <- predict(car.tree, newdata = car.test)
mean((car.pred - car.test$Sales)^2)
#c Use cross-validation in order to determine the optimal level of tree complexity. Does pruning the tree improve the test MSE?
set.seed(1)
car.cv <- cv.tree(car.tree)
par(mfrow = c(1, 2))
plot(car.cv$size, car.cv$dev, type = "b")
plot(car.cv$k, car.cv$dev, type = "b")

par(mfrow = c(1,1))
# It looks like 7 is the best size.
prune.car <- prune.tree(car.tree, best = 7)
plot(prune.car)
text(prune.car, pretty = 0)

predict.prune <- predict(prune.car, newdata = car.test)
mean((predict.prune - car.test$Sales)^2)
#d Use the bagging approach in order to analyze this data. What test MSE do
# you obtain? Use the importance() function to determine which variables are most
# important.
library(randomForest)
set.seed(1)
bag.car <- randomForest(Sales ~ ., data = Carseats, subset = train, mtry = 10,
                        importance = TRUE)
bag.car
predict.bag <- predict(bag.car, newdata = car.test)
mean((predict.bag - car.test$Sales)^2)
importance(bag.car)
varImpPlot(bag.car)
#e: Use random forests to analyze this data. What test MSE do you obtain?
# Use the importance() function to determine which variables are most important.
# Describe the effect of m, the number of variables considered at each split, on 
# the error rate obtained.
set.seed(1)
rf.car1 <- randomForest(Sales ~ ., data = Carseats, subset = train, mtry = 1,
                        importance = TRUE)
set.seed(1)
pred.rf1 <- predict(rf.car1, newdata = car.test)
mean((pred.rf1 - car.test$Sales)^2)

set.seed(1)
rf.car2 <- randomForest(Sales ~ ., data = Carseats, subset = train, mtry = 2,
                        importance = TRUE)
set.seed(1)
pred.rf2 <- predict(rf.car2, newdata = car.test)
mean((pred.rf2 - car.test$Sales)^2)

set.seed(1)
rf.car <- randomForest(Sales ~ ., data = Carseats, subset = train, mtry = 3,
                       importance = TRUE)
set.seed(1)
pred.rf <- predict(rf.car, newdata = car.test)
mean((pred.rf - car.test$Sales)^2)
#tang m=mtry len toi 9 dung vong lap cung duoc
importance(rf.car)
varImpPlot(rf.car)
rm(list = ls())
