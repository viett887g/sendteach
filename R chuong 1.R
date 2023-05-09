library(rlang)
library(dplyr)
library(ISLR)
library(dplyr)
library(tibble)
library(tidyverse)
library(magrittr)
#This exercise involves the Auto data set studied in the lab. 
#Make sure that the missing values have been removed from the data.
#a) Which of the predictors are quantitative, and which are qualitative?
Auto <- read.csv("C:/Users/ADMIN/Downloads/Auto.csv", 
                   header=TRUE,
                   na.strings = "?")
str(Auto)
Auto=na.omit(Auto)
print(Auto)
#b What is the range of each quantitative predictor?
apply(Auto[,1:6], 2, range)
#C. What is the mean and standard deviation of each quantitative predictor?
#mean
options(width = 95)
apply(Auto[,1:6], 2, mean)
#SD
options(width = 95)
apply(Auto[,1:6], 2, sd)
#D. Now remove the 10th through 85th observations. What is the range, mean, and standard deviation of each predictor in the subset of the data that remains?
apply(Auto[-c(10:85),1:6], 2, range)
options(width = 95)
apply(Auto[-c(10:85),1:6], 2, mean)
apply(Auto[-c(10:85),1:6], 2, sd)
#E. Using the full data set, investigate the predictors graphically, using scatterplots and other tools of your choice. Create some plots (at least 3) highlighting the relationships among the predictors. Comment on your findings.
pairs(Auto)
plot(Auto$acceleration, Auto$weight)
plot(Auto$horsepower, Auto$mpg)
plot(Auto$year, Auto$mpg)
