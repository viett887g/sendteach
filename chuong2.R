library(ISLR)
#library(MASS)
#library(broom)
#library(modelr)
#library(tidyverse)
#library(caret)
#library(dplyr)
#library(tibble)
#library(glmnet)
Auto <- read.csv(file = "C:/Users/ADMIN/Downloads/Auto.csv")
Auto
data("Auto", package = "ISLR")
pairs(Auto)
#c
lm.fit1 <-  lm(mpg ~ . - name, data = Auto)
summary(lm.fit1)
#i. Is there a relationship between the predictors and the response? 
#Vâng, có một mối quan hệ giữa các yếu tố dự đoán và phản hồi bằng cách kiểm tra giả thuyết không về việc liệu tất cả các hệ số hồi quy có bằng không hay không. 
#Thống kê F khác xa 1 (với giá trị p nhỏ), cho thấy bằng chứng chống lại giả thuyết không.

#ii. Which predictors appear to have a statistically significant relationship to the response?
#Nhìn vào các giá trị p được liên kết với thống kê t của từng yếu tố dự đoán, chúng ta thấy rằng chuyển vị, trọng lượng, năm và nguồn gốc có mối quan hệ có ý nghĩa thống kê, trong khi xi lanh, mã lực và gia tốc thì không.

#iii. What does the coefficient for the `year` variable suggest?
#Hệ số hồi quy cho năm, 0,7507727, cho thấy rằng cứ sau một năm, mpg tăng theo hệ số. Nói cách khác, ô tô trở nên tiết kiệm nhiên liệu hơn mỗi năm gần 1 mpg / năm

#e Use the * and : symbols to fit linear regression models with interaction effects. Do any interactions appear to be statistically significant?
lm.fit2 <-  lm(mpg ~ cylinders * displacement + displacement * weight, data = Auto)
summary(lm.fit2)
#Tương tác giữa chuyển vị và trọng lượng có ý nghĩa thống kê, trong khi tương tác giữa xi lanh và chuyển vị thì không.

#f Try a few diﬀerent transformations of the variables log(x) , sqrt(x),x^2
x=Auto[,"mpg"]
x=mean(x)
log(x)
sqrt(x)
x*x