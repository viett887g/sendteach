#Generate a simulated two-class data set with 100 observations and two features in which there is a visible but non-linear separation between the two classes. Show that in this setting, 
#a support vector machine with a polynomial kernel (with degree greater than 1) or a radial kernel will outperform a support vector classifier on the training data. Which technique performs best on the test data? Make plots and report training and test error rates in order to back up your assertions.
set.seed(1)
transl <- 3
X <- matrix(rnorm(100 * 2), ncol = 2)
X[1:30, ] <- X[1:30, ] + transl
X[31:60, ] <- X[31:60, ] - transl
y <- c(rep(0, 60), rep(1, 40))
dat <- data.frame(x = X, y = as.factor(y))
plot(X, col = y + 1)
#Chia thành tập huấn luyện và kiểm tra:
train <- sample(100, 80)
dat.train <- dat[train, ]
dat.test <- dat[-train, ]
#dieuchinh du lieju roi ve
library(e1071)
svm.lin <- svm(y ~ ., data = dat.train, kernel = 'linear', scale = FALSE)
plot(svm.lin, data = dat.train)
summary(svm.lin)
#Tính toán sai số huấn luyện của SVM phanlop
table(predict = svm.lin$fitted, truth = dat.train$y)
svm.poly <- svm(y ~ ., data = dat.train, kernel = 'polynomial', scale = FALSE)
plot(svm.poly, data = dat.train)
table(predict = svm.poly$fitted, truth = dat.train$y)
#Có nhieu  dự đoán đúng.
svm.rad <- svm(y ~ ., data = dat.train, kernel = 'radial', scale = FALSE)
plot(svm.rad, data = dat.train)
table(predict = svm.rad$fitted, truth = dat.train$y)
#khong co du doan nao sai tat ca deu dung het
lin.pred <- predict(svm.lin, dat.test)
table(predict = lin.pred, truth = dat.test$y)

poly.pred <- predict(svm.poly, dat.test)

rad.pred <- predict(svm.rad, dat.test)
table(predict = rad.pred, truth = dat.test$y)
table(predict = poly.pred, truth = dat.test$y)