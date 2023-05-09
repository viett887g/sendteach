#a Create a training set containing a random sample of 800 observations, and a test set containing the remaining observations. Hint: You can use the “sample” function or “createDataPartition” function in the “Caret” package to split the dataset. Use set.seed(1000)
suppressMessages(library(ISLR))
suppressMessages(library(tree))

oj = OJ
names(oj) = tolower(names(oj))

set.seed(1000)
index = sample(1:nrow(oj), 800)
train = oj[index,]
test = oj[-index,]
purchase.test = oj$purchase[-index]
#b. Fit a tree to the training data, with Purchase as the response and the other variables as predictors. Use the summary() function to produce summary statistics about the tree, and describe the results obtained. What is the training error rate? How many terminal nodes does the tree have? Hint: Use tree package.
tree.oj = tree(purchase ~ ., train)

summary(tree.oj)
tree.train.error = 17.5#Training-Error-rate = 17.5 %
tree.train.accuracy = 100-tree.train.error
#c. Type in the name of the tree object in order to get a detailed text output. Pick one of the terminal nodes, and interpret the information displayed.
tree.oj
#d. Create a plot of the tree, and interpret the results.
plot(tree.oj)
text(tree.oj, pretty=0)
e.#e Predict the response on the test data, and produce a confusion matrix comparing the test labels to the predicted test labels. What is the test error rate?
pred = predict(tree.oj, test, type="class")

table(pred, purchase.test)
tree.test.accuracy = round(mean(pred == purchase.test)*100,2)
tree.test.accuracy
tree.test.error = round(mean(pred != purchase.test)*100,2)
tree.test.error
#f. Apply the cv.tree() function to the training set in order to determine the optimal tree size.
set.seed(1000)
cv.oj = cv.tree(tree.oj, FUN = prune.misclass)
cv.oj
#g. Produce a plot with tree size on the x-axis and cross-validated classification error rate on the y-axis.
plot(cv.oj$size, cv.oj$dev, type="b")
#h h. Which tree size corresponds to the lowest cross-validated classification error rate?
#kích thước cây “4” dường như có tỷ lệ lỗi xác thực chéo thấp nhất là 157. thậm chí kích thước “4” có tỷ lệ lỗi thấp nhất, chúng tôi sẽ chọn kích thước “2” vì tỷ lệ lỗi của kích thước “2” chỉ là một chút cao hơn ở mức 158 và cũng không có sự cải thiện lớn nào về tỷ lệ lỗi khi tăng từ 2 lên 4 trong biểu đồ trên. điều này sẽ đơn giản hóa cây và tăng khả năng diễn giải mà vẫn có tỷ lệ lỗi CV gần như giống nhau.
#i. Produce a pruned tree corresponding to the optimal tree size obtained using cross-validation. If cross-validation does not lead to selection of a pruned tree, then create a pruned tree with five terminal nodes.
prune.oj = prune.misclass(tree.oj, best=2)
summary(prune.oj)
plot(prune.oj)
text(prune.oj, pretty=0)
prune.train.error = 18.75#Training-Error-rate (pruned tree) = 18.75 %
prune.train.accuracy = 100-prune.train.error
#j. Compare the training error rates between the pruned and unpruned trees. Which is higher?
#tỷ lệ lỗi huấn luyện của cây được cắt tỉa là 18,75% so với cây không được cắt tỉa là 17,5%. tỷ lệ lỗi đào tạo của cây bị cắt tỉa cao hơn và điều đó có nghĩa là bằng cách cắt tỉa, chúng tôi đã giảm tính linh hoạt trong mô hình và do đó, lỗi đào tạo đã tăng lên một chút do Xu hướng tăng mặc dù phương sai của mô hình giảm.
#k. Compare the test error rates between the pruned and unpruned trees. Which is higher?
prune.pred = predict(prune.oj, test, type="class")

table(prune.pred, purchase.test)
prune.test.accuracy = round(mean(prune.pred == purchase.test)*100,2)
prune.test.accuracy
prune.test.error = round(mean(prune.pred != purchase.test)*100,2)
prune.test.error
error.df = data.frame(Model_type = c("unPruned","Pruned w/ Term.nodes=2"), Training_Error_rate = c(tree.train.error,prune.train.error), Test_Error_rate = c(tree.test.error, prune.test.error))
error.df