library(ISLR) # for the data
library(gbm)
library(randomForest) 

x.train = Khan$xtrain
x.test = Khan$xtest
y.train = as.factor(Khan$ytrain)
y.test = as.factor(Khan$ytest)

#Check dimensions
dim(x.train)
length(y.train)

#(a)
#Table for y.train
table(y.train)
#Frequency
table(y.train)/length(y.train)

#Table for y.test
table(y.test)
#Frequency
table(y.test)/length(y.test)

#The test data appears to have a similar distributions.
#Both distributions are not sparse. i.e. several samples in each level
#The test data distribution has significantly more points in the 3rd level

#(b)This is a classification problem. The target variable (y) can take a number of fixed classes
#1, 2, 3, 4. The goal of regression is to predict a continuous value and the goal of 
#classification is to predict a categorical value.

#(c)
set.seed(4061)
#Grow random forest
rf.out = randomForest(y.train~., data= x.train)
#Find models predictions for training data
rf.yhat = predict(rf.out,  x.train, type="class")
#Generate confusion matrix
(tb.rf = table(rf.yhat, y.train))

#(d)
#Generate predictions for test set
rf.pred = predict(rf.out,  x.test, type="class")
#Check confusion matrix
(tb.rf = table(rf.pred, y.test))

#Find prediction accuracy
sum(diag(tb.rf)) / sum(tb.rf)
#95% accuracy

#(e)
#Plot importance
par(mfrow=c(1,1))
varImpPlot(rf.out, pch=15, main="Variable Importance Plot")
abline(v = 0.4,col = "blue")

#Find importance
imp = rf.out$importance
idx <- which(imp > 0.4) # row numbers
cbind(idx,imp[idx])
#509  545  566  742 1003 1319 1389 1708 1954 2046 2050

#(f) #The Gini index was used as a measure of variable importance. It is a measure of impurity in a tree.
# The Gini index of a node is the probability of incorrectly classifying a random element in the node.
# Variables that result in the largest decrease of Gini index are considered more important.

#(g)
set.seed(4061)
#Create data frames
CS.x.train = data.frame(x.train)
CS.x.test = data.frame(x.test)

#Create GB model 
gbm.out = gbm(y.train~.,data = CS.x.train)

#Find predictions for test set
gbm.p = predict(gbm.out, newdata=CS.x.test,type = "response")

#Find class predictions by taking the highest probability
class_names = colnames(gbm.p)[apply(gbm.p, 1, which.max)]
result = data.frame(y.test, class_names)

#Create confusion matrix
(tb.gbm= table(result))
#Find accuracy
(acc.gbm = sum(diag(tb.gbm)) / sum(tb.gbm))
#100% accuracy on test data

