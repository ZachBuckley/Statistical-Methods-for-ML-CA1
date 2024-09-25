library(MASS)
library(caret)
ca.train = read.csv("C:\\Users\\zachb\\OneDrive\\Documents\\Machine Learning and Math Modelling\\Semester 2\\ST4061 - Statistical Methods for Machine Learning II\\CA1\\ca_train.csv",stringsAsFactors=TRUE)
ca.test = read.csv("C:\\Users\\zachb\\OneDrive\\Documents\\Machine Learning and Math Modelling\\Semester 2\\ST4061 - Statistical Methods for Machine Learning II\\CA1\\ca_test.csv",stringsAsFactors=TRUE)

#Question 1
#(1)
#View the data
par(mfrow=c(1,1))
plot(ca.train$entry,ca.train$exit, pch=c(19,17)[ca.train$academy], col=c(1,2)[ca.train$y], cex=1, xlab = "Entry", ylab = "Exit")
legend(185,720,col=c(1,2),legend=levels(ca.train$y),pch=20, bty='n', title = "Hired")
legend(150,720,pch=c(19,17),legend=levels(ca.train$academy), bty='n', title = "Academy")

#Fit LDA model
lda.o = lda(y~., data=ca.train)
lda.o

#Confusion matrix
lda.p = predict(lda.o, newdata=ca.test)$class
caret::confusionMatrix(lda.p, ca.test$y, positive = "Yes")$table

#(2)
#Fit QDA model
qda.o = qda(y~., data=ca.train)
qda.o

#Confusion matrix
qda.p = predict(qda.o, newdata=ca.test)$class
caret::confusionMatrix(qda.p, ca.test$y, positive = "Yes")$table

#(3)
#Specificity = (True negatives)/(True negatives + False positives)
#LDA specificity
caret::confusionMatrix(lda.p, ca.test$y, positive = "Yes")$byClass[2]
#0.998431 

#QDA specificity
caret::confusionMatrix(qda.p, ca.test$y, positive = "Yes")$byClass[2]
#0.9973849

#Both models have a very high specificity value with the LDA model having a slightly higher value.
#This means this model gives very few false positive results - i.e. the models rarely predicts that a
#player will be hired when in fact they don't get hired. It is very good at correctly identifying cases
#where a player does not get hired. High specificity may come at a cost of low sensitivity

#(4)
#LDA Sensitivity
caret::confusionMatrix(lda.p, ca.test$y, positive = "Yes")$byClass[1:2]
#0.1688312   
#QDA sensitivity
caret::confusionMatrix(qda.p, ca.test$y, positive = "Yes")$byClass[1:2]
#0.2337662   

#LDA may give a slightly higher specificity value due to having lower sensitivity than the QDA model.
#Lower sensitivity means the model will give many false negative results - i.e. the model will often predict a 
#player wont get hired when in fact they do. Having a higher specificity often comes at a cost of lowering the sensitivity

#The difference may also be due to the underlying structure of the data. LDA assumes that the covariance matrices for each
#predictor is the same while QDA doesn't.




