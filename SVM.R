train=read.csv("train.csv")
test=read.csv("test.csv")
test$C_Claim=ifelse(test$C_Claim>0,"Yes","No")
train$C_Claim=ifelse(train$C_Claim>0,"Yes","No")

train$C_Claim=as.factor(train$C_Claim)
test$C_Claim=as.factor(test$C_Claim)


maj <- train[train$C_Claim=="No",]
subid <- sample(1:dim(maj)[1], size=720*1.1, replace=FALSE)
ids <- rbind(train[train$C_Claim=="Yes",],maj[subid,])
table(ids$C_Claim)
summary(ids)

set.seed(2);
library(e1071)
library(caret)

svmfit=svm(C_Claim~., data=ids, kernel ="polynomial", gamma=0.1,cost=5)
ypred=predict(svmfit ,test,decision.values = T)
ypred1=attributes(predict(svmfit ,test,decision.values = T))
table(predict =ypred , truth=test$C_Claim )

tuned = tune.svm(C_Claim~., data = ids, gamma = 0.1, cost = 5, tunecontrol=tune.control(cross=10))
summary(tuned)

library(ROCR)
rocplot =function (pred , truth , ...){ predob = prediction (pred , truth) 
perf = performance (predob , "tpr", "fpr") 
plot(perf ,...)}
rocplot(ypred1$decision.values,test$C_Claim)
abline(0,1)

