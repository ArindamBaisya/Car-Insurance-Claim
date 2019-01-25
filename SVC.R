train=read.csv("train.csv")
test=read.csv("test.csv")
test$C_Claim=ifelse(test$C_Claim>0,"Yes","No")
train$C_Claim=ifelse(train$C_Claim>0,"Yes","No")

train$C_Claim=as.factor(train$C_Claim)
test$C_Claim=as.factor(test$C_Claim)

maj <- train[train$C_Claim=="No",]
set.seed(2)
subid <- sample(1:dim(maj)[1], size=720*2, replace=FALSE)
ids <- rbind(train[train$C_Claim=="Yes",],maj[subid,])
table(ids$C_Claim)
summary(ids)
library(e1071)

svmfit=svm(C_Claim~., data=ids , kernel ="linear", cost=0.1, scale=FALSE)

ypred=predict(svmfit ,test)
table(predict =ypred , truth=test$C_Claim)

