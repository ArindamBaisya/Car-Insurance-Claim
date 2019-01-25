#reads test & train data
train=read.csv("train.csv")
test=read.csv("test.csv")
#converts C_Claim from 0,1 to Yes/No
test$C_Claim=ifelse(test$C_Claim>0,"Yes","No")
train$C_Claim=ifelse(train$C_Claim>0,"Yes","No")
#converts them to factors
train$C_Claim=as.factor(train$C_Claim)
test$C_Claim=as.factor(test$C_Claim)
set.seed(1)


library(MASS)
#ld fit
ld.fit=lda(C_Claim~., data=train)

coef(ld.fit)
lda.pred=predict(ld.fit, test, type="response")
#I got best results with 0.9935
lda.cl=ifelse(lda.pred$posterior[,1]<0.9935, "Yes", "No")

table(lda.cl,test$C_Claim)
