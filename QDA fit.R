train=read.csv("train.csv")
test=read.csv("test.csv")
train=train[-c(4,5,6)]
test=test[-c(4,5,6)]
test$C_Claim=ifelse(test$C_Claim>0,"Yes","No")
train$C_Claim=ifelse(train$C_Claim>0,"Yes","No")

train$C_Claim=as.factor(train$C_Claim)
test$C_Claim=as.factor(test$C_Claim)

library(MASS)

qd.fit<-qda(C_Claim~Vehicle+Calendar_Year+Blind_Make+Blind_Model+Cat1+Cat2+Cat3+Cat4+Cat5+Cat6+Cat7+Cat8+Cat9+Cat10+Cat11+OrdCat+Var1+Var2+Var3+Var4+Var5+Var6+Var7+Var8+NVVar1+NVVar2+NVVar3+NVVar4, data=train)

#coef(qd.fit)summ
qd.class=predict(qd.fit,test)
qda.cl=ifelse(qd.class$posterior[,2]<0.9935, "Yes", "No")

table(qda.cl,test$C_Claim)

rocplot =function (pred , truth , ...){ predob = prediction (pred , truth) 
perf = performance (predob , "tpr", "fpr") 
plot(perf ,...)}
rocplot(ypred1$decision.values,test$C_Claim)
abline(0,1)
