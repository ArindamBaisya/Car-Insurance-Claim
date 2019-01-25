train=read.csv("train.csv")
test=read.csv("test.csv")
test$C_Claim=ifelse(test$C_Claim>0,"Yes","No")
train$C_Claim=ifelse(train$C_Claim>0,"Yes","No")

train$C_Claim=as.factor(train$C_Claim)
test$C_Claim=as.factor(test$C_Claim)
log.fit<-glm(C_Claim~.,data = train, family= binomial)
summary(log.fit)
coef(log.fit)
summary(log.fit)$coef
log.prob=predict(log.fit,test, type="response")
log.prob[40:60]
glm.pred=rep("No" ,20000)
glm.pred[log.prob >.0099]="Yes"
table(glm.pred,test$C_Claim)
