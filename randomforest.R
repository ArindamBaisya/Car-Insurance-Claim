id=read.csv("revdata.csv",header=T)[,2:35]
str(id)
id<-id[,-1]
str(id)
id$Claim_Amount=as.factor(ifelse(id$Claim_Amount>0,"Yes","No"))
id$Blind_Model=as.factor(id$Blind_Model)
id$Blind_Submodel=as.factor(id$Blind_Submodel)
str(id)

# subample the super majority class to reduce the ratio to 10:1
maj <- id[id$Claim_Amount=="No",]
subid <- sample(1:dim(maj)[1], size=720*2, replace=FALSE)
ids <- rbind(id[id$Claim_Amount=="Yes",],maj[subid,])
table(ids$Claim_Amount)
summary(ids)


# we will also have a validation set from the original data
valSamples <- sample(1:dim(id)[1], size=dim(id)[1]/10, replace=FALSE)
validation <- id[valSamples,]


sample.size=floor(nrow(ids)*0.8)
train=sample(seq_len(nrow(ids)),size = sample.size)
traindata=ids[train,]
testdata=ids[-train,]


table(traindata$Claim_Amount)


set.seed(1)
rf.df=randomForest(Claim_Amount~.,data=traindata,importance=T, prox=TRUE, strata=traindata$Claim_Amount, sampsize=c(88,80))
summary(rf.df)
yhat=predict(rf.df,newdata=testdata,type="class")
confusionMatrix<-table(yhat,testdata$Claim_Amount)
confusionMatrix
confusionMatrix[1,2]/sum(confusionMatrix[,2])


# make prediction from the validation set
validation.pred1 <- predict(rf.df,newdata=validation)
# compare it to the actual outcome
confusionMatrix <- table(predicted=validation.pred1,observed=validation$Claim_Amount)
confusionMatrix
confusionMatrix[1,2]/sum(confusionMatrix[,2])

