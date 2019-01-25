#This piece of code reads revdata from directry
#and splits it into train & test data in the proportion 80:20
#Finally it writes test and train
data=read.csv("revdata.csv")
data$Claim_Amount=as.factor(data$Claim_Amount)
data<-data[-c(1,2,35)]
set.seed(1);
n = nrow(data)
trainIndex = sample(1:n, size = round(0.8*n), replace=FALSE)
train = data[trainIndex ,]
test = data[-trainIndex ,]
write.csv(train,"train.csv")
write.csv(test,"test.csv")
