#Kaggle Competition - Rossmann Store Sales

#Read input
dir = 'input/'
store = read.csv(paste0(dir,"store.csv"))
train = read.csv(paste0(dir,"train.csv"))
test = read.csv(paste0(dir,"test.csv"))

#Data Cleanup
##Fix NAs in Open in test
test$Open[which(is.na(test$Open))] = 0

##Remove Customers from train and date from both
train = train[,c(-3,-5)]
test = test[,-4]

##Merge datasets together for feature engineering
test$Sales = 'dummy'
train$Id = 'dummyID'
train = rbind(train, test)

# Separate the test from train
test = train[train$Sales=="dummy",]
test$Sales = NULL
train = train[train$Sales !="dummy",]
train$Id = NULL

#Testing Model
##Setup cross validation set
library(caret)

##Seed for consistent results
set.seed(100)

part = createDataPartition(train$Sales, p = 0.6, list = FALSE)
trainset = train[part,]
testset = train[-part,]

##Setup Linear model for CV
lmTrain = lm(Sales ~ ., trainset)
predTrain = predict(lmTrain, trainset)
predTest = predict(lmTrain, newdata=testset)

#Prediction
##Create a linear model
lmModel = lm(Sales ~ ., train)
predModel = predict(lmModel, newdata = test)

#Creates submission
submission = data.frame(test$Id)
names(submission) = 'ID'
submission$Sales = predModel
write.csv(submission, file="sales.csv", row.names=FALSE)
