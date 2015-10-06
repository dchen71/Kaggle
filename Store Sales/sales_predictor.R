#Kaggle Competition - Rossmann Store Sales

#Read input
dir = 'input/'
store = read.csv(paste0(dir,"store.csv"))
train = read.csv(paste0(dir,"train.csv"))
test = read.csv(paste0(dir,"test.csv"))

#Setup cross validation set
library(caret)

#Seed for consistent results
set.seed(100)

part = createDataPartition(train$Sales, p = 0.6, list = FALSE)
trainset = train[part,]
testset = train[-part,]

#Setup Linear model for CV
lmTrain = lm(Sales ~ . - Date, trainset)
predTrain = predict(lmTrain, trainset)
predTest = predict(lmTrain, newdata=testset)

#Create a linear model
lmModel = lm(Sales ~ . - Date - Customers, train)
predModel = predict(lmModel, newdata = test)

#Creates submission
submission = data.frame(test$Id)
names(submission) = 'ID'
submission$Sales = predModel
write.csv(submission, file="sales.csv", row.names=FALSE)
