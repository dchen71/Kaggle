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

##Conversions to factors
factorizeData = function(df){
    df$SchoolHoliday = as.factor(df$SchoolHoliday)
    df$Store = as.factor(df$Store)
    df$DayOfWeek = as.factor(df$DayOfWeek)
    df$Open = as.factor(df$Open)
    df$Promo = as.factor(df$Promo)
    return(df)
}

train = factorizeData(train)
test = factorizeData(test)

##Feature engineering

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
