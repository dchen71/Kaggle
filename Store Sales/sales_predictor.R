#Kaggle Competition - Rossmann Store Sales

#Read input
dir = 'input/'
store = read.csv(paste0(dir,"store.csv"))
train = read.csv(paste0(dir,"train.csv"))
test = read.csv(paste0(dir,"test.csv"))

#Data Cleanup
##Fix NAs in Open in test
test$Open[which(is.na(test$Open))] = 0

##Conversions to factors
factorizeData = function(df){
    df$SchoolHoliday = as.factor(df$SchoolHoliday)
    df$DayOfWeek = as.factor(df$DayOfWeek)
    df$Open = as.factor(df$Open)
    df$Promo = as.factor(df$Promo)
    return(df)
}

train = factorizeData(train)
test = factorizeData(test)

##Remove date from both
train = train[,-3]
test = test[,-4]

#Add predicted number of customers for test
lmCust = lm(Customers ~ ., train[,-3])
predCust = predict(lmCust, newdata=test)
test$Customers = predCust

##Feature engineering

#Testing Model for sales
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

##Function to print out the number of Sales results within 10% of actual
checkSales = function(pred,df){
    results = data.frame(pred)
    results$Sales = df$Sales
    wrong = 0
    for(i in 1:nrow(results)){
        test = results[i,1]
        actual = results[i,2]
        if(!(test >= actual *0.9 && test <= actual * 1.1))
            wrong = wrong + 1
    }
    print(paste0('The accuracy of values within 10% is ', (nrow(df) - wrong)/nrow(df), '%'))
}

checkSales(predTest, testset)

#Prediction
##Create a linear model
lmModel = lm(Sales ~ ., train)
predModel = predict(lmModel, newdata = test)

#Creates submission
submission = data.frame(test$Id)
names(submission) = 'ID'
submission$Sales = predModel
write.csv(submission, file="sales.csv", row.names=FALSE)
