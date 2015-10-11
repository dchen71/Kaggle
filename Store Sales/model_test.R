#Kaggle Competition - Rossmann Store Sales

##Cross Validation of model

#Read input
dir = 'input/'
store = read.csv(paste0(dir,"store.csv"), na.strings = c("NA", ''))
train = read.csv(paste0(dir,"train.csv"))

#Merge datasets
train = merge(train, store)

#Data Cleanup
##Conversions to factors
processData = function(df){
    df$SchoolHoliday = as.factor(df$SchoolHoliday)
    df$DayOfWeek = as.factor(df$DayOfWeek)
    df$Open = as.factor(df$Open)
    df$Promo = as.factor(df$Promo)
    df$CompetitionDistance[is.na(df$CompetitionDistance)] = max(df$CompetitionDistance[!is.na(df$CompetitionDistance)]) * 1.5
    df$Promo2 = as.factor(df$Promo2)
    
    #Convert NA into factors
    df$PromoInterval = addNA(df$PromoInterval)
    df$CompetitionOpenSinceYear = addNA(df$CompetitionOpenSinceYear)
    df$CompetitionOpenSinceMonth = addNA(df$CompetitionOpenSinceMonth)
    df$Promo2SinceWeek = addNA(df$Promo2SinceWeek)
    df$Promo2SinceYear = addNA(df$Promo2SinceYear)
    
    #Parsing and conversion of dates to factors
    df$Date = as.Date(train$Date)
    df$Month = as.integer(format(df$Date, "%m"))
    df$Year = as.integer(format(df$Date, "%y"))
    
    return(df)
}

train = processData(train)

##Remove date from both
train = train[,-3]

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
lmTrain = lm(Sales ~ . - Customers, trainset)
predTest = predict(lmTrain, newdata=testset)

#Convert all predictors lower than 0 to 0
predTest[predTest < 0] = 0

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

#Add predicted number of customers for test
lmCust = lm(Customers ~ ., trainset[,-3])
predCust = predict(lmCust, newdata=testset)

#Convert all predictors lower than 0 to 0
predCust[predCust < 0] = 0

testCust = testset
testCust$Customers = predCust

##Setup Linear model for CV
lmSalesCust = lm(Sales ~ ., trainset)
predSalesCust = predict(lmSalesCust, newdata=testCust)

#Convert all predictors lower than 0 to 0
predSalesCust[predSalesCust < 0] = 0

checkSales(predSalesCust,testset)

#Add predicted number of customers for test
lmCust = lm(Customers ~ ., trainset[,-3])
predCust = predict(lmCust, newdata=testset)

#Convert all predictors lower than 0 to 0
predCust[predCust < 0] = 0

##Function to print out the number of Customers results within 10% of actual
checkCust = function(pred,df){
    results = data.frame(pred)
    results$Customers = df$Customers
    wrong = 0
    for(i in 1:nrow(results)){
        test = results[i,1]
        actual = results[i,2]
        if(!(test >= actual * 0.8 && test <= actual * 1.2))
            wrong = wrong + 1
    }
    print(paste0('The accuracy of values within 20% is ', (nrow(df) - wrong)/nrow(df), '%'))
}

checkCust(predCust, testset)