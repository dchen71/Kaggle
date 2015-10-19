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
    df$Date = as.Date(df$Date)
    df$Month = as.integer(format(df$Date, "%m"))
    df$Year = as.integer(format(df$Date, "%y"))
    df$Day = as.integer(format(df$Date, '%d'))
    df$Week = 0
    df$Week[df$Day >= 01 & df $Day <= 07] = 1
    df$Week[df$Day >= 08 & df $Day <= 14] = 2
    df$Week[df$Day >= 15 & df $Day <= 21] = 3
    df$Week[df$Day >= 22 & df $Day <= 28] = 4
    df$Week[df$Day >= 29 & df $Day <= 31] = 5
    df$Day = NULL
    df$Month = as.factor(df$Month)
    df$Year = as.factor(df$Year)
    df$Week = as.factor(df$Week)
    df$Date = NULL
    
    ##Feature engineering
    ##Switch DayOFWeek into weekday/weekend
    df$Weekday = 0
    df$Weekend = 0
    df$Weekday[df$DayOfWeek >= 1 & df$DayOfWeek <= 5] = 1
    df$Weekend[df$DayOfWeek >= 6 & df$DayOfWeek <= 7] = 1
    df$Weekday = as.factor(df$Weekday)
    df$Weekend = as.factor(df$Weekend)
    df$DayOfWeek = NULL
    
    return(df)
}

train = processData(train)

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

