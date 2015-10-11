#Kaggle Competition - Rossmann Store Sales

#Read input
dir = 'input/'
store = read.csv(paste0(dir,"store.csv"), na.strings = c("NA", ''))
train = read.csv(paste0(dir,"train.csv"))
test = read.csv(paste0(dir,"test.csv"))

#Merge datasets
train = merge(train, store)
test = merge(test,store)

#Data Cleanup
##Fix NAs in Open in test
test$Open[which(is.na(test$Open))] = 0

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
test = processData(test)

##Remove date from both
train = train[,-3]
test = test[,-4]

#Add predicted number of customers for test
lmCust = lm(Customers ~ ., train[,-3])
predCust = predict(lmCust, newdata=test)
predCust[predCust < 0] = 0
test$Customers = predCust

##Feature engineering

#Prediction
##Create a linear model for Sales
lmModel = lm(Sales ~ ., train)
predModel = predict(lmModel, newdata = test)
predModel[predModel < 0] = 0


#Creates submission
submission = data.frame(test$Id)
names(submission) = 'ID'
submission$Sales = predModel
write.csv(submission, file="sales.csv", row.names=FALSE)
