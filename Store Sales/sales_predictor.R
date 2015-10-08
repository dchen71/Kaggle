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

months = c('Jan', 'Feb', 'Mar', "Apr", "May", "Jun", 'Jul', "Aug", 'Sep', 'Oct', 'Nov', 'Dec')

##Conversions to factors and removal of certain variables
processData = function(df){
    df$SchoolHoliday = as.factor(df$SchoolHoliday)
    df$DayOfWeek = as.factor(df$DayOfWeek)
    df$Open = as.factor(df$Open)
    df$Promo = as.factor(df$Promo)
    df$CompetitionDistance[is.na(df$CompetitionDistance)] = max(df$CompetitionDistance[!is.na(df$CompetitionDistance)]) * 1.5
    df$CompetitionOpenSinceYear = NULL
    df$CompetitionOpenSinceMonth = NULL
    df$Promo2 = as.factor(df$Promo2)
    df$Promo2SinceWeek = NULL
    df$Promo2SinceYear = NULL
    
    #Setup Promo Interval/month
    for(i in months){
        month = paste0('PromoInt', i)
        add = data.frame(change=rep(0,nrow(df)))
        names(add) = month
        add[grep(i, df$PromoInterval),] = 1
        add[,1] = as.factor(add[,1])
        df = cbind(df, add)
    }
    df$PromoInterval = NULL
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
