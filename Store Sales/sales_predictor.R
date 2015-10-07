#Kaggle Competition - Rossmann Store Sales

#Read input
dir = 'input/'
store = read.csv(paste0(dir,"store.csv"))
train = read.csv(paste0(dir,"train.csv"))
test = read.csv(paste0(dir,"test.csv"))

#Data Cleanup
##Fix NAs in Open in test
test$Open[which(is.na(test$Open))] = 0

months = c('Jan', 'Feb', 'Mar', "Apr", "May", "Jun", 'Jul', "Aug", 'Sep', 'Oct', 'Nov', 'Dec')

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
