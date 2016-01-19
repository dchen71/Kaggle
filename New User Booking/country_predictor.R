#Kaggle Competition - New User Bookings

library(lubridate)
library(xgboost)

#Read input
dir = 'input/'
train = read.csv(paste0(dir,"train_users_2.csv"))
test = read.csv(paste0(dir,"test_users.csv"))
#countries = read.csv(paste0(dir,"countries.csv"))

#sessions  = read.csv(paste0(dir,"sessions.csv"))

#Preprocess data
##countries - use distance, destination lang, destination, may have issues with test set
###Consider adding in countries via columns with desitation, distance, language, levenshtein

#convert dates to date format first

#timestamp_firstactive seems be be setup yearmonthdayhourminutesecond
##should sort before checking this
##seems to be 20140101-20140331 for train
##seems to be 20140401-20140630 for test

#date_account_created and date_first_booking need to be converted into date
##age seems to have na min age is 14 and max is 2014
#table(train$age)

#Preprocesses the data for both train and test
#preprocess = function(df){
#    df = merge(df, sessions, by.x="id", by.y="user_id")
#    df$month_created = month(df$date_account_created)
#    df$year_created = year(df$date_account_created)
#    df$year_booked = year(df$date_first_booking)
#    df$month_booked = month(df$date_first_booking)
#    df$diff_booked = as.Date(df$date_first_booking) - as.Date(df$date_account_created)
#    return(df)
#}

preprocess = function(df){
    #Normalizes some of the ages
    df$age[df$age > 150 & !is.na(df$age)] = 2015 - df$age[df$age > 150 & !is.na(df$age)]
    df$age[df$age > 115 & !is.na(df$age)] = 115
    df$age[df$age < 18 & !is.na(df$age)] = 18
    
    df$dac_month = month(df$date_account_created)
    df$dac_year = year(df$date_account_created)
    df$dac_day = day(df$date_account_created)
    df$date_account_created = NULL
        
    return(df)
}


train = preprocess(train)
test = preprocess(test)

#Prediction
##Setting up the datamatrix for training
xgbMat = xgb.DMatrix((data.matrix(train[,!colnames(train) %in% c("country_destination", "id", "date_first_booking")])), 
                     label=as.numeric(train$country_destination) - 1, missing=NaN)

##Train using softmax multi classiication
xgb = xgb.train(data=xgbMat, max.depth = 6, eta = 0.3, nround = 100, objective = "multi:softmax", 
                num_class = 12, subsample=0.5, colsample_bytree=0.5)

# Plot important features for boost
names = colnames(train[, !colnames(train) %in% c("country_destination", "id")])
importance_matrix = xgb.importance(names, model = xgb)
xgb.plot.importance(importance_matrix[1:10,])

##Prediction dataset
xgb.submit = predict(xgb, newdata = data.matrix(test[, !colnames(test) %in% c("id", "date_first_booking")]), 
                     missing=NaN)
xgb.submit.text = levels(train$country_destination)[xgb.submit+1]

#Write to csv for XgBoost
submission = data.frame(test$id)
names(submission) = 'id'
submission$country = xgb.submit.text
write.csv(submission,file="xg.csv",row.names=FALSE)
