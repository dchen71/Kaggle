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
    
    #Part out the date the account was created
    df$dac_month = as.factor(month(df$date_account_created))
    df$dac_year = as.factor(year(df$date_account_created))
    df$dac_day = as.factor(day(df$date_account_created))
    df$date_account_created = NULL
    
    #Part out the timestamp of account's first activity
    df$tfa_year = substr(df$timestamp_first_active,0,4)
    df$tfa_month = substr(df$timestamp_first_active,5,6)
    df$tfa_day = substr(df$timestamp_first_active,7,8)
    df$tfa_hour = substr(df$timestamp_first_active,9,10)
    df$tfa_min = substr(df$timestamp_first_active,11,12)
    df$timestamp_first_active = NULL
    
    #Remove variable because test is all null
    df$date_first_booking = NULL
    
    return(df)
}


train = preprocess(train)
test = preprocess(test)

#Prediction
##Setting up the datamatrix for training
xgbMat = xgb.DMatrix((data.matrix(train[,!colnames(train) %in% c("country_destination", "id")])), 
                     label=as.numeric(train$country_destination) - 1, missing=NaN)

##Train using softmax multi classiication
xgb = xgb.train(data=xgbMat, max.depth = 10, eta = 0.1, nround = 25, objective = "multi:softmax", 
                num_class = 12, subsample=0.5, colsample_bytree=0.5)

##Prediction dataset
xgb.submit = predict(xgb, newdata = data.matrix(test[, !colnames(test) %in% c("id")]), 
                     missing=NaN)
xgb.submit.text = levels(train$country_destination)[xgb.submit+1]

#Write to csv for XgBoost
submission = data.frame(test$id)
names(submission) = 'id'
submission$country = xgb.submit.text
write.csv(submission,file="xg.csv",row.names=FALSE)

# Plot important features for boost
names = colnames(train[, !colnames(train) %in% c("country_destination", "id")])
importance_matrix = xgb.importance(names, model = xgb)
xgb.plot.importance(importance_matrix[1:10,])
