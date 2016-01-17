#Kaggle Competition - New User Bookings

library(lubridate)

#Read input
dir = 'input/'
train = read.csv(paste0(dir,"train_users.csv"))
test = read.csv(paste0(dir,"test_users_2.csv"))
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
    
        
    return(df)
}


train = preprocess(train)
test = preprocess(test)

#Prediction



#Creates submission
write.csv(submission, "prediction.csv", row.names=FALSE)
