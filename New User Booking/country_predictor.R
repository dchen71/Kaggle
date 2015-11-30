#Kaggle Competition - New User Bookings

library(lubridate)

#Read input
dir = 'input/'
train = read.csv(paste0(dir,"train_users.csv"))
test = read.csv(paste0(dir,"test_users.csv"))
countries = read.csv(paste0(dir,"countries.csv"))
age = read.csv(paste0(dir, "age_gender_bkts.csv"))
sessions  = read.csv(paste0(dir,"sessions.csv"))

#Preprocess data
##age - Merge age and train/test based on age bucket(not sure for na)
##countries - use distance, destination lang, destination, may have issues with test set
###Consider adding in countries via columns with desitation, distance, language, levenshtein

#Preprocesses the data for both train and test
preprocess = function(df){
    df = merge(df, sessions, by.x="id", by.y="user_id")
    df$month_created = month(df$date_account_created)
    df$year_created = year(df$date_account_created)
    df$year_booked = year(df$date_first_booking)
    df$month_booked = month(df$date_first_booking)
    df$diff_booked = as.Date(df$date_first_booking) - as.Date(df$date_account_created)
    return(df)
}

train = preprocess(train)
test = preprocess(test)

#Prediction



#Creates submission

