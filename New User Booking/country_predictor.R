#Kaggle Competition - New User Bookings

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
##merge sessions with train/test and id/user_id

#Prediction



#Creates submission

