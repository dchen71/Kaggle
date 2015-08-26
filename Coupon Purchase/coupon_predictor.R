#Kaggle Competition - Coupon Purchase

#Initialize libraries
require(Matrix)
library(caTools)
library(plyr)

#Read input
#area_train      <<- read.csv('input/coupon_area_train.csv') #coupon per area
#area_test       <<- read.csv('input/coupon_area_test.csv') #coupon per area
detail_train    <<- read.csv('input/coupon_detail_train.csv') #details on bought or not
list_train      <<- read.csv('input/coupon_list_train.csv') #coupon details
list_test       <<- read.csv('input/coupon_list_test.csv') #coupon details
visit_train     <<- read.csv('input/coupon_visit_train.csv') #referrers and bought or not
user_list       <<- read.csv('input/user_list.csv') #user details

#cluster coupons/pref/location/gender of user/age
#Look up holidays/type of for major ones at least
#prefecture and district data may be colinear
#maybe dispense date/dispense length could be useful
#figure out how to setup rmse to validate
#check validity using list_train data and comparing coupon
#Consider compressing useable into weekday/weekend/holiday

#Maybe use large_area_name for regional
#Use ken name for prefectural
#Create training set
train = merge(detail_train,list_train)
train = train[,c("COUPON_ID_hash","USER_ID_hash",
                 "GENRE_NAME","DISCOUNT_PRICE","PRICE_RATE",
                 "USABLE_DATE_MON","USABLE_DATE_TUE","USABLE_DATE_WED","USABLE_DATE_THU",
                 "USABLE_DATE_FRI","USABLE_DATE_SAT","USABLE_DATE_SUN","USABLE_DATE_HOLIDAY",
                 "USABLE_DATE_BEFORE_HOLIDAY","VALIDPERIOD", "small_area_name")]

## Setup number of visits
visits = merge(visit_train, user_list) 
coupon_visited = count(visits, c('USER_ID_hash', 'VIEW_COUPON_ID_hash'))
coupon_visited = merge(coupon_visited, user_list[c('USER_ID_hash')])
names(coupon_visited) = c('USER_ID_hash', 'COUPON_ID_hash', 'VISITS')
train = merge(train, coupon_visited, all.x=TRUE)
train = merge(train, user_list[c('USER_ID_hash', 'SEX_ID', 'AGE')])

#Combine test data with train data
list_test$USER_ID_hash = "dummyuser"
list_test$SEX_ID = "M"
list_test$AGE = 1
list_test$VISITS = 1
test_data = list_test[,c("COUPON_ID_hash","USER_ID_hash",
                         "GENRE_NAME","DISCOUNT_PRICE","PRICE_RATE",
                         "USABLE_DATE_MON","USABLE_DATE_TUE","USABLE_DATE_WED","USABLE_DATE_THU",
                         "USABLE_DATE_FRI","USABLE_DATE_SAT","USABLE_DATE_SUN","USABLE_DATE_HOLIDAY",
                         "USABLE_DATE_BEFORE_HOLIDAY","VALIDPERIOD", "small_area_name", 'VISITS', "SEX_ID", "AGE")]

train = rbind(train,test_data)


#NA imputation
train$VALIDPERIOD[is.na(train$VALIDPERIOD)] = 999
train$VISITS[is.na(train$VISITS)] = 0
train[is.na(train)] = 1

#Setup feature to find difference from average of type in region
#Feature engineering
#train$AVG_RATE = 1/log10(mean(price_rate ~ location/genre))
train$DISCOUNT_PRICE = 1/log10(train$DISCOUNT_PRICE) #Normalizes value via divide by log
train$PRICE_RATE = 1/log10(train$PRICE_RATE) #Normalizes value via divide by log
train$VALIDPERIOD = 1/log10(train$VALIDPERIOD) #Normalizes value via divide by log
train$AGE = 1/log10(train$AGE) #Normalizes value via divide by log
train$VISITS = 1/log10(train$VISITS) #Normalizes value via divide by log

train = train[,c(2,1,3:ncol(train))]

#Convert the factors to columns of 0's and 1's
train = cbind(train[,c(1,2)],model.matrix(~ -1 + .,train[,-c(1,2)],
                                          contrasts.arg=lapply(train[,names(which(sapply(train[,-c(1,2)], is.factor)==TRUE))], contrasts, contrasts=FALSE)))

#Separate data back into test/train
test = train[train$USER_ID_hash=="dummyuser",]
test = test[,c(-2,-84:-87)]
train = train[train$USER_ID_hash!="dummyuser",]
train = train[,-86]

train = train[,-84:-86]

# Randomly split the data into training and testing sets
#split = sample.split(train$COUPON_ID_hash, SplitRatio = 0.7)

# Split up the data using subset
#train_sub = subset(train, split==TRUE)
#test_sub = subset(train, split==FALSE)

#Data frame containing averages of characteristics for collaborative filtering
uchar = aggregate( . ~ USER_ID_hash, data=train[,-1],FUN=mean)
uchar$DISCOUNT_PRICE = 1
uchar$PRICE_RATE = 1

#Change weights varaible
#Figure out if possible to progmaticcaly determine weights
#Weight Matrix: Visits SEX AGE GENRE_NAME DISCOUNT_PRICE PRICE_RATE USABLE_DATE_ VALIDPERIOD small_area_name
weights = c(2,1,0,0,0,4,0,0,0)

W = as.matrix(Diagonal(x=c(rep(weights[1],13), rep(weights[2],1), rep(weights[3],1),
                           rep(weights[4],9), rep(weights[5],1), rep(weights[6],55),             
                           rep(weights[7],1))))

#Calculation of cosine similairties of users and coupons for test purchases
score = as.matrix(uchar[,2:ncol(uchar)]) %*% W %*% t(as.matrix(test[,2:ncol(test)]))

#Order the list of coupons according to similarities and take only first 15 coupons
uchar$PURCHASED_COUPONS = do.call(rbind, lapply(1:nrow(uchar),FUN=function(i){
    purchased_cp = paste(test$COUPON_ID_hash[order(score[i,], decreasing = TRUE)][1:15],collapse=" ")
    return(purchased_cp)
}))

#Creates submission
submission = merge(user_list, uchar, all.x=TRUE)
submission = submission[,c("USER_ID_hash","PURCHASED_COUPONS")]
write.csv(submission, file="recommended.csv", row.names=FALSE)