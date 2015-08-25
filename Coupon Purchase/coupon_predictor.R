#Kaggle Competition - Coupon Purchase

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

#Maybe use large_area_name for regional
#Use ken name for prefectural
#Create training set
train = merge(detail_train,list_train)
train = train[,c("COUPON_ID_hash","USER_ID_hash",
                 "GENRE_NAME","DISCOUNT_PRICE","PRICE_RATE",
                 "USABLE_DATE_MON","USABLE_DATE_TUE","USABLE_DATE_WED","USABLE_DATE_THU",
                 "USABLE_DATE_FRI","USABLE_DATE_SAT","USABLE_DATE_SUN","USABLE_DATE_HOLIDAY",
                 "USABLE_DATE_BEFORE_HOLIDAY","VALIDPERIOD", "small_area_name")]

#Combine test data with train data
list_test$USER_ID_hash = "dummyuser"
test_data = list_test[,c("COUPON_ID_hash","USER_ID_hash",
                         "GENRE_NAME","DISCOUNT_PRICE","PRICE_RATE",
                         "USABLE_DATE_MON","USABLE_DATE_TUE","USABLE_DATE_WED","USABLE_DATE_THU",
                         "USABLE_DATE_FRI","USABLE_DATE_SAT","USABLE_DATE_SUN","USABLE_DATE_HOLIDAY",
                         "USABLE_DATE_BEFORE_HOLIDAY","VALIDPERIOD", "small_area_name")]

train = rbind(train,test_data)

#NA imputation
train$VALIDPERIOD[is.na(train$VALIDPERIOD)] = 999
train[is.na(train)] = 1

#Setup feature to find difference from average of type in region
#Feature engineering
train$DISCOUNT_PRICE = 1/log10(train$DISCOUNT_PRICE) #Normalizes value via divide by log
train$PRICE_RATE = 1/log10(train$PRICE_RATE) #Normalizes value via divide by log
train$VALIDPERIOD = 1/log10(train$VALIDPERIOD) #Normalizes value via divide by log

#Convert the factors to columns of 0's and 1's
train = cbind(train[,c(1,2)],model.matrix(~ -1 + .,train[,-c(1,2)],
                                          contrasts.arg=lapply(train[,names(which(sapply(train[,-c(1,2)], is.factor)==TRUE))], contrasts, contrasts=FALSE)))

#Separate data back into test/train
test = train[train$USER_ID_hash=="dummyuser",]
test = test[,-2]
train = train[train$USER_ID_hash!="dummyuser",]

#Data frame containing averages of characteristics
uchar = aggregate(. ~ USER_ID_hash, data=train[,-1],FUN=mean)
uchar$DISCOUNT_PRICE = 1
uchar$PRICE_RATE = 1

#Change weights to make it out of 100
#Weight Matrix: GENRE_NAME DISCOUNT_PRICE PRICE_RATE USABLE_DATE_ ken_name VALIDPERIOD small_area_name
require(Matrix)
W = as.matrix(Diagonal(x=c(rep(2,13), rep(1,1), rep(0,1), rep(0,9), rep(0,1), rep(4,55))))

#Calculation of cosine similairties of users and coupons for test purchases
score = as.matrix(uchar[,2:ncol(uchar)]) %*% W %*% t(as.matrix(test[,2:ncol(test)]))

#Order the list of coupons according to similarities and take only first 20 coupons
uchar$PURCHASED_COUPONS = do.call(rbind, lapply(1:nrow(uchar),FUN=function(i){
    purchased_cp <- paste(test$COUPON_ID_hash[order(score[i,], decreasing = TRUE)][1:20],collapse=" ")
    return(purchased_cp)
}))

#Creates submission
submission = merge(user_list, uchar, all.x=TRUE)
submission = submission[,c("USER_ID_hash","PURCHASED_COUPONS")]
write.csv(submission, file="recommended.csv", row.names=FALSE)