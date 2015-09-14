#Kaggle Competition - Coupon Purchase

#Initialize libraries
require(Matrix)
library(caTools)

#Read input
detail_train    <<- read.csv('input/coupon_detail_train.csv', encoding="UTF-8") #details on bought or not
list_train      <<- read.csv('input/coupon_list_train.csv', encoding="UTF-8") #coupon details
list_test       <<- read.csv('input/coupon_list_test.csv', encoding="UTF-8") #coupon details
visit_train     <<- read.csv('input/coupon_visit_train.csv') #referrers and bought or not
user_list       <<- read.csv('input/user_list.csv', encoding="UTF-8") #user details

#Change locale for characters
Sys.setlocale(category="LC_ALL", locale = "japanese")

#figure out how to setup rmse to validate
#check validity using list_train data and comparing coupon

#Maybe use large_area_name for regional
#Use ken name for prefectural
#Create training set
train = merge(detail_train,list_train)
train = train[,c("COUPON_ID_hash","USER_ID_hash",
                 "GENRE_NAME","DISCOUNT_PRICE","PRICE_RATE",
                 "USABLE_DATE_MON","USABLE_DATE_TUE","USABLE_DATE_WED","USABLE_DATE_THU",
                 "USABLE_DATE_FRI","USABLE_DATE_SAT","USABLE_DATE_SUN","USABLE_DATE_HOLIDAY",
                 "USABLE_DATE_BEFORE_HOLIDAY","VALIDPERIOD", "DISPPERIOD", "ken_name", "small_area_name")]

#Combine test data with train data
list_test$USER_ID_hash = "dummyuser"
test_data = list_test[,c("COUPON_ID_hash","USER_ID_hash",
                         "GENRE_NAME","DISCOUNT_PRICE","PRICE_RATE",
                         "USABLE_DATE_MON","USABLE_DATE_TUE","USABLE_DATE_WED","USABLE_DATE_THU",
                         "USABLE_DATE_FRI","USABLE_DATE_SAT","USABLE_DATE_SUN","USABLE_DATE_HOLIDAY",
                         "USABLE_DATE_BEFORE_HOLIDAY","VALIDPERIOD","DISPPERIOD", "ken_name","small_area_name")]

train = rbind(train,test_data)

#NA imputation
train$VALIDPERIOD[is.na(train$VALIDPERIOD)] = 999
train[is.na(train)] = 1

#Setup mean price table
ken_price = aggregate(x=train$DISCOUNT_PRICE, by=list(train$ken_name, train$GENRE_NAME),FUN="mean")
names(ken_price) = c('ken_name','GENRE_NAME', 'AVG_PRICE')

#user_list[,c(2,3,6)]

#Setup feature to find difference from average of type in region
#Feature engineering
train = merge(train, ken_price)
train$ADJUSTED_PRICE = 1/log10(train$AVG_PRICE - train$DISCOUNT_PRICE)
train$AVG_PRICE = NULL
train$DISCOUNT_PRICE = 1/log10(train$DISCOUNT_PRICE) #Normalizes value via divide by log
train$PRICE_RATE = 1/log10(train$PRICE_RATE) #Normalizes value via divide by log
train$VALIDPERIOD = 1/log10(train$VALIDPERIOD) #Normalizes value via divide by log
train$DISPPERIOD = 1/log10(train$DISPPERIOD)

train = train[,c(1,3:17,2,18:19)]

#Convert the factors to columns of 0's and 1's
train = cbind(train[,c(1,2)],model.matrix(~ -1 + .,train[,-c(1,2)],
                                          contrasts.arg=lapply(train[,names(which(sapply(train[,-c(1,2)], is.factor)==TRUE))], contrasts, contrasts=FALSE)))

#Separate data back into test/train
test = train[train$USER_ID_hash=="dummyuser",]
test = test[,-2]
train = train[train$USER_ID_hash!="dummyuser",]

# Randomly split the data into training and testing sets
#split = sample.split(train$COUPON_ID_hash, SplitRatio = 0.7)

# Split up the data using subset
#train_sub = subset(train, split==TRUE)
#test_sub = subset(train, split==FALSE)

#Data frame containing averages of characteristics for collaborative filtering
uchar = aggregate( . ~ USER_ID_hash, data=train[,-1],FUN=mean)
uchar$DISCOUNT_PRICE = 1
uchar$PRICE_RATE = 1

#Weight Matrix
W = as.matrix(Diagonal(x=c(rep(2.05,13), #GENRE_NAME
                           rep(2,1),  #DISCOUNT_PRICE
                           rep(-0.13,1), #PRICE_RATE
                           rep(0,9), #USABLE_DATE
                           rep(0,1), #VALIDPERIOD
                           rep(0,1), #DISPPERIOD
                           rep(1.01,47), #ken_area
                           rep(4.85,55), #small_area_name
                           rep(0,1) #ADJUSTED_PRICE
                           ))) 

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

#Restore locale
Sys.setlocale(category="LC_ALL", locale = "English_United States.1252")