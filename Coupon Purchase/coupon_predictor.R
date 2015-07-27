#Kaggle Competition - Coupon Purchase

#Read input
#area_train      <<- read.csv('coupon_area_train.csv', stringsAsFactors = FALSE) #
#area_test       <<- read.csv('coupon_area_test.csv', stringsAsFactors = FALSE) #Says region coupon from but in list_train anyways
detail_train    <<- read.csv('coupon_detail_train.csv', stringsAsFactors = FALSE)
list_train      <<- read.csv('coupon_list_train.csv', stringsAsFactors = FALSE)
list_test       <<- read.csv('coupon_list_test.csv', stringsAsFactors = FALSE)
visit_train     <<- read.csv('coupon_visit_train.csv', stringsAsFactors = FALSE)
user_list       <<- read.csv('user_list.csv', stringsAsFactors = FALSE)

#cluster coupons/pref/location
#Lookup the people who quit maybe and what they did with coupons
#Merge detail train with list train for now for a complete coupon info
#Look up holidays
#Visit_train has info on people looking up coupons and potentially buying

#pretty much predict types of things user likes and then location/when they buy or use then preict from list set

#overall seems to be a markov model for ranking, likely lookup the hidden markov model to predict

#Training set from merging coupon details with list
train = merge(detail_train,list_train)
train$CAPSULE_TEXT = NULL
train$SMALL_AREA_NAME = NULL
train$PURCHASEID_hash = NULL
#Predict the valid fields

#Create submission
submission = data.frame(user_list$USER_ID_hash)
submission$PURCHASED_COUPONS = list_test$COUPON_ID_hash
submission <- submission[,c("USER_ID_hash","PURCHASED_COUPONS")]
write.csv(submission, file="submission.csv", row.names=FALSE)