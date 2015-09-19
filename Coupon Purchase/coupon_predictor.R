#Kaggle Competition - Coupon Purchase

#Initialize libraries
require(Matrix)

#Read input
dir = 'input/'
detail_train = read.csv(paste0(dir,"coupon_detail_train.csv"), encoding="UTF-8")
list_train = read.csv(paste0(dir,"coupon_list_train.csv"),encoding="UTF-8")
list_test = read.csv(paste0(dir,"coupon_list_test.csv"),encoding="UTF-8")
user_list = read.csv(paste0(dir,"user_list.csv"),encoding="UTF-8")
visit_train = read.csv(paste0(dir,"coupon_visit_train.csv"),nrows=-1)
visit_train = visit_train[visit_train$PURCHASE_FLG!=1,c("VIEW_COUPON_ID_hash","USER_ID_hash")]

#Change locale for characters
Sys.setlocale(category="LC_ALL", locale = "japanese")

# Feature engineering, put into factor of 0s for NAs and 1s actual values
list_train$VALIDPERIOD[is.na(list_train$VALIDPERIOD)] = -1
list_train$VALIDPERIOD = list_train$VALIDPERIOD +1
list_train$VALIDPERIOD[list_train$VALIDPERIOD>0] = 1
list_train$VALIDPERIOD = as.factor(list_train$VALIDPERIOD)
list_test$VALIDPERIOD[is.na(list_test$VALIDPERIOD)] = -1
list_test$VALIDPERIOD = list_test$VALIDPERIOD+1
list_test$VALIDPERIOD[list_test$VALIDPERIOD>0] = 1
list_test$VALIDPERIOD = as.factor(list_test$VALIDPERIOD)

# Sets up sum of coupon USABLE_DATEs for training and test dataset
for (i in 12:20) {
    list_train[is.na(list_train[,i]),i] = 0;    list_train[list_train[,i]>1,i] = 1
    list_test[is.na(list_test[,i]),i] = 0;    list_test[list_test[,i]>1,i] = 1
}
list_train$USABLE_DATE_sum = rowSums(list_train[,12:20])
list_test$USABLE_DATE_sum = rowSums(list_test[,12:20])

# Create train set by merging coupon_detail_train and coupon_list_train
# to get USER_ID_hash by coupon
train = merge(detail_train,list_train)
train = train[,c("COUPON_ID_hash","USER_ID_hash",
                  "GENRE_NAME","DISCOUNT_PRICE","DISPPERIOD",
                  "large_area_name","small_area_name","VALIDPERIOD","USABLE_DATE_sum")]
# Append test set to the training set for model.matrix factor column conversion
list_test$USER_ID_hash = "dummyuser"
cpchar = list_test[,c("COUPON_ID_hash","USER_ID_hash",
                   "GENRE_NAME","DISCOUNT_PRICE","DISPPERIOD",
                   "large_area_name","small_area_name","VALIDPERIOD","USABLE_DATE_sum")]
train = rbind(train,cpchar)

# NA imputation to values of 1
train[is.na(train)] = 1
# Feature engineering
train$DISCOUNT_PRICE = 1/log10(train$DISCOUNT_PRICE)    
train$DISPPERIOD[train$DISPPERIOD>7] = 7;train$DISPPERIOD = train$DISPPERIOD/7
train$USABLE_DATE_sum = train$USABLE_DATE_sum/9

# Convert the factors to columns of 0's and 1's
train = cbind(train[,c(1,2)],model.matrix(~ -1 + .,train[,-c(1,2)],
                                           contrasts.arg=lapply(train[,names(which(sapply(train[,-c(1,2)], is.factor)==TRUE))], 
                                                                contrasts, contrasts=FALSE)))

# Separate the test from train following factor column conversion
test = train[train$USER_ID_hash=="dummyuser",]
test = test[,-2]
train = train[train$USER_ID_hash!="dummyuser",]

#Numeric attributes cosine multiplication factors set to 1
train$DISCOUNT_PRICE = 1
train$DISPPERIOD = 1
train$USABLE_DATE_sum = 1

# Create starting uchar for all users initialized to zero
uchar = data.frame(USER_ID_hash=user_list[,"USER_ID_hash"])
uchar = cbind(uchar,matrix(0, nrow=dim(uchar)[1], ncol=(dim(train)[2] -2)))
names(uchar) = names(train)[2:dim(train)[2]]

# Incorporate the purchase training data from train, use sum function    
uchar = aggregate(.~USER_ID_hash, data=rbind(uchar,train[,-1]),FUN=sum)

# Add visit training data in chunks due to large dataset
imax = dim(visit_train)[1]   
i2 = 1
while (i2 < imax) {  # this loop takes a few minutes      
    i1 = i2
    i2 = i1 + 100000
    if (i2 > imax) i2 = imax
    cat("Merging coupon visit data i1=",i1," i2=",i2,"\n")
    trainv = merge(visit_train[i1:i2,],list_train, by.x="VIEW_COUPON_ID_hash", by.y="COUPON_ID_hash")
    trainv = trainv[,c("VIEW_COUPON_ID_hash","USER_ID_hash",
                        "GENRE_NAME","DISCOUNT_PRICE","DISPPERIOD",                          
                        "large_area_name","small_area_name","VALIDPERIOD","USABLE_DATE_sum")]
    # Same treatment as with coupon_detail train data
    trainv$DISCOUNT_PRICE = 1;train$DISPPERIOD = 1;train$USABLE_DATE_sum = 1;trainv[is.na(trainv)] = 1
    trainv = cbind(trainv[,c(1,2)],model.matrix(~ -1 + .,trainv[,-c(1,2)],
                                                 contrasts.arg=lapply(trainv[,names(which(sapply(trainv[,-c(1,2)], is.factor)==TRUE))], 
                                                                      contrasts, contrasts=FALSE)))
    # Discount coupon visits relative to coupon purchases
    couponVisitFactor = .005      
    trainv[,3:dim(trainv)[2]] = trainv[,3:dim(trainv)[2]] * couponVisitFactor  
    uchar = aggregate(.~USER_ID_hash, data=rbind(uchar,trainv[,-1]),FUN=sum)    
}

# Weight matrix with 7 factors, separate for male and female users 
## Male weight matrix
Wm = as.matrix(Diagonal(x=c(rep(0.5,1), 		#GENRE_NAME - massage/aesthetics
                            rep(2.00,1), 		#GENRE_NAME - gift card
                            rep(2.00,1), 		#GENRE_NAME - grooming
                            rep(2.00,1), 		#GENRE_NAME - others
                            rep(2.00,1), 		#GENRE_NAME - nail art
                            rep(2.00,1), 		#GENRE_NAME - beauty
                            rep(2.00,1), 		#GENRE_NAME - hair salon
                            rep(2.00,1), 		#GENRE_NAME - hotel/inn
                            rep(2.00,1), 		#GENRE_NAME - relaxation
                            rep(2.00,1), 		#GENRE_NAME - leisure
                            rep(2.00,1), 		#GENRE_NAME - lesson
                            rep(2.00,1), 		#GENRE_NAME - health
                            rep(2.00,1), 		#GENRE_NAME - shipping
                            rep(1.25,1), 		#DISCOUNT_PRICE
						    rep(1.25,1), 		#DISPPERIOD
						    rep(1.00,9), 		#large_area_name
                            rep(4.50,55),		#small_area_name
                            rep(0.625,2),		#VALIDPERIOD
                            rep(0.35,1))))		#USABLE_DATE_sum
## Female weight matrix
Wf = as.matrix(Diagonal(x=c(rep(0.5,1), 		#GENRE_NAME - massage/aesthetics
                            rep(1.75,1), 		#GENRE_NAME - gift card
                            rep(1.75,1), 		#GENRE_NAME - grooming
                            rep(1.75,1), 		#GENRE_NAME - others
                            rep(1.75,1), 		#GENRE_NAME - nail art
                            rep(1.75,1), 		#GENRE_NAME - beauty
                            rep(1.75,1), 		#GENRE_NAME - hair salon
                            rep(1.75,1), 		#GENRE_NAME - hotel/inn
                            rep(1.75,1), 		#GENRE_NAME - relaxation
                            rep(1.75,1), 		#GENRE_NAME - leisure
                            rep(1.75,1), 		#GENRE_NAME - lesson
                            rep(1.75,1), 		#GENRE_NAME - health
                            rep(1.75,1), 		#GENRE_NAME - shipping
							rep(0.75,1), 		#DISCOUNT_PRICE
							rep(1.50,1), 		#DISPPERIOD
							rep(1.00,9), 		#large_area_name
                            rep(4.50,55),		#small_area_name
                            rep(0.625,2),		#VALIDPERIOD
                            rep(0.25,1))))		#USABLE_DATE_sum

# Calculation of cosine similarities of users and coupons
score = as.matrix(uchar[,2:ncol(uchar)]) %*% Wm %*% t(as.matrix(test[,2:ncol(test)]))
score[user_list$SEX_ID=='f',] = as.matrix(uchar[user_list$SEX_ID=='f',2:ncol(uchar)]) %*% Wf %*% t(as.matrix(test[,2:ncol(test)]))
# Order the list of coupons according to similairties and take only first 10 coupons
uchar$PURCHASED_COUPONS = do.call(rbind, lapply(1:nrow(uchar),FUN=function(i){
    purchased_cp = paste(test$COUPON_ID_hash[order(score[i,], decreasing = TRUE)][1:10],collapse=" ")
    return(purchased_cp)
}))
# Creates submission
submission = uchar[,c("USER_ID_hash","PURCHASED_COUPONS")]
submission$PURCHASED_COUPONS[rowSums(score)==0] = ""
write.csv(submission, file="recommended.csv", row.names=FALSE)

#Restore locale
Sys.setlocale(category="LC_ALL", locale = "English_United States.1252")

