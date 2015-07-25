#Kaggle Competition - Coupon Purchase

area_train      <<- read.csv('coupon_area_train.csv', stringsAsFactors = FALSE)
area_test       <<- read.csv('coupon_area_test.csv', stringsAsFactors = FALSE)
detail_train    <<- read.csv('coupon_detail_train.csv', stringsAsFactors = FALSE)
list_train      <<- read.csv('coupon_list_train.csv', stringsAsFactors = FALSE)
list_test       <<- read.csv('coupon_list_test.csv', stringsAsFactors = FALSE)
visit_train     <<- read.csv('coupon_visit_train.csv', stringsAsFactors = FALSE)
user_list       <<- read.csv('user_list.csv', stringsAsFactors = FALSE)