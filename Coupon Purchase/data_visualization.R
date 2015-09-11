#Coupon Purchase Prediction Data Visualizations

#Loads libraries
library(ggplot2)
library(extrafont)

#Read input
area_train      <<- read.csv('input/coupon_area_train.csv') #coupon per area
detail_train    <<- read.csv('input/coupon_detail_train.csv') #details on bought or not
list_train      <<- read.csv('input/coupon_list_train.csv') #coupon details
visit_train     <<- read.csv('input/coupon_visit_train.csv') #referrers and bought or not
user_list       <<- read.csv('input/user_list.csv') #user details

#Install fonts
font_import()
fonts()
fonttable()

#Plot the number of coupons sold per prefecture
pref_coupons = as.data.frame(table(area_train$PREF_NAME))
names(pref_coupons) = c('pref','freq')
ggplot(data=area_train,aes(x=as.factor(PREF_NAME))) + geom_bar(stat='bin') + 
    labs(title="Coupons bought per prefecture" ,x="Prefecture", y="Coupons bought") + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1,family="MS Mincho"))

#Plot the number of coupons sold per prefecture(smaller)
small_pref_coupons = as.data.frame(table(area_train$SMALL_AREA_NAME))
names(small_pref_coupons) = c('pref','freq')
ggplot(data=area_train,aes(x=as.factor(SMALL_AREA_NAME))) + geom_bar(stat='bin') + 
    labs(title="Coupons bought per area" ,x="Area", y="Coupons bought") + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1,family="MS Mincho"))

#Plot number of male to female
pie <- ggplot(user_list, aes(x = factor(1), fill = factor(SEX_ID))) +
    geom_bar(width = 1) + labs(title="Ratio of male to female users", x="",y="") +
    scale_fill_discrete(name="Sex")
pie + coord_polar(theta = "y")