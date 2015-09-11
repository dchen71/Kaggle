#Coupon Purchase Prediction Data Visualizations

#Loads libraries
library(ggplot2)

#Read input
area_train      <<- read.csv('input/coupon_area_train.csv', encoding="UTF-8") #coupon per area
detail_train    <<- read.csv('input/coupon_detail_train.csv', encoding="UTF-8") #details on bought or not
list_train      <<- read.csv('input/coupon_list_train.csv', encoding="UTF-8") #coupon details
user_list       <<- read.csv('input/user_list.csv', encoding="UTF-8") #user details

#Change locale for characters
Sys.setlocale(category="LC_ALL", locale = "japanese")

#Plot the number of coupons sold per prefecture
pref_coupons = as.data.frame(table(area_train$PREF_NAME))
names(pref_coupons) = c('pref','freq')
ggplot(data=area_train,aes(x=PREF_NAME)) + geom_bar(stat='bin') + 
    labs(title="Coupons bought per prefecture" ,x="Prefecture", y="Coupons bought") + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Plot the number of coupons sold per prefecture(smaller)
small_pref_coupons = as.data.frame(table(area_train$SMALL_AREA_NAME))
names(small_pref_coupons) = c('pref','freq')
ggplot(data=area_train,aes(x=SMALL_AREA_NAME)) + geom_bar(stat='bin') + 
    labs(title="Coupons bought per area" ,x="Area", y="Coupons bought") + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Plot number of male to female using this service
mf = ggplot(user_list, aes(x = factor(1), fill = factor(SEX_ID))) +
    geom_bar(width = 1) + labs(title="Ratio of male to female users", x="",y="") +
    scale_fill_discrete(name="Sex")
mf + coord_polar(theta = "y")

#Plot number of male/prefecture using this service
males = subset(user_list, SEX_ID == 'm')
ggplot(data=males,aes(x=PREF_NAME)) + geom_bar(stat='bin') + 
    labs(title="Male users per prefecture" ,x="Prefecture", y="Coupons bought") + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Plot number of female/prefecture using this service
females = subset(user_list, SEX_ID == 'f')
ggplot(data=males,aes(x=PREF_NAME)) + geom_bar(stat='bin') + 
    labs(title="Female users per prefecture" ,x="Prefecture", y="Coupons bought") + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Plots age groups using this service
ages = data.frame(matrix(nrow=6,ncol=2))
names(ages) = c('Group','Count')
ages$Group = c('under18','a18to25','a25to35','a35to50','a50to65','a65to80')
ages[1,2] = data.frame(table(user_list$AGE < 18))[2,2]
ages[2,2] = data.frame(table(user_list$AGE >= 18,user_list$AGE <= 25))[4,3]
ages[3,2] = data.frame(table(user_list$AGE > 25,user_list$AGE <= 35))[4,3]
ages[4,2] = data.frame(table(user_list$AGE > 35,user_list$AGE <= 50))[4,3]
ages[5,2] = data.frame(table(user_list$AGE > 50,user_list$AGE <= 65))[4,3]
ages[6,2] = data.frame(table(user_list$AGE > 65,user_list$AGE <= 80))[2,3]
age = ggplot(ages, aes(x = factor(1), y=Count,fill = factor(Group))) +
    geom_bar(width = 1,stat="identity") + labs(title="Age distribution of users", x="",y="") +
    scale_fill_discrete(name="Age groups")
age + coord_polar(theta = "y")

#Plot age groups of males using this service
male_ages = data.frame(matrix(nrow=6,ncol=2))
names(male_ages) = c('Group','Count')
male_ages$Group = c('under18','a18to25','a25to35','a35to50','a50to65','a65to80')
male_ages[1,2] = data.frame(table(males$AGE < 18))[2,2]
male_ages[2,2] = data.frame(table(males$AGE >= 18,males$AGE <= 25))[4,3]
male_ages[3,2] = data.frame(table(males$AGE > 25,males$AGE <= 35))[4,3]
male_ages[4,2] = data.frame(table(males$AGE > 35,males$AGE <= 50))[4,3]
male_ages[5,2] = data.frame(table(males$AGE > 50,males$AGE <= 65))[4,3]
male_ages[6,2] = data.frame(table(males$AGE > 65,males$AGE <= 80))[2,3]
male_age = ggplot(male_ages, aes(x = factor(1), y=Count,fill = factor(Group))) +
    geom_bar(width = 1,stat="identity") + labs(title="Age distribution of males", x="",y="") +
    scale_fill_discrete(name="Age groups")
male_age + coord_polar(theta = "y")

#Plot age groups of females using this service
female_ages = data.frame(matrix(nrow=6,ncol=2))
names(female_ages) = c('Group','Count')
female_ages$Group = c('under18','a18to25','a25to35','a35to50','a50to65','a65to80')
female_ages[1,2] = data.frame(table(females$AGE < 18))[2,2]
female_ages[2,2] = data.frame(table(females$AGE >= 18,females$AGE <= 25))[4,3]
female_ages[3,2] = data.frame(table(females$AGE > 25,females$AGE <= 35))[4,3]
female_ages[4,2] = data.frame(table(females$AGE > 35,females$AGE <= 50))[4,3]
female_ages[5,2] = data.frame(table(females$AGE > 50,females$AGE <= 65))[4,3]
female_ages[6,2] = data.frame(table(females$AGE > 65,females$AGE <= 80))[2,3]
female_age = ggplot(female_ages, aes(x = factor(1), y=Count,fill = factor(Group))) +
    geom_bar(width = 1,stat="identity") + labs(title="Age distribution of females", x="",y="") +
    scale_fill_discrete(name="Age groups")
female_age + coord_polar(theta = "y")


#Restore locale
Sys.setlocale(category="LC_ALL", locale = "English_United States.1252")