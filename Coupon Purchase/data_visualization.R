#Coupon Purchase Prediction Data Visualizations

#Loads libraries
library(ggplot2)
library(dplyr)
library(grid)
library(gridExtra)

#Read input
area_train      <<- read.csv('input/coupon_area_train.csv', encoding="UTF-8") #coupon per area
detail_train    <<- read.csv('input/coupon_detail_train.csv', encoding="UTF-8") #details on bought or not
list_train      <<- read.csv('input/coupon_list_train.csv', encoding="UTF-8") #coupon details
user_list       <<- read.csv('input/user_list.csv', encoding="UTF-8") #user details

#Initialize variables
purchases = merge(list_train[,c(2:5,21:24)], detail_train[,c(5,6)])

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

#Finds the total number of coupons bought per person and do the mean of all of them
coupon_count = aggregate(ITEM_COUNT ~ USER_ID_hash, data=detail_train, FUN="sum")
mean(coupon_count$ITEM_COUNT)

#Male coupon data
male_coupons = merge(males, coupon_count)
mean(male_coupons$ITEM_COUNT)
male_coupon_total = data.frame(matrix(nrow=6,ncol=2))
names(male_coupon_total) = c('Group','Count')
male_coupon_total$Group = c('under18','a18to25','a25to35','a35to50','a50to65','a65to80')
male_coupon_total[1,2] = sum(male_coupons$ITEM_COUNT[male_coupons$AGE < 18])
male_coupon_total[2,2] = sum(male_coupons$ITEM_COUNT[male_coupons$AGE[male_coupons$AGE <= 25] >= 18])
male_coupon_total[3,2] = sum(male_coupons$ITEM_COUNT[male_coupons$AGE[male_coupons$AGE <= 35] >= 25])
male_coupon_total[4,2] = sum(male_coupons$ITEM_COUNT[male_coupons$AGE[male_coupons$AGE <= 50] > 35])
male_coupon_total[5,2] = sum(male_coupons$ITEM_COUNT[male_coupons$AGE[male_coupons$AGE <= 65] > 50])
male_coupon_total[6,2] = sum(male_coupons$ITEM_COUNT[male_coupons$AGE[male_coupons$AGE <= 80] > 65])

ggplot(data=male_coupon_total,aes(x=Group,y=Count,fill=Group)) + geom_bar(stat='identity') + 
    labs(title="Total number of coupons bought by men" ,x="Age Groups", y="Coupons bought") + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Female coupon data
female_coupons = merge(females, coupon_count)
mean(female_coupons$ITEM_COUNT)
female_coupon_total = data.frame(matrix(nrow=6,ncol=2))
names(female_coupon_total) = c('Group','Count')
female_coupon_total$Group = c('under18','a18to25','a25to35','a35to50','a50to65','a65to80')
female_coupon_total[1,2] = sum(female_coupons$ITEM_COUNT[female_coupons$AGE < 18])
female_coupon_total[2,2] = sum(female_coupons$ITEM_COUNT[female_coupons$AGE[female_coupons$AGE <= 25] >= 18])
female_coupon_total[3,2] = sum(female_coupons$ITEM_COUNT[female_coupons$AGE[female_coupons$AGE <= 35] >= 25])
female_coupon_total[4,2] = sum(female_coupons$ITEM_COUNT[female_coupons$AGE[female_coupons$AGE <= 50] > 35])
female_coupon_total[5,2] = sum(female_coupons$ITEM_COUNT[female_coupons$AGE[female_coupons$AGE <= 65] > 50])
female_coupon_total[6,2] = sum(female_coupons$ITEM_COUNT[female_coupons$AGE[female_coupons$AGE <= 80] > 65])

ggplot(data=female_coupon_total,aes(x=Group,y=Count,fill=Group)) + geom_bar(stat='identity') + 
    labs(title="Total number of coupons bought by women" ,x="Age Groups", y="Coupons bought") + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Tallies the number of coupons per genre
ggplot(data=list_train,aes(x=GENRE_NAME,fill=GENRE_NAME)) + geom_bar(stat='bin') + 
       labs(title="Number of Coupons per Genre" ,x="Genres", y="Number of coupons") + 
       theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Tallies number of coupons per prefecture
ggplot(data=list_train,aes(x=large_area_name,,fill=large_area_name)) + geom_bar(stat='bin') + 
    labs(title="Number of Coupons per Prefecture" ,x="Prefecture", y="Number of coupons") + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Contains distribution of coupon genre per area
area_distribution = aggregate(COUPON_ID_hash ~ large_area_name + GENRE_NAME, data=list_train, FUN="length")
names(area_distribution) = c('Pref','Genre', 'Coupons')
area_distribution = area_distribution[order(area_distribution$Pref, area_distribution$Coupons),]

#Function to plot bar graphs comparing coupon distribution against two prefectures
plot_area = function(df, prefs) {
    filtered = filter(df, Pref %in% prefs)
    plot = ggplot(data=filtered,aes(x=Genre,y=Coupons,fill=Pref)) + 
        geom_bar(stat='identity', position=position_dodge()) + 
        labs(title="Comparsion of coupon numbers between areas",x="Genre", y="Number of Coupons") + 
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    return(plot)
}

#Plot the comparasions of coupon distribution bought between prefectures
plot_area(area_distribution, c('??????','??????'))

#Contains distribution of coupon genre per ken
ken_distribution = aggregate(COUPON_ID_hash ~ ken_name + GENRE_NAME, data=list_train, FUN="length")
names(ken_distribution) = c('Pref','Genre', 'Coupons')
ken_distribution = ken_distribution[order(ken_distribution$Pref, ken_distribution$Coupons),]

#Function to plot bar graphs comparing coupon distribution against two prefectures
plot_ken = function(df, prefs) {
    filtered = filter(df, Pref %in% prefs)
    plot = ggplot(data=filtered,aes(x=Genre,y=Coupons,fill=Pref)) + 
        geom_bar(stat='identity', position=position_dodge()) + 
        labs(title="Comparsion of coupon numbers between prefectures",x="Genre", y="Number of Coupons") + 
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    return(plot)
}

#Plot the comparasions of coupon distribution bought between prefectures
plot_ken(ken_distribution, c('?????????','?????????'))

#Contains the mean percentage discount distribution of ken/genre
ken_percent_price = aggregate(x=list_train$PRICE_RATE, by=list(list_train$ken_name, list_train$GENRE_NAME),FUN="mean")
names(ken_percent_price) = c('Pref','Genre', 'Mean_Discount')
ken_percent_price = ken_percent_price[order(ken_percent_price$Pref, ken_percent_price$Mean_Discount),]

#Function to plot bar graphs comparing discount percentage against two prefectures
plot_discount = function(df, prefs) {
    filtered = filter(df, Pref %in% prefs)
    plot = ggplot(data=filtered,aes(x=Genre,y=Mean_Discount,fill=Pref)) + 
        geom_bar(stat='identity', position=position_dodge()) + 
        labs(title="Comparsion of discount % between prefectures and categories",x="Categories", y="Avg price") + 
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    return(plot)
}

#Plot the comparasions of discount percentage bought between prefectures
plot_discount(ken_percent_price, c('?????????','?????????'))

#Contains the mean price distribution of ken/genre
ken_price = aggregate(x=list_train$DISCOUNT_PRICE, by=list(list_train$ken_name, list_train$GENRE_NAME),FUN="mean")
names(ken_price) = c('Pref','Genre', 'Mean_Price')
ken_price = ken_price[order(ken_price$Pref, ken_price$Mean_Price),]

#Function to plot bar graphs comparing mean discounted price against two prefectures
plot_mean = function(df, prefs) {
    filtered = filter(df, Pref %in% prefs)
    plot = ggplot(data=filtered,aes(x=Genre,y=Mean_Price,fill=Pref)) + 
                geom_bar(stat='identity', position=position_dodge()) + 
                labs(title="Comparsion of mean price between prefectures and categories",x="Categories", y="Avg price") + 
                theme(axis.text.x = element_text(angle = 45, hjust = 1))
    return(plot)
}

#Plot the comparasions of mean coupon price bought between prefectures
plot_mean(ken_price, c('?????????','?????????'))

#Find coupons bought by male
male_coupon_list = merge(males[,c(2,3,6)], purchases)
#Find coupons bought by female
female_coupon_list = merge(females[,c(2,3,6)], purchases)

#Shows the total number of coupons bought by gender
#Male
male_total = ggplot(data=male_coupon_list,aes(x=GENRE_NAME,fill=GENRE_NAME)) + geom_bar(stat='bin') + 
    labs(title="Number of Coupons coupons bought by men" ,x="Genre", y="Number of coupons") + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
#Female
female_total = ggplot(data=female_coupon_list,aes(x=GENRE_NAME,fill=GENRE_NAME)) + geom_bar(stat='bin') + 
    labs(title="Number of Coupons coupons bought by women" ,x="Genre", y="Number of coupons") + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

grid.arrange(male_total, female_total, main="Male vs Female Purchasing Habits")

#Creates new row which contains age group
#1 = under18
#2 = 18-25
#3 = 26-35
#4 = 36-50
#5 = 51-65
#6 = 66-80
process_coupon_list = function(df){
    df$Group = 0
    #Logic to input 1 if matches age group
    for(i in 1:nrow(df)){
        if(df[i,3] < 18){
            df[i,12] = 1
        }
        else if(df[i,3] >= 18){
            if(df[i,3] <= 25){
                df[i,12] = 2
            }
            else if (df[i,3] > 25){
                if(df[i,3] <= 35){
                    df[i,12] = 3
                }
                else if(df[i,3] > 35){
                    if(df[i,3] <= 50){
                        df[i,12] = 4
                    }
                    else if(df[i,3] > 50){
                        if(df[i,3] <= 65){
                            df[i,12] = 5
                        }
                        else if (df[i,3] > 65){
                            df[i,12] = 6
                        }
                    }
                }
            }
        }
    }
    return(df)
}

#Process data by putting in age groups
male_coupon_list = process_coupon_list(male_coupon_list)
female_coupon_list = process_coupon_list(female_coupon_list)

#Function to return plot showing genres per age group
plot_age_genre = function(df,age){
    filtered = subset(df, df$Group == age)
    plot = ggplot(data=filtered,aes(x=GENRE_NAME,fill=GENRE_NAME)) + 
        geom_bar(stat='bin') + 
        labs(x="Categories", y="# Coupons Bought") + 
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    return(plot)
}

#Find male age group bought
m1 = plot_age_genre(male_coupon_list,1)
m2 = plot_age_genre(male_coupon_list,2)
m3 = plot_age_genre(male_coupon_list,3)
m4 = plot_age_genre(male_coupon_list,4)
m5 = plot_age_genre(male_coupon_list,5)
m6 = plot_age_genre(male_coupon_list,6)

#Find female age group bought
f1 = plot_age_genre(female_coupon_list,1)
f2 = plot_age_genre(female_coupon_list,2)
f3 = plot_age_genre(female_coupon_list,3)
f4 = plot_age_genre(female_coupon_list,4)
f5 = plot_age_genre(female_coupon_list,5)
f6 = plot_age_genre(female_coupon_list,6)

#Multi chart comparsions
grid.arrange(f1,m1, main="Female <18 vs Male <18 Purchase Habits")
grid.arrange(f2,m2, main="Female 18-25 vs Male 18-25 Purchase Habits")
grid.arrange(f3,m3, main="Female 26-35 vs Male 26-35 Purchase Habits")
grid.arrange(f4,m4, main="Female 36-50 vs Male 36-50 Purchase Habits")
grid.arrange(f5,m5, main="Female 51-65 vs Male 51-65 Purchase Habits")
grid.arrange(f6,m6, main="Female 66-80 vs Male 66-80 Purchase Habits")
grid.arrange(m2,m3, main="Male 18-25 vs 26-35 Purchase Habits")
grid.arrange(m3,m4, main="Male 26-35 vs 36-50 Purchase Habits")

#Restore locale
Sys.setlocale(category="LC_ALL", locale = "English_United States.1252")