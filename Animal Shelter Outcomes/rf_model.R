## Kaggle - Shelter Animal Outcomes
## Random forest model

#Libraries
library(randomForest)
library(lubridate)
library(caret)

#Read input data
raw_train = read.csv("input/train.csv", stringsAsFactors = FALSE)
raw_test = read.csv("input/test.csv", stringsAsFactors = FALSE)

#Add na columns to test to merge
names(raw_test)[1] = "AnimalID"
raw_test$OutcomeType = "test"
raw_test$OutcomeSubtype = "test"

#Take in all values, and convert to new factors
train = rbind(raw_train, raw_test)
train = as.data.frame(train)

##Feature Engineering
#Convert datetime into year, day of week, hour
train$Year = as.factor(year(train$DateTime))
train$Month = as.factor(month(train$DateTime))
train$Day = as.factor(day(train$DateTime))
train$Weekdate = as.factor(weekdays(as.Date(train$DateTime)))
train$Hour = hour(train$DateTime)

#Clean up data on sexuponoutcome
train$SexuponOutcome[train$SexuponOutcome == ""] = "Unknown"
#train$Gender = "Unknown"
#train$Gender[grep("Male", train$SexuponOutcome)] = "Male"
#train$Gender[grep("Female", train$SexuponOutcome)] = "Female"
#train$Fertile = "Unknown" #Neutered/Spayed = FALSE
#train$Fertile[grep("Intact", train$SexuponOutcome)] = TRUE
#train$Fertile[grep("Neutered", train$SexuponOutcome)] = FALSE
#train$Fertile[grep("Spayed", train$SexuponOutcome)] = FALSE
#train$SexuponOutcome = NULL

#Deal with potential mixes, anything with mix or / = mix
train$Mix = FALSE
train$Mix[grep("/", train$Breed)] = TRUE
train$Mix[grep("Mix", train$Breed)] = TRUE

#Convert ageuponoutcome into float based on years
train$AgeuponOutcome[train$AgeuponOutcome == "1 weeks"] = "1 week"
train$AgeuponOutcome[train$AgeuponOutcome == ""] = NA
train$AgeuponOutcome[train$AgeuponOutcome == "1 day"] = "<1 week" #Segregate 1-6 days as <1 week
train$AgeuponOutcome[train$AgeuponOutcome == "2 days"] = "<1 week"
train$AgeuponOutcome[train$AgeuponOutcome == "3 days"] = "<1 week"
train$AgeuponOutcome[train$AgeuponOutcome == "4 days"] = "<1 week"
train$AgeuponOutcome[train$AgeuponOutcome == "5 days"] = "<1 week"
train$AgeuponOutcome[train$AgeuponOutcome == "6 days"] = "<1 week"
train$AgeuponOutcome[train$AgeuponOutcome == "1 week"] = "<1 month" #Segregate 1-3 weeks as   <1 month
train$AgeuponOutcome[train$AgeuponOutcome == "2 weeks"] = "<1 month" #Segregate 1-3 weeks as   <1 month
train$AgeuponOutcome[train$AgeuponOutcome == "3 weeks"] = "<1 month" #Segregate 1-3 weeks as   <1 month
train$AgeuponOutcome[train$AgeuponOutcome == "4 weeks"] = "1 month" #4 weeks cleaned to 1 month


#Resubset the test and training data
train = lapply(train, as.factor)
test = tail(train, nrow(raw_test))
train = head(train, nrow(raw_train))

#Remove predictor variable
test$OutcomeType = NULL
test$OutcomeSubtype = NULL

