## Kaggle - Shelter Animal Outcomes
## Random forest model

#Libraries
library(randomForest)
library(lubridate)

#Read input data
raw_train = read.csv("input/train.csv", stringsAsFactors = FALSE)
raw_test = read.csv("input/test.csv", stringsAsFactors = FALSE)

#Add na columns to test to merge
names(raw_test)[1] = "AnimalID"
raw_test$OutcomeType = "test"
raw_test$OutcomeSubtype = "test"

#Take in all values, and convert to new factors
train = rbind(raw_train, raw_test)
train = lapply(train, as.factor)
train = as.data.frame(train)

##Feature Engineering
#Convert datetime into year, day of week, hour
train$Year = as.factor(year(train$DateTime))
train$Month = as.factor(month(train$DateTime))
train$Day = as.factor(day(train$DateTime))
train$Weekdate = as.factor(weekdays(as.Date(train$DateTime)))
train$Hour = hour(train$DateTime)

#Resubset the test and training data
test = tail(train, nrow(raw_test))
train = head(train, nrow(raw_train))

#Remove predictor variable
test$OutcomeType = NULL
test$OutcomeSubtype = NULL

#Convert ageuponoutcome into float
#Segregate sex
#Make column based on spayed/neutered/etc
#possibly make column based on mix or /
#Consider checking the time(at least am pm or hour wise)