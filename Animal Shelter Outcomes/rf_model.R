## Kaggle - Shelter Animal Outcomes
## Random forest model

#Libraries
library(randomForest)

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

#Resubset the test and training data
test = tail(train, nrow(raw_test))
train = train[1:nrow(raw_train),]

#Remove predictor variable
test$OutcomeType = NULL
test$OutcomeSubtype = NULL

#Convert datetime into year, day of week
#Convert ageuponoutcome into float