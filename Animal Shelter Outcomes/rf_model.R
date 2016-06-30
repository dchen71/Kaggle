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
train = as.data.frame(train)

##Feature Engineering
#Convert datetime into year, day of week, hour
train$OutcomeYear = as.factor(year(train$DateTime))
train$OutcomeMonth = as.factor(month(train$DateTime))
train$OutcomeDay = as.factor(day(train$DateTime))
train$OutcomeWeekdate = as.factor(weekdays(as.Date(train$DateTime)))
train$OutcomeHour = hour(train$DateTime)

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

#Find number of days from AgeuponOutcome
#365 days = 1 year, 30 days = 1 month, 7 days = 1 week
train$AgeuponOutcome[train$AgeuponOutcome == ""] = "Unknown"
train$AgeuponOutcome[train$AgeuponOutcome == "1 weeks"] = "1 week"

train$AgeInDaysUponOutcome = NA
#Hash table for days
daysTable = list(days = 1,
                 day = 1,
                 weeks = 7,
                 week = 7,
                 months = 30,
                 month = 30,
                 year = 365,
                 years = 365)
for(i in 1:nrow(train)){
  if(train$AgeuponOutcome[i] != "Unknown"){
    row = strsplit(train$AgeuponOutcome[i], " ")
    train$AgeInDaysUponOutcome[i] = as.integer(row[[1]][1]) * as.integer(daysTable[row[[1]][2]]) * 7
  }
}
#Compensate for the 0 years with average days for those under 1 year
train$AgeInDaysUponOutcome[which(train$AgeInDaysUponOutcome == 0)] = mean(train$AgeInDaysUponOutcome[which(train$AgeInDaysUponOutcome < 365)])
train$AgeInDaysUponOutcome[which(is.na(train$AgeInDaysUponOutcome))] = "Unknown"

#Column to check basically if an animal has a name
train$HasName = ifelse(nchar(train$Name) == 0, FALSE, TRUE)

#Deal with blank value in outcomesubtype
train$OutcomeSubtype = ifelse(nchar(train$OutcomeSubtype) == 0, "Unknown", train$OutcomeSubtype)

#Resubset the test and training data
train = lapply(train, as.factor)
train = as.data.frame(train)
test = tail(train, nrow(raw_test))
train = head(train, nrow(raw_train))

#Remove extra test factor from OutcomeType
train$OutcomeType = droplevels(train$OutcomeType)
train$OutcomeSubtype = droplevels(train$OutcomeSubtype)

#Remove predictor variable
test$OutcomeType = NULL
test$OutcomeSubtype = NULL

## Modeling
#Create model using randomForest
set.seed(1)
rfModel = randomForest(OutcomeType ~ AnimalType + SexuponOutcome + AgeInDaysUponOutcome +  OutcomeYear + OutcomeMonth + OutcomeDay + OutcomeWeekdate + OutcomeHour + Mix + HasName, data=train, ntree=200)
PredTest = predict(rfModel, newdata=test, type="vote")

#Preps file for kaggle submission
MySubmission = data.frame(ID = 1:nrow(test), 
                          PredTest)

#Creates csv for kaggle
write.csv(MySubmission, "prediction.csv", row.names=FALSE)
