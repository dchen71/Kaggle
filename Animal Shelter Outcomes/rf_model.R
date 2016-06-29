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

#Convert ageuponoutcome into float based on years
#Consider simply converting this to be based on days rather than factor
train$AgeuponOutcome[train$AgeuponOutcome == "1 weeks"] = "1 week"
train$AgeuponOutcome[train$AgeuponOutcome == ""] = "Unknown"
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
train$AgeuponOutcome[train$AgeuponOutcome == "5 weeks"] = "1 month"

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
rfModel = randomForest(OutcomeType ~ AnimalType + SexuponOutcome + AgeuponOutcome +  OutcomeYear + OutcomeMonth + OutcomeDay + OutcomeWeekdate + OutcomeHour + Mix + HasName, data=train, ntree=200)
PredTest = predict(rfModel, newdata=test, type="class")

#Preps file for kaggle submission
MySubmission = data.frame(ID = 1:nrow(test), 
                          Adoption = 0, 
                          Died = 0, 
                          Return_to_owner = 0, 
                          Transfer = 0)

for(i in 1:nrow(test)){
  if(as.character(PredTest[i]) == "Adoption"){
    MySubmission$Adoption[i] = 1
  } else if(as.character(PredTest[i]) == "Adoption"){
    MySubmission$Adoption[i] = 1
  } else if(as.character(PredTest[i]) == "Died"){
    MySubmission$Died[i] = 1
  } else if(as.character(PredTest[i]) == "Return_to_owner"){
    MySubmission$Return_to_owner[i] = 1
  } else if(as.character(PredTest[i]) == "Transfer"){
    MySubmission$Transfer[i] = 1
  }
}

#Creates csv for kaggle
write.csv(MySubmission, "prediction.csv", row.names=FALSE)
