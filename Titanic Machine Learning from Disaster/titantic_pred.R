# KAGGLE COMPETITION - Titanic: Machine Learning from Disaster

#Reads the training and test data and creates a merge data set
train = read.csv("train.csv", stringsAsFactors=FALSE)
test = read.csv("test.csv", stringsAsFactors=FALSE)
total = merge(train,test,all.x=TRUE, all.y=TRUE, sort= FALSE)

#Loads packages to use
library(ROCR)
library(randomForest)

#Sets seed for reproducibility
set.seed(100)

##
## Data Processing
##

#Convert male = 0, female = 1 for sex
total$Sex[which(total$Sex == 'female')] = 1
total$Sex[which(total$Sex == 'male')] = 0

#Age NA take the average age of all passengers
meanAge = mean(total$Age, na.rm=TRUE)
total$Age[which(is.na(total$Age))] = meanAge

#Convert the blank embarked values into the most common one, S
total$Embarked[total$Embarked == ''] = 'S'

#Fix NA of test$Fare based on mean of pclass 3
meanclass3 = mean(total$Fare[total$Pclass == 3], na.rm = TRUE)
total$Fare[which(is.na(total$Fare))] = meanclass3

#Calculates total family members of a person
total$family = total$SibSp + total$Parch

#Determines if person is a child(age < 18), 0 = no, 1 = yes
total$child = total$Age < 18

#Convert to factors
total$Sex = as.factor(total$Sex)
total$Survived = as.factor(total$Survived)
total$child = as.factor(total$child)
total$Embarked = as.factor(total$Embarked)

#Remake the train and test
train = head(total, nrow(train))
test = tail(total, nrow(test))

##
## Model and Prediction
##

#RF testing sex,fare,age => .76
#classRF = randomForest(as.factor(Survived) ~ Sex + Age + Fare, data=train)
#PredTest = predict(classRF, newdata=test)

#RF pclass,sex,age,sibsp,parch,fare,family,child,embarked => .785
classRF = randomForest(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + family + child + Embarked, data=train,ntree=2000, importance=TRUE)
PredTest = predict(classRF, newdata=test)

#Testing conditional random forest example
library(party)
classRF = cforest(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + family + child, data=train, controls= cforest_unbiased(ntree=2000, mtry=3))
PredTest <- predict(classRF, test, OOB=TRUE, type = "response")

#Preps file for kaggle submission
MySubmission = data.frame(PassengerID = test$PassengerId, Survived = PredTest)

#Creates csv for kaggle
write.csv(MySubmission, "prediction.csv", row.names=FALSE)
