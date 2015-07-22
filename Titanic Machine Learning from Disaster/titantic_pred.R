# KAGGLE COMPETITION - Titanic: Machine Learning from Disaster

#Reads the training and test data and creates a merge data set
train = read.csv("train.csv", stringsAsFactors=FALSE)
test = read.csv("test.csv", stringsAsFactors=FALSE)
total = merge(train,test,all.x=TRUE, all.y=TRUE)

#Loads packages to use
library(ROCR)
library(randomForest)

#Sets seed for consistent results
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

#Fix NA of test$Fare based on mean of pclass 3
meanclass3 = mean(total$Fare[total$Pclass == 3], na.rm = TRUE)
total$Fare[which(is.na(total$Fare))] = meanclass3

#Calculates total family members of a person
total$family = total$SibSp + total$Parch

#Determines if person is a child(age < 18), 0 = no, 1 = yes
total$child = total$Age < 18
total$child[which(total$child)] = 1

#Convert to factors
total$Sex = as.factor(total$Sex)
total$Pclass = as.factor(total$Pclass)
#total$Age = as.factor(total$Age)
total$SibSp = as.factor(total$SibSp)
total$Parch = as.factor(total$Parch)
#total$Fare = as.factor(total$Fare)
total$family = as.factor(total$family)
total$Survived = as.factor(total$Survived)
total$child = as.factor(total$child)


#Remake the train and test
train = head(total, nrow(train))
test = tail(total, nrow(test))

##
## Model and Prediction
##

#RF on class and sex and prediction => 76.5% accuracy
#classRF = randomForest(as.factor(Survived) ~ Pclass + Sex, data=train)
#PredTest = predict(classRF, newdata=test)


#RF on class,sex,age and prediction => .75
#classRF = randomForest(as.factor(Survived) ~ Pclass + Sex + Age, data=train)
#PredTest = predict(classRF, newdata=test)

#RF testing fare and sex => 75%
#classRF = randomForest(as.factor(Survived) ~ Fare + Sex, data=train)
#PredTest = predict(classRF, newdata=test)

#RF testing sex and age => 76.5%
#classRF = randomForest(as.factor(Survived) ~ Age + Sex, data=train)
#PredTest = predict(classRF, newdata=test)

#RF testing class,sex,age,sibsp,parch,fare => .76
#classRF = randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare, data=train)
#PredTest = predict(classRF, newdata=test)

#varImpPlot(classRF)

#RF testing sex,fare,age => .76
#classRF = randomForest(as.factor(Survived) ~ Sex + Age + Fare, data=train)
#PredTest = predict(classRF, newdata=test)

#RF pclass,sex,age,sibsp,parch,fare,family,child => .76
classRF = randomForest(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + family + child, data=train)
PredTest = predict(classRF, newdata=test)


#Preps file for kaggle submission
MySubmission = data.frame(PassengerID = test$PassengerId, Survived = PredTest)

#Creates csv for kaggle
write.csv(MySubmission, "prediction.csv", row.names=FALSE)
