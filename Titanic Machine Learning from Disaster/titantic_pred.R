# KAGGLE COMPETITION - Titanic: Machine Learning from Disaster

#Reads the training and test data
train = read.csv("train.csv", stringsAsFactors=FALSE)
test = read.csv("test.csv", stringsAsFactors=FALSE)

#Loads packages to use
library(ROCR)
library(randomForest)

#RF on class and prediction => .65% accuracy
#classRF = randomForest(as.factor(Survived) ~ Pclass, data=train)
#PredTest = predict(classRF, newdata=test)

#Convert male = 0, female = 1 for sex
train$Sex[which(train$Sex == 'female')] = 1
train$Sex[which(train$Sex == 'male')] = 0
test$Sex[which(test$Sex == 'female')] = 1
test$Sex[which(test$Sex == 'male')] = 0


#RF on class and sex and prediction => 77% accuracy
classRF = randomForest(as.factor(Survived) ~ Pclass + Sex, data=train)
PredTest = predict(classRF, newdata=test)


#Preps file for kaggle submission
MySubmission = data.frame(PassengerID = test$PassengerId, Survived = PredTest)

#Creates csv for kaggle
write.csv(MySubmission, "prediction.csv", row.names=FALSE)
