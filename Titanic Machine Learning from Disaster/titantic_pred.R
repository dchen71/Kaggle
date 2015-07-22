# KAGGLE COMPETITION - Titanic: Machine Learning from Disaster

#Reads the training and test data
train = read.csv("train.csv", stringsAsFactors=FALSE)
test = read.csv("test.csv", stringsAsFactors=FALSE)

#Loads packages to use
library(ROCR)
library(randomForest)

#RF on class and prediction
classRF = randomForest(as.factor(Survived) ~ Pclass, data=train)
PredTest = predict(classRF, newdata=test)

#Preps file for kaggle submission
MySubmission = data.frame(PassengerID = test$PassengerID, Survived = PredTest)

#Creates csv for kaggle
write.csv(MySubmission, "prediction.csv", row.names=FALSE)
