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

#Age NA take the average age of all passengers
meanAge = mean(total$Age, na.rm=TRUE)
total$Age[which(is.na(total$Age))] = meanAge

#Convert the blank embarked values into the most common one, S
total$Embarked[total$Embarked == ''] = 'S'

#Fix NA of test$Fare based on mean of pclass 3
meanclass3 = mean(total$Fare[total$Pclass == 3], na.rm = TRUE)
total$Fare[which(is.na(total$Fare))] = meanclass3

#Calculates total family members of a person
total$Family = total$SibSp + total$Parch

#Adding Mother
total$Mother = 0
total$Mother[total$Sex == 'female' & total$Parch > 0 & total$Age > 17 & total$Title == 'Mrs'] = 1

#Determines if person is a child(age < 18)
total$Child = total$Age < 18

# Make the title column
for(i in 1:length(total$Name)){
    total$title[i] = 'None'
    if(grepl('Mr.', total$Name[i])){total$title[i] <- 'Mr'}
    if(grepl('Miss.', total$Name[i])){total$title[i] <- 'Miss'}
    if(grepl('Mrs.', total$Name[i])){total$title[i] <- 'Mrs'}
    if(grepl('Master.', total$Name[i])){total$title[i] <- 'Master'}
    if(grepl('Dr.', total$Name[i])){total$title[i] <- 'Doctor'}
    if(grepl('Major.', total$Name[i])){total$title[i] <- 'Major'}
}
total$title <- as.factor(total$title)

#Convert to factors
total$Sex = as.factor(total$Sex)
total$Survived = as.factor(total$Survived)
total$Child = as.factor(total$Child)
total$Embarked = as.factor(total$Embarked)

#Remake the train and test
train = head(total, nrow(train))
test = tail(total, nrow(test))

##
## Model and Prediction
##

#Random Forest and prediction
classRF = randomForest(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Family + Mother + Child + Embarked + title, data=train,ntree=2000, importance=TRUE)
PredTest = predict(classRF, newdata=test, type="class")

#Preps file for kaggle submission
MySubmission = data.frame(PassengerID = test$PassengerId, Survived = PredTest)

#Creates csv for kaggle
write.csv(MySubmission, "prediction.csv", row.names=FALSE)
