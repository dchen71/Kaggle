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
total$family = total$SibSp + total$Parch

#Adding Mother
total$Mother<-0
total$Mother[full$Sex=='female' & full$Parch>0 & full$Age>17 & full$Title!='Miss']<-1

#Determines if person is a child(age < 18)
total$child = total$Age < 18

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

# Clean up the cabin values
total$Cabin_clean = 0

for(i in 1:length(total$Cabin)){
    
    if(grepl("A", total$Cabin[i])){total$Cabin_clean[i] <- 1}
    if(grepl("B", total$Cabin[i])){total$Cabin_clean[i] <- 2}
    if(grepl("C", total$Cabin[i])){total$Cabin_clean[i] <- 3}
    if(grepl("D", total$Cabin[i])){total$Cabin_clean[i] <- 4}
    if(grepl("F", total$Cabin[i])){total$Cabin_clean[i] <- 5}
    if(grepl("E", total$Cabin[i])){total$Cabin_clean[i] <- 5}
    #else{total$Cabin_clean[i] <- 0}
}
table(total$Cabin_clean)
total$Cabin_clean <- as.integer(total$Cabin_clean)

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

#RF pclass,sex,age,sibsp,parch,fare,family,child,embarked, title => .789
classRF = randomForest(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + family + child + Embarked + title, data=train,ntree=2000, importance=TRUE)
PredTest = predict(classRF, newdata=test, type="class")

#Preps file for kaggle submission
MySubmission = data.frame(PassengerID = test$PassengerId, Survived = PredTest)

#Creates csv for kaggle
write.csv(MySubmission, "prediction.csv", row.names=FALSE)
