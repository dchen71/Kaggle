##
## KAGGLE COMPETITION - Titanic: Machine Learning from Disaster
## Prediction based on association rules
##

#
## Load data and necessary packages
#

#Reads the training and test data and creates a merge data set
train = read.csv("train.csv", stringsAsFactors=FALSE)
test = read.csv("test.csv", stringsAsFactors=FALSE)
total = merge(train,test,all.x=TRUE, all.y=TRUE, sort= FALSE)

#Loads packages to use
library(arules)
library(dplyr)

#
## Data Engineering
#

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

#Save unmodified version
total.all = total
test.all = tail(total, nrow(test))

#Convert to factors
total = total %>% mutate_if(is.integer, as.factor) %>% mutate_if(is.numeric, as.factor)
total$Sex = as.factor(total$Sex)
total$Embarked = as.factor(total$Embarked)

#Remove useless variables for transaction conversions
total$Name = NULL
total$Ticket = NULL
total$Cabin = NULL
total$Fare = NULL
total$PassengerId = NULL

#Remake the train and test
train = head(total, nrow(train))
test = tail(total, nrow(test))

##
## Model and Prediction
##

rules.all = apriori(train) #Find the rules for the transactions
trans.train = as(train, "transactions")
trans.test = as(test, "transactions")
dis_rules = dissimilarity(trans.train) #Find the dissimilarity of the transaction object
rules_hclust = hclust(dis_rules) #Hierarchical clustering
rules_cut = cutree(rules_hclust, k=2) #Cut tree to get groups
pred = predict(trans.train, trans.test, rules_cut) #Get predicted grouping

#Change results of prediction to 0 for deceased and 1 for alive
test$Survived = pred #Add in prediction
test$Survived = ifelse(test$Survived == 1, 0, 1)
test$PassengerId = test.all$PassengerId

#Save test file
write.csv(test[,c("PassengerId", "Survived")],"prediction.csv", row.names=FALSE)
