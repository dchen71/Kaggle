# KAGGLE COMPETITION - DEALING WITH THE TEXT DATA

#Reads the training and test data
eBayTrain = read.csv("eBayiPadTrain.csv", stringsAsFactors=FALSE)
eBayTest = read.csv("eBayiPadTest.csv", stringsAsFactors=FALSE)

#Loads TM,ROCR, randomForest
library(tm)
library(ROCR)
library(randomForest)

#Converts all to factors
test = lapply(eBayTest, as.factor)
train = lapply(eBayTrain,as.factor)

test$startprice = as.numeric(test$startprice)
train$startprice = as.numeric(train$startprice)

levels(test$description) = levels(train$description)
levels(test$biddable) = levels(train$biddable)
levels(test$condition) = levels(train$condition)
levels(test$cellular) = levels(train$cellular)
levels(test$carrier) = levels(train$carrier)
levels(test$color) = levels(train$color)
levels(test$storage) = levels(train$storage)
levels(test$productline) = levels(train$productline)

#Setup the randomforest
descriptRF = randomForest(sold ~ biddable + startprice  + productline + condition 
                          + carrier + color + storage, data=train)

#Test model for AUC:
predictRF = predict(descriptRF, newdata=train)

#Checks the AUC value
predictRF = as.numeric(predictRF)
ROCRpred = prediction(predictRF, train$sold)
as.numeric(performance(ROCRpred, "auc")@y.values)
importance(descriptRF)

#Revised RF based on importance from before, auc prior was ~.93
testRF = randomForest(sold ~ productline + condition 
                      + carrier + color + storage, data=train)

#Test prediction for AUC:
predicttestRF = predict(testRF, newdata=train)

#Checks the AUC value, ~.88
predicttestRF = as.numeric(predicttestRF)
ROCRpred = prediction(predicttestRF, train$sold)
as.numeric(performance(ROCRpred, "auc")@y.values)
importance(testRF)

#Create a corpus from the description variable
CorpusDescription = Corpus(VectorSource(c(eBayTrain$description, eBayTest$description)))

#Preprocessing of Corpus based on description
CorpusDescription = tm_map(CorpusDescription, content_transformer(tolower), lazy=TRUE)
CorpusDescription = tm_map(CorpusDescription, PlainTextDocument, lazy=TRUE)
CorpusDescription = tm_map(CorpusDescription, removePunctuation, lazy=TRUE)
CorpusDescription = tm_map(CorpusDescription, removeWords, stopwords("english"), lazy=TRUE)
CorpusDescription = tm_map(CorpusDescription, stemDocument, lazy=TRUE)

#Convert corpus to a DocumentTermMatrix, remove sparse terms, and turn it into a data frame. 
#Filter threshold of .99
dtm = DocumentTermMatrix(CorpusDescription)
sparse = removeSparseTerms(dtm, 0.99)
DescriptionWords = as.data.frame(as.matrix(sparse))

#Ensure variable names are readable for R
colnames(DescriptionWords) = make.names(colnames(DescriptionWords))

#Merge datasets together and break again in order to get equal factors
eBay = merge(eBayTest,eBayTrain, all.x = TRUE, all.y = TRUE, sort = FALSE)

eBay$storage = as.factor(eBay$storage)
eBay$biddable = as.factor(eBay$biddable)
eBay$condition = as.factor(eBay$condition)
eBay$productline = as.factor(eBay$productline)
eBay$condition = as.factor(eBay$condition)
eBay$carrier = as.factor(eBay$carrier)
eBay$color = as.factor(eBay$color)
eBay$cellular = as.factor(eBay$cellular)

eBayTest = head(eBay, nrow(eBayTest))
eBayTrain = tail(eBay, nrow(eBayTrain))

eBayTest$sold = NULL

#Resplit entries back into training and test data sets
DescriptionWordsTrain = head(DescriptionWords, nrow(eBayTrain))
DescriptionWordsTest = tail(DescriptionWords, nrow(eBayTest))

#Add dependent variable to the training set
#Add variables biddable + startprice  + productline + condition, wordcount
#Converts data to factor
DescriptionWordsTrain$sold = eBayTrain$sold

DescriptionWordsTrain$WordCount = eBayTrain$WordCount
DescriptionWordsTest$WordCount = eBayTest$WordCount

DescriptionWordsTrain$biddable = eBayTrain$biddable
DescriptionWordsTest$biddable = eBayTest$biddable

DescriptionWordsTrain$condition = eBayTrain$condition
DescriptionWordsTest$condition = eBayTest$condition

DescriptionWordsTrain$productline = eBayTrain$productline
DescriptionWordsTest$productline = eBayTest$productline

DescriptionWordsTrain$startprice = eBayTrain$startprice
DescriptionWordsTest$startprice = eBayTest$startprice

DescriptionWordsTrain$condition = eBayTrain$condition
DescriptionWordsTest$condition = eBayTest$condition

DescriptionWordsTest$carrier = eBayTest$carrier
DescriptionWordsTrain$carrier = eBayTrain$carrier

DescriptionWordsTest$color = eBayTest$color
DescriptionWordsTrain$color = eBayTrain$color

#Checks the AUC value, 
testRF = randomForest(as.factor(sold) ~ apple + back + box + brand + case + charger + condition + clean + corner + cosmet + crack + dent + excellent + fulli + functional + good + great + includ + ipad + light + mint + new + normal + perfect + scratch + screen + scuff + tear + use + work + productline + startprice, data=DescriptionWordsTrain)
predicttestRF = predict(testRF, newdata=DescriptionWordsTrain)

predicttestRF = as.numeric(predicttestRF)
ROCRpred = prediction(predicttestRF, DescriptionWordsTrain$sold)
as.numeric(performance(ROCRpred, "auc")@y.values)
importance(testRF)

#Setup the randomforest
descriptRF = randomForest(as.factor(sold) ~ ., data=DescriptionWordsTrain)

# Make predictions:
predictRF = predict(descriptRF, newdata=DescriptionWordsTest)


#Preps file for kaggle submission
MySubmission = data.frame(UniqueID = eBayTest$UniqueID, Probability1 = predictRF)

#Creates csv for kaggle
write.csv(MySubmission, "SubmissionRF.csv", row.names=FALSE)

