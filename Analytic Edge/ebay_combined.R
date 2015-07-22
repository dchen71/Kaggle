# KAGGLE COMPETITION - DEALING WITH THE TEXT DATA

#Reads the training and test data
eBayTrain = read.csv("eBayiPadTrain.csv", stringsAsFactors=FALSE)
eBayTest = read.csv("eBayiPadTest.csv", stringsAsFactors=FALSE)

#Loads TM,ROCR, randomForest, corrplot
library(tm)
library(ROCR)
library(randomForest)
library(corplot)
library(caTools)

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

#Creates a correlation plot of the word variables
wordCor = cor(DescriptionWords)
corrplot(wordCor)

#Remove low importance words, < 2
testCor = data.frame(DescriptionWords$box)
testCor$come = DescriptionWords$come
testCor$condit = DescriptionWords$condit
testCor$cosmet = DescriptionWords$cosmet
testCor$good = DescriptionWords$good
testCor$great = DescriptionWords$great
testCor$ipad = DescriptionWords$ipad
testCor$new = DescriptionWords$new
testCor$onli = DescriptionWords$onli
testCor$scratch = DescriptionWords$scratch
testCor$screen = DescriptionWords$screen
testCor$still = DescriptionWords$still
testCor$this = DescriptionWords$this
testCor$use = DescriptionWords$use
testCor$veri = DescriptionWords$veri
testCor$work = DescriptionWords$work

#Creates a correlation plot of the > 2 imporance words
wordCor = cor(testCor)
corrplot(wordCor)

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
#Add variables from eBayTrain and wordcount to training/testing sets
DescriptionWordsTrain$sold = eBayTrain$sold

DescriptionWordsTrain$WordCount = eBayTrain$WordCount
DescriptionWordsTest$WordCount = eBayTest$WordCount

DescriptionWordsTrain$biddable = eBayTrain$biddable
DescriptionWordsTest$biddable = eBayTest$biddable

DescriptionWordsTrain$color = eBayTrain$color
DescriptionWordsTest$color = eBayTest$color

DescriptionWordsTrain$storage = eBayTrain$storage
DescriptionWordsTest$storage = eBayTest$storage

DescriptionWordsTrain$carrier = eBayTrain$carrier
DescriptionWordsTest$carrier = eBayTest$carrier

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

# Split the data
set.seed(50)
spl = sample.split(DescriptionWordsTrain$sold, SplitRatio = 0.6)
SoldTrain = subset(DescriptionWordsTrain, spl==TRUE)
SoldTest = subset(DescriptionWordsTrain, spl==FALSE)

#Checks the AUC value, 
testRF = randomForest(as.factor(sold) ~ box + come + condit + condition + cosmet + good + great + ipad + new + onli + scratch + screen + still + this + use + veri + work + biddable + productline + startprice + carrier + color, data=SoldTrain)
predicttestRF = predict(testRF, newdata=SoldTest)

predicttestRF = as.numeric(predicttestRF)
ROCRpred = prediction(predicttestRF, SoldTest$sold)
as.numeric(performance(ROCRpred, "auc")@y.values)
importance(testRF)

#AUC .85 -> .76097
#testRF = randomForest(as.factor(sold) ~ box + case + charger + condition + crack + dent + excellent + functional + good + great + includ + light + mint + new + normal + perfect + scratch + screen + scuff +  work + productline + startprice, data=DescriptionWordsTrain)

#AUC .825 -> .7594?
#testRF = randomForest(as.factor(sold) ~ case + charger + condition + crack + dent + excellent + great + new + perfect + scratch + scuff +  work + productline + startprice, data=DescriptionWordsTrain)

#Setup the randomforest
descriptRF = randomForest(as.factor(sold) ~ box + come + condit + condition + cosmet + good + great + ipad + new + onli + scratch + screen + still + this + use + veri + work + biddable + productline + startprice + carrier + color, data=DescriptionWordsTrain)

# Make predictions:
predictRF = predict(descriptRF, newdata=DescriptionWordsTest)

#Preps file for kaggle submission
MySubmission = data.frame(UniqueID = eBayTest$UniqueID, Probability1 = predictRF)

#Creates csv for kaggle
write.csv(MySubmission, "SubmissionRF.csv", row.names=FALSE)

