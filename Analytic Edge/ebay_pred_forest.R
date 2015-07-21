# KAGGLE COMPETITION - DEALING WITH THE TEXT DATA

#CART seems to be more towards classification so might not be useful p wise

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
descriptRF = randomForest(sold ~ biddable + startprice + condition + cellular + carrier + color + storage + productline, data=train)

# Make predictions:
predictRF = predict(descriptRF, newdata=test, type="prob")[,2]

#Need to find and optimize the auc here

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

#Resplit entries back into training and test data sets
DescriptionWordsTrain = head(DescriptionWords, nrow(eBayTrain))
DescriptionWordsTest = tail(DescriptionWords, nrow(eBayTest))

#Add dependent variable to the training set
#To both data sets, add WordCount, biddable
#Converts data to factor
DescriptionWordsTrain$sold = as.factor(eBayTrain$sold)

DescriptionWordsTrain$WordCount = as.factor(eBayTrain$WordCount)
levels(DescriptionWordsTest$WordCount) = levels(DescriptionWordsTrain$WordCount)
DescriptionWordsTest$WordCount = as.factor(eBayTest$WordCount)

DescriptionWordsTrain$biddable = as.factor(eBayTrain$biddable)
levels(DescriptionWordsTest$biddable) = levels(DescriptionWordsTrain$biddable)

DescriptionWordsTest$biddable = as.factor(eBayTest$biddable)
DescriptionWordsTrain$condition = as.factor(eBayTrain$condition)
levels(DescriptionWordsTest$condition) = levels(DescriptionWordsTrain$condition)

DescriptionWordsTest$condition = as.factor(eBayTest$condition)
DescriptionWordsTrain$productline = as.factor(eBayTrain$productline)
levels(DescriptionWordsTest$productline) = levels(DescriptionWordsTrain$productline)

DescriptionWordsTest$productline = as.factor(eBayTest$productline)
DescriptionWordsTrain$storage = as.factor(eBayTrain$storage)
levels(DescriptionWordsTest$storage) = levels(DescriptionWordsTrain$storage)

DescriptionWordsTest$storage = as.factor(eBayTest$storage)

DescriptionWordsTest$work = as.factor(DescriptionWordsTest$work)
DescriptionWordsTrain$work = as.factor(DescriptionWordsTrain$work)
levels(DescriptionWordsTest$work) = levels(DescriptionWordsTrain$work)

DestiptionWordsTest = lapply(DescriptionWordsTest[sapply(DescriptionWordsTest,is.numeric)], as.factor)
DestiptionWordsTrain = lapply(DescriptionWordsTrain[sapply(DescriptionWordsTrain,is.numeric)], as.factor)

#Setup the randomforest
descriptRF = randomForest(sold ~ ., data=DescriptionWordsTrain)

# Make predictions:
predictRF = predict(descriptRF, newdata=DescriptionWordsTest)

#Preps file for kaggle submission
MySubmission = data.frame(UniqueID = eBayTest$UniqueID, Probability1 = PredTest)

#Creates csv for kaggle
write.csv(MySubmission, "SubmissionRF.csv", row.names=FALSE)

