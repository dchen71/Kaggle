# KAGGLE COMPETITION - DEALING WITH THE TEXT DATA

#Reads the training and test data
eBayTrain = read.csv("eBayiPadTrain.csv", stringsAsFactors=FALSE)
eBayTest = read.csv("eBayiPadTest.csv", stringsAsFactors=FALSE)

#Loads TM and ROCR package
library(tm)
library(ROCR)

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
DescriptionWordsTrain$sold = eBayTrain$sold

DescriptionWordsTrain$WordCount = eBayTrain$WordCount
DescriptionWordsTest$WordCount = eBayTest$WordCount
DescriptionWordsTrain$biddable = eBayTrain$biddable
DescriptionWordsTest$biddable = eBayTest$biddable
DescriptionWordsTrain$condition = eBayTrain$condition
DescriptionWordsTest$condition = eBayTest$condition
DescriptionWordsTrain$productline = eBayTrain$productline
DescriptionWordsTest$productline = eBayTest$productline
DescriptionWordsTrain$storage = eBayTrain$storage
DescriptionWordsTest$storage = eBayTest$storage


#Logistic regression model using all of the variables
DescriptionWordsLog = glm(sold ~ ., data=DescriptionWordsTrain, family=binomial)

#Predictions on our test set:
PredTest = predict(DescriptionWordsLog, newdata=DescriptionWordsTest, type="response")

#Checks the AUC value
#ROCRpred = prediction(PredTest, DescriptionWordsTest$sold)
#as.numeric(performance(ROCRpred, "auc")@y.values)

#Preps file for kaggle submission
MySubmission = data.frame(UniqueID = eBayTest$UniqueID, Probability1 = PredTest)

#Creates csv for kaggle
write.csv(MySubmission, "SubmissionDescriptionLog.csv", row.names=FALSE)
