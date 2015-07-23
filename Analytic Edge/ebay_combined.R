# KAGGLE COMPETITION - DEALING WITH THE TEXT DATA

##
## Preprocessing
##

#Reads the training and test data
eBayTrain = read.csv("eBayiPadTrain.csv", stringsAsFactors=FALSE)
eBayTest = read.csv("eBayiPadTest.csv", stringsAsFactors=FALSE)
train_mod = eBayTrain

#Loads TM,ROCR, randomForest, corrplot
library(tm)
library(ROCR)
library(randomForest)
library(corrplot)
library(caTools)

#Merge the dataset into a single data frame
total = merge(eBayTrain, eBayTest, all.x=TRUE, all.y=TRUE, sort=FALSE)

#Clean up ipad mini retina(might be ipad mini 3) and set all to unknown
train_mod$productline[train_mod$productline == 'iPad mini Retina'] = 'Unknown'
total$productline[total$productline == 'iPad mini Retina'] = 'Unknown'

#Set unknown ipad 5 to unknown
total$productline[total$productline == 'iPad 5'] = 'Unknown'
train_mod$productline[total$productline == 'iPad 5'] = 'Unknown'

#Convert to factors
total$biddable = as.factor(total$biddable)
total$condition = as.factor(total$condition)
total$cellular = as.factor(total$cellular)
total$carrier = as.factor(total$carrier)
total$color = as.factor(total$color)
total$storage = as.factor(total$storage)
total$productline = as.factor(total$productline)
total$sold = as.factor(total$sold)

#Want to build average price sold /model/wireless/storage

#Avgvalue based on avg of bought within productline/
total$avgvalue = 0
total$avgvalue[total$productline == 'iPad 1'] = mean(train_mod$startprice[train_mod$productline == 'iPad 1' & train_mod$sold == 1])
total$avgvalue[total$productline == 'iPad 2'] = mean(train_mod$startprice[train_mod$productline == 'iPad 2' & train_mod$sold == 1])
total$avgvalue[total$productline == 'iPad 3'] = mean(train_mod$startprice[train_mod$productline == 'iPad 3' & train_mod$sold == 1])
total$avgvalue[total$productline == 'iPad 4'] = mean(train_mod$startprice[train_mod$productline == 'iPad 4' & train_mod$sold == 1])
total$avgvalue[total$productline == 'iPad mini'] = mean(train_mod$startprice[train_mod$productline == 'iPad mini' & train_mod$sold == 1])
total$avgvalue[total$productline == 'iPad mini 2'] = mean(train_mod$startprice[train_mod$productline == 'iPad mini 2' & train_mod$sold == 1])
total$avgvalue[total$productline == 'iPad mini 3'] = mean(train_mod$startprice[train_mod$productline == 'iPad mini 3' & train_mod$sold == 1])
total$avgvalue[total$productline == 'iPad Air'] = mean(train_mod$startprice[train_mod$productline == 'iPad Air' & train_mod$sold == 1])
total$avgvalue[total$productline == 'iPad Air 2'] = mean(train_mod$startprice[train_mod$productline == 'iPad Air 2' & train_mod$sold == 1])
total$avgvalue[total$productline == 'Unknown'] = mean(train_mod$startprice[train_mod$productline == 'Unknown' & train_mod$sold == 1])
total$avgvalue = as.factor(total$avgvalue)

#iPad 1
#total$msrp[total$productline == 'iPad 1' & total$cellular == 0 total$storage == 16] = 499
#total$msrp[total$productline == 'iPad 1' & total$cellular == 1 total$storage == 16] = 629
#total$msrp[total$productline == 'iPad 1' & total$cellular == 0 total$storage == 32] = 599
#total$msrp[total$productline == 'iPad 1' & total$cellular == 1 total$storage == 32] = 699
#total$msrp[total$productline == 'iPad 1' & total$cellular == 0 total$storage == 64] = 729
#total$msrp[total$productline == 'iPad 1' & total$cellular == 1 total$storage == 64] = 829
#total$msrp[total$productline == 'iPad 1' & total$cellular == 0 total$storage == 'Unknown'] = 499
#total$msrp[total$productline == 'iPad 1' & total$cellular == 1 total$storage == 'Unknown'] = 629
#total$msrp[total$productline == 'iPad 1' & total$cellular == 'Unknown' total$storage == 'Unknown'] = 499

#Split total back into training and testing for corpus
train = head(total,nrow(eBayTrain))
test = tail(total, nrow(eBayTest))

##
## CORPUS CREATION
##

#Create a corpus from the description variable
CorpusDescription = Corpus(VectorSource(c(train$description, test$description)))

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

DescriptionWordsTest$avgvalue = test$color
DescriptionWordsTrain$avgvalue = train$color

# Split the data
set.seed(50)
spl = sample.split(DescriptionWordsTrain$sold, SplitRatio = 0.6)
SoldTrain = subset(DescriptionWordsTrain, spl==TRUE)
SoldTest = subset(DescriptionWordsTrain, spl==FALSE)

#Checks the AUC value, 
testRF = randomForest(as.factor(sold) ~ box + come + condit + condition + cosmet + good + great + ipad + new + onli + scratch + screen + still + this + use + veri + work + biddable + productline + startprice + carrier + color + avgvalue, data=SoldTrain, ntree=500)
predicttestRF = predict(testRF, newdata=SoldTest, type="class")

predicttestRF = as.numeric(predicttestRF)
ROCRpred = prediction(predicttestRF, SoldTest$sold)
as.numeric(performance(ROCRpred, "auc")@y.values)
importance(testRF)

#AUC .85 -> .76097
#testRF = randomForest(as.factor(sold) ~ box + case + charger + condition + crack + dent + excellent + functional + good + great + includ + light + mint + new + normal + perfect + scratch + screen + scuff +  work + productline + startprice, data=DescriptionWordsTrain)

#AUC .825 -> .7594?
#testRF = randomForest(as.factor(sold) ~ case + charger + condition + crack + dent + excellent + great + new + perfect + scratch + scuff +  work + productline + startprice, data=DescriptionWordsTrain)

#Setup the randomforest
descriptRF = randomForest(as.factor(sold) ~ box + come + condit + condition + cosmet + good + great + ipad + new + onli + scratch + screen + still + this + use + veri + work + biddable + productline + startprice + carrier + color + avgvalue, data=DescriptionWordsTrain, ntree=500)

# Make predictions:
predictRF = predict(descriptRF, newdata=DescriptionWordsTest, type="class")

#Preps file for kaggle submission
MySubmission = data.frame(UniqueID = eBayTest$UniqueID, Probability1 = predictRF)

#Creates csv for kaggle
write.csv(MySubmission, "SubmissionRF.csv", row.names=FALSE)

