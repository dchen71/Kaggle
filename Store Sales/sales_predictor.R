#Kaggle Competition - Rossmann Store Sales

#Read input
dir = 'input/'
store = read.csv(paste0(dir,"store.csv"))
train = read.csv(paste0(dir,"train.csv"))
test = read.csv(paste0(dir,"test.csv"))

#Setup cross validation set
library(caret)

part = createDataPartition(train$Sales, p = 0.6, list = FALSE)
trainset = train[part,]
testset = train[-part,]