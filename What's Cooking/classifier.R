#Kaggle Competition - What's Cooking
#Predict category of cuisine based on ingredients

#Init libraries
library(jsonlite) #Read json
library(tm) #Parse ingredients and get corpus
library(randomForest) #randomForest

#Read input
test = fromJSON("test.json", flatten=TRUE)
train = fromJSON("train.json", flatten=TRUE)

#Preprocesses the data
preProcess = function(df){
    df$ingredients = lapply(df$ingredients, FUN=function(x) gsub("-", "_", x)) # Subs - with _
    df$ingredients = lapply(df$ingredients, FUN=function(x) gsub(" ", "_", x)) # Subs space with _
    df$ingredients = lapply(df$ingredients, FUN=function(x) gsub("[^a-z0-9_]", "", x)) # Allow regular character    
    return(df)
}

test = preProcess(test)
train = preProcess(train)

# Create a corpus of terms for train and test
##May want to look into ingredients with spaces or ' in them
ingred_corpus = c(Corpus(VectorSource(train$ingredients)), Corpus(VectorSource(test$ingredients)))

# Create document term matrix
##Maybe do preprocessing on documenttermmatrix
ingred_DTM = DocumentTermMatrix(ingred_corpus)
ingred_DTM = removeSparseTerms(ingred_DTM, 0.995) # 99.5% occurence only
ingred_DTM = as.data.frame(as.matrix(ingred_DTM))

# Features
##Add number of ingredients / recipe
ingred_DTM$num_ingred  = rowSums(ingred_DTM)

# Splits the document matrix back into train/test
word_train  = ingred_DTM[1:nrow(train), ]
word_test = ingred_DTM[-(1:nrow(train)), ]

#Add back dependent variable
word_train$cuisine = as.factor(train$cuisine)

#XGBoost model
##Prepare matrix for boosting
xgbMat = xgb.DMatrix(Matrix(data.matrix(word_train[,!colnames(word_train) %in% c("cuisine")])), 
                     label=as.numeric(word_train$cuisine) - 1)

##Train using softmax multi classiication
xgb = xgboost(xgbMat, max.depth = 25, eta = 0.3, nround = 200, objective = "multi:softmax", num_class = 20)

# Plot important features for boost
names = colnames(word_train[, !colnames(word_train) %in% c("cuisine")])
importance_matrix = xgb.importance(names, model = xgb)
xgb.plot.importance(importance_matrix[1:30,])

##Prediction dataset
xgb.submit = predict(xgb, newdata = data.matrix(word_test[, !colnames(word_test) %in% c("cuisine")]))
xgb.submit.text = levels(word_train$cuisine)[xgb.submit+1]

#Write to csv for XgBoost
submission = data.frame(test$id)
names(submission) = 'id'
submission$cuisine = xgb.submit.text
write.csv(submission,file="xg.csv",row.names=FALSE)

#Random Forest model
rfModel = randomForest(as.factor(cusine) ~ ., data = word_train)
predictRF = predict(rfModel, newdata=word_test)

#Write to csv for RandomForest
submission = data.frame(test$id)
names(submission) = 'id'
submission$cuisine = predictRF
write.csv(submission,file="rf.csv",row.names=FALSE)