#Kaggle Competition - What's Cooking
#Predict category of cuisine based on ingredients

#Init libraries
library(jsonlite) #Read json
library(tm) #Parse ingredients and get corpus

#Read input
test = fromJSON("test.json", flatten=TRUE)
train = fromJSON("train.json", flatten=TRUE)

#Preprocesses the data
preProcess = function(df){
    df$ingredients = lapply(df$ingredients, FUN=function(x) gsub("-", "_", x)) # Subs - with _
    df$ingredients = lapply(df$ingredients, FUN=function(x) gsub("[^a-z0-9_ ]", "", x)) # Allow regular character and spaces    
    return(df)
}

test = preProcess(test)
train = preProcess(train)

# Create a corpus of terms for train and test
ingred_corpus = c(Corpus(VectorSource(train$ingredients)), Corpus(VectorSource(test$ingredients)))

# Create document term matrix
##Maybe do preprocessing on documenttermmatrix
ingred_DTM = DocumentTermMatrix(ingred_corpus)
ingred_DTM = removeSparseTerms(ingred_DTM, 0.995) # 99.5% occurence only
ingred_DTM = as.data.frame(as.matrix(ingred_DTM))


#Write to csv
submission = data.frame(test$id)
names(submission) = 'id'
#submission$cusine = predModel
write.csv(submission,filename="submission.csv",row.names=FALSE)