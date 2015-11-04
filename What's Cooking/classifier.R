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




#Write to csv
submission = data.frame(test$id)
names(submission) = 'id'
#submission$cusine = predModel
write.csv(submission,filename="submission.csv",row.names=FALSE)