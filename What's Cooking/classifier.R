#Kaggle Competition - What's Cooking
#Predict category of cuisine based on ingredients

#Read input
library(jsonlite)
test = fromJSON("test.json")
train = fromJSON("train.json")
