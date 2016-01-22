#Kaggle Competition - Product Search Relevance

#Read input
dir = 'input/'
train = read.csv(paste0(dir,"train.csv"))
test = read.csv(paste0(dir,"test.csv"))
#attributes = read.csv(paste0(dir, "attributes.csv")) #seems to have snippets of descriptions
descriptions = read.csv(paste0(dir, "product_descriptions.csv"))

#Merged products with their descriptions
mTrain = merge(train, descriptions)
mTest = merge(test, descriptions)