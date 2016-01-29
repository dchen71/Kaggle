#Kaggle Competition - Product Search Relevance

library(readr)

#Read input
dir = 'input/'
train = read.csv(paste0(dir,"train.csv"))
test = read.csv(paste0(dir,"test.csv"))
descriptions = read.csv(paste0(dir, "product_descriptions.csv"))
attr = read.csv(paste0(dir, "attributes.csv"))

#Reshape the attribute dataset
reshape = function(df){
  uniques = unique(df$product_uid[!is.na(df$name)])
  uniques[80] = NULL #manually removing the NA value
  new_attr = data.frame(product_uid = uniques)
  for(i in 1:length(uniques)){
    product = subset(df, df$product_uid == uniques[i])
    for(j in 1:length(product$name)){
      category = as.character(product$name[j])
      if(category %in% names(new_attr)){
        new_attr[j, category] = as.character(product$value[j])
      } else{
        new_attr[, ncol(new_attr) + 1] = NA
        names(new_attr)[ncol(new_attr)] = category
        new_attr[j, category] = as.character(product$value[j])
      }
    }
    print(uniques[i])
  }
  
  return(new_attr)
}

test = reshape(attr)

#Merged products with their descriptions
train = merge(train, descriptions, by.x = "product_uid", by.y = "product_uid", all.x = TRUE, all.y = FALSE)
test = merge(test, descriptions, by.x = "product_uid", by.y = "product_uid", all.x = TRUE, all.y = FALSE)

#plans
#take whole words from search query and check name/description for it
#might need to manually change data from predictions to not go over or under cap

#Find matches between search_term and product_title/relevance, based on benchmark score script
word_match = function(words, title, desc){
  n_title = 0
  n_desc = 0
  words = unlist(strsplit(as.character(words)," "))
  nwords = length(words)
  for(i in 1:length(words)){
    pattern = paste("(^| )",words[i],"($| )",sep="")
    n_title = n_title + grepl(pattern,title,perl=TRUE,ignore.case=TRUE)
    n_desc = n_desc + grepl(pattern,desc,perl=TRUE,ignore.case=TRUE)
  }
  return(c(n_title,nwords,n_desc))
}

#Process train
train_words = as.data.frame(t(mapply(word_match,train$search_term,train$product_title,train$product_description)))
train$nmatch_title = train_words[,1]
train$nwords = train_words[,2]
train$nmatch_desc = train_words[,3]

#Process test
test_words = as.data.frame(t(mapply(word_match,test$search_term,test$product_title,test$product_description)))
test$nmatch_title = test_words[,1]
test$nwords = test_words[,2]
test$nmatch_desc = test_words[,3]

rm(train_words,test_words)

#Prediction using glm
glm_model = glm(relevance ~ nmatch_title + nmatch_desc + nwords,data=train)
test_relevance = predict(glm_model,test)
test_relevance = ifelse(test_relevance>3,3,test_relevance)
test_relevance = ifelse(test_relevance<1,1,test_relevance)

#Create submission
submission = data.frame(id=test$id, relevance=test_relevance)
write_csv(submission,"submission.csv")