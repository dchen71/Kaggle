#Kaggle Competition - Product Search Relevance

library(readr)

#Read input
dir = 'input/'
train = read.csv(paste0(dir,"train.csv"))
test = read.csv(paste0(dir,"test.csv"))
descriptions = read.csv(paste0(dir, "product_descriptions.csv"))
attr = read.csv(paste0(dir, "attributes.csv"))

#will need to parse num x num as well as 1 in/1 inch like stuff

#Reshape the attribute dataset with the top 10 attributes
reshape = function(df){
  #Subset out the product ids
  uniques = unique(df$product_uid)
  uniques = uniques[!is.na(uniques)]
  
  #Init new data frame containing the top ten values from attr
  new_attr = data.frame(product_uid = uniques, brand=NA, bullet01=NA, bullet02=NA, bullet03=NA,bullet04=NA, 
                        bullet05=NA, p_width=NA, p_height=NA, p_depth=NA, p_weight=NA)
  spec_names = c("Bullet01", "Bullet02", "Bullet03", "Bullet04", "Bullet05","Bullet06", 
                 "MFG Brand Name", "Product Width (in.)", "Product Height (in.)", 
                 "Product Depth (in.)", "Product Weight (lb.)")
  
  #Subset and change names row to match col names
  attr_data = subset(attr, name %in% spec_names)
  attr_data$name = as.character(attr_data$name)
  attr_data$name[grep("Bullet01", attr_data$name)] = "bullet01"
  attr_data$name[grep("Bullet02", attr_data$name)] = "bullet02"
  attr_data$name[grep("Bullet03", attr_data$name)] = "bullet03"
  attr_data$name[grep("Bullet04", attr_data$name)] = "bullet04"
  attr_data$name[grep("Bullet05", attr_data$name)] = "bullet05"
  attr_data$name[grep("Bullet06", attr_data$name)] = "bullet06"
  attr_data$name[grep("MFG Brand Name", attr_data$name)] = "brand"
  attr_data$name[grep("Product Width", attr_data$name)] = "p_width"
  attr_data$name[grep("Product Height", attr_data$name)] = "p_height"
  attr_data$name[grep("Product Depth", attr_data$name)] = "p_depth"  
  attr_data$name[grep("Product Weight", attr_data$name)] = "p_weight"   
  
  for(i in 1:length(uniques)){
    product = subset(attr_data, attr_data$product_uid == uniques[i])
    for(j in 1:length(product$name)){
      category = as.character(product$name[j])
      if(category %in% names(new_attr)){
        new_attr[i, category] = as.character(product$value[j])
      } 
    }
  }
  
  return(new_attr)
}

top_attr = reshape(attr)

#Merged products with their descriptions
train = merge(train, descriptions, by.x = "product_uid", by.y = "product_uid", all.x = TRUE, all.y = FALSE)
test = merge(test, descriptions, by.x = "product_uid", by.y = "product_uid", all.x = TRUE, all.y = FALSE)

#Merge training and test data with attr
train = merge(train, top_attr, by.x = "product_uid", by.y = "product_uid", all.x = TRUE, all.y = FALSE)
test = merge(test, top_attr, by.x = "product_uid", by.y = "product_uid", all.x = TRUE, all.y = FALSE)

#plans
#take whole words from search query and check name/description for it
#might need to manually change data from predictions to not go over or under cap

#Find matches between search_term and product_title/relevance, based on benchmark score script
word_match = function(words, title, desc, brand, bullet01,bullet02,bullet03,bullet04,bullet05,p_width,p_height, 
                      p_depth,p_weight){
  #Initialize the variables
  n_title = 0
  n_desc = 0
  n_brand = 0
  n_bullet01 = 0
  n_bullet02 = 0
  n_bullet03 = 0
  n_bullet04 = 0
  n_bullet05 = 0
  p_width = 0
  p_height = 0
  p_depth = 0
  p_weight = 0
  
  #Split the words into word array
  words = unlist(strsplit(as.character(words)," "))
  nwords = length(words)
  
  #For loop for each word to check presence in text
  for(i in 1:length(words)){
    pattern = paste("(^| )",words[i],"($| )",sep="")
    n_title = n_title + grepl(pattern,title,perl=TRUE,ignore.case=TRUE)
    n_desc = n_desc + grepl(pattern,desc,perl=TRUE,ignore.case=TRUE)
    n_brand = n_brand + grepl(pattern,desc,perl=TRUE,ignore.case=TRUE)
    n_bullet01 = n_bullet01 + grepl(pattern,desc,perl=TRUE,ignore.case=TRUE)
    n_bullet02 = n_bullet02 + grepl(pattern,desc,perl=TRUE,ignore.case=TRUE)
    n_bullet03 = n_bullet03 + grepl(pattern,desc,perl=TRUE,ignore.case=TRUE)
    n_bullet04 = n_bullet04 + grepl(pattern,desc,perl=TRUE,ignore.case=TRUE)
    n_bullet05 = n_bullet05 + grepl(pattern,desc,perl=TRUE,ignore.case=TRUE)
    p_width = p_width + grepl(pattern,desc,perl=TRUE,ignore.case=TRUE)
    p_height = p_height + grepl(pattern,desc,perl=TRUE,ignore.case=TRUE)
    p_depth = p_depth + grepl(pattern,desc,perl=TRUE,ignore.case=TRUE)
    p_weight = p_weight + grepl(pattern,desc,perl=TRUE,ignore.case=TRUE)
  }
  return(c(n_title,nwords,n_desc,n_brand, n_bullet01, n_bullet02, n_bullet03, n_bullet04, n_bullet05, p_width,
           p_height, p_depth, p_weight))
}

#Process train
train_words = as.data.frame(t(mapply(word_match,train$search_term,train$product_title,train$product_description, 
                                     train$brand,train$bullet01,train$bullet02, train$bullet03, train$bullet04, 
                                     train$bullet05, train$p_width, train$p_height, train$p_depth, train$p_weight)))
train$nmatch_title = train_words[,1]
train$nwords = train_words[,2]
train$nmatch_desc = train_words[,3]
train$nbrand = train_words[,4]
train$nbullets = train_words[,5] + train_words[,6] + train_words[,7] + train_words[,8] + train_words[,9]
train$nwidth = train_words[,10]
train$nheight = train_words[,11]
train$ndepth = train_words[,12]
train$nweight = train_words[,13]


#Process test
test_words = as.data.frame(t(mapply(word_match,test$search_term,test$product_title,test$product_description,
                                    test$brand,test$bullet01,test$bullet02, test$bullet03, test$bullet04, 
                                    test$bullet05, test$p_width, test$p_height, test$p_depth, test$p_weight)))
test$nmatch_title = test_words[,1]
test$nwords = test_words[,2]
test$nmatch_desc = test_words[,3]
test$nbrand = test_words[,4]
test$nbullets = test_words[,5] + train_words[,6] + train_words[,7] + train_words[,8] + train_words[,9]
test$nwidth = test_words[,10]
test$nheight = test_words[,11]
test$ndepth = test_words[,12]
test$nweight = test_words[,13]

rm(train_words,test_words)

#Prediction using glm
glm_model = glm(relevance ~ nmatch_title + nmatch_desc + nwords,data=train)
test_relevance = predict(glm_model,test)
test_relevance = ifelse(test_relevance>3,3,test_relevance)
test_relevance = ifelse(test_relevance<1,1,test_relevance)

#Create submission
submission = data.frame(id=test$id, relevance=test_relevance)
write_csv(submission,"submission.csv")