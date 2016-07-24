## Kaggle - TalkingData Mobile User Demographics
## Basic xgboost model
### Can potentially implement around app events to find user apps and category for further categorization

## Initialization

# Packages
#library(dplyr)
library(xgboost)
library(Matrix)

# Read input data
#app_events = read.csv("input/app_events.csv", colClasses = "character", stringsAsFactors = TRUE)
#app_labels = read.csv("input/app_labels.csv", colClasses = "character", stringsAsFactors = TRUE)
#events = read.csv("input/events.csv", colClasses = "character", stringsAsFactors = TRUE)
#label_cat = read.csv("input/label_categories.csv", colClasses = "character", stringsAsFactors = TRUE)
phone_brand_model = read.csv("input/phone_brand_device_model.csv", stringsAsFactors = TRUE)
test = read.csv("input/gender_age_test.csv", stringsAsFactors = TRUE)
train = read.csv("input/gender_age_train.csv", stringsAsFactors = TRUE)


## Cleaning and merging

#Rename the dashes and pluses as . for submission
train$group = gsub("[^a-zA-Z0-9_]",".", train$group)

#Remove columns that won't be used(memory considerations)
##Can potentially use this column to find lenght of time a app is installed/active/etc
#events$timestamp = NULL
#events$longitude = NULL
#events$latitude = NULL

#Merge app labels with cateogry
##Consider cleaning up data/merging ex. games, fiance, simple/simple 1
#app_labels = left_join(app_labels, label_cat, by = "label_id")
#rm(label_cat)

#Merge app labels with app events
#app_data = left_join(app_labels, app_events, by = "app_id")

#table together category installed/active per user


#Merge data with app_events/events
#events = left_join(events, app_events, by = "event_id")
#events = events[order(events$event_id),]
#rm(app_events)

#Merge data with the phone brand and model
train = merge(train,phone_brand_model, by=intersect(names(train), names(phone_brand_model)), all.x=TRUE)
test = merge(test,phone_brand_model, by=intersect(names(test), names(phone_brand_model)), all.x=TRUE)
rm(phone_brand_model)



#Merge app labels with events
#events = merge(events, app_labels,by = "app_id", all.x = T)

#Merge everythig with train and test
#events = merge(events, app_labels, by= "app_id", all.x = T)
#train = merge(train, events, by = "device_id", all.x = T)
#test = merge(test, events, by = "device_id", all.x = T)


## Modeling
#Create model using xgboost
train_labels = as.numeric(train$group) - 1
xgbMat = xgb.DMatrix(Matrix(data.matrix(train[,!colnames(train) %in% c("group")])), 
                     label=train_labels)

#Create submission
submission = data.frame(test$device_id)

write.csv(submission, "submission.csv", row.names = FALSE)