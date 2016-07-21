## Kaggle - TalkingData Mobile User Demographics
## model

# Packages
library(dplyr)

# Read input data
app_events = read.csv("input/app_events.csv", colClasses = "character", stringsAsFactors = TRUE)
app_labels = read.csv("input/app_labels.csv", colClasses = "character", stringsAsFactors = TRUE)
events = read.csv("input/events.csv", colClasses = "character", stringsAsFactors = TRUE)
labels = read.csv("input/label_categories.csv", colClasses = "character", stringsAsFactors = TRUE)
phone_brand_model = read.csv("input/phone_brand_device_model.csv", colClasses = "character", stringsAsFactors = TRUE)
test = read.csv("input/gender_age_test.csv", colClasses = "character", stringsAsFactors = TRUE)
train = read.csv("input/gender_age_train.csv", colClasses = "character", stringsAsFactors = TRUE)

#Rename the dashes and pluses as . for submission
train$group = gsub("[^a-zA-Z0-9_]",".", train$group)

#Remove columsn that won't be used(memory considerations)
events$timestamp = NULL
events$longitude = NULL
events$latitude = NULL

#Merge data with app_events/events
events = merge(events, app_events, by = "event_id", all.x = T)
rm(app_events)

#Merge data with the phone brand and model
train = merge(train,phone_brand_model, by=intersect(names(train), names(phone_brand_model)), all.x=TRUE)
test = merge(test,phone_brand_model, by=intersect(names(test), names(phone_brand_model)), all.x=TRUE)
rm(phone_brand_model)

#Merge app labels with cateogry
app_labels = merge(app_labels, labels, by = "label_id", all.x = T)
rm(labels)

#Merge everythig with train and test
events = merge(events, app_labels, by= "app_id", all.x = T)
train = merge(train, events, by = "device_id", all.x = T)
test = merge(test, events, by = "device_id", all.x = T)

#Create submission
submission = data.frame(test$device_id)

write.csv(submission, "submission.csv", row.names = FALSE)