## Kaggle - TalkingData Mobile User Demographics
## model

# Packages
library(readr)
library(dplyr)

# Read input data
app_events = read_csv("input/app_events.csv")
app_labels = read_csv("input/app_labels.csv")
events = read_csv("input/events.csv")
labels = read_csv("input/label_categories.csv")
phone_brand_model = read_csv("input/phone_brand_device_model.csv")
test = read_csv("input/gender_age_test.csv")
train = read_csv("input/gender_age_train.csv")

#Merge data with the phone brand and model
merge(train,phone_brand_model)
merge(test,phone_brand_model, by=intersect(names(test), names(phone_brand_model)), all.x=FALSE)

#Create submission
submission = data.frame(test$device_id)

write.csv(submission, "submission.csv", row.names = FALSE)