#San Francisco Crime Classification

#Loads libraries


#Initializes data
train = read.csv("Data/train.csv")
test = read.csv('Data/test.csv')

categories = data.frame(cat = unique(levels(train$Category)))

#Trying glm prediction
log_model1 = glm(Category ~ PdDistrict + X + Y, data=train, family=binomial)
pred_test1 = predict(log_model1,newdata=test ,type="response")

#Shaping data for saving
save_data = data.frame(Id = test$Id)
save_data[,2:(length(unique(train$Category)) + 1)] = 0
names(save_data) = c('Id', unique(levels(train$Category)))
save_data[,2] = pred_test1

#Save the data to a csv
write.csv(save_data, file="classification.csv", row.names=FALSE)