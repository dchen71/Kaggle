#San Francisco Crime Classification

#Loads libraries
library(ggmap)
library(ggplot2)
library(dplyr)

#Initializes data
train = read.csv("Data/train.csv")
test = read.csv('Data/test.csv')
map = get_map(location="sanfrancisco",zoom=12,source="osm")

#Function to plot graph of locations of crime for visualization purposes
map_crime = function(crime_df, crime) {
    filtered <- filter(crime_df, Category %in% crime)
    plot <- ggmap(map, extent='device') + 
        geom_point(data = filtered, aes(x = X, y = Y, color=Category, alpha=0.5))
    return(plot)
}

#Plots crimes
# map_crime(train, c('PROSTITUTION'))
# map_crime(train, c('SEX OFFENSES FORCIBLE', 'SEX OFFENSES NON FORCIBLE', 'PORNOGRAPHY/OBSECENE MAT'))
# map_crime(train, c('WARRANTS'))
# map_crime(train, c('LARCENY/THEFT', 'VEHICLE THEFT', 'STOLEN PROPERTY', 'ROBBERY'))
# map_crime(train, c('NON-CRIMINAL', 'LOITERING', 'RECOVERED VEHICLE', 'DISORDERLY CONDUCT'))
# map_crime(train, c('DRIVING UNDER THE INFLUENCE', 'FAMILY OFFENSES', 'LIQUOR LAWS', 'RUNAWAY'))

#Trying glm prediction
log_model1 = glm(Category ~ PdDistrict + X + Y, data=train, family=binomial)
pred_test1 = predict(log_model1,newdata=test ,type="classificaion")

#Shaping data for saving
save_data = data.frame(test$Id)
save_data[,2:(length(unique(train$Category)) + 1)] = 0
names(save_data) = c('Id', unique(levels(train$Category)))
save_data[,2] = pred_test1

#Save the data to a csv
write.csv(save_data, file="classification.csv", row.names=FALSE)