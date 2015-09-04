#San Francisco Crime Classification

#Loads libraries
library(ggmap)
library(ggplot2)
library(dplyr)

#Initializes data
train = read.csv("Data/train.csv")
map = get_map(location="sanfrancisco",zoom=12,source="osm")

#Function to plot graph of locations of crime for visualization purposes
map_crime = function(crime_df, crime) {
    filtered = filter(crime_df, Category %in% crime)
    plot = ggmap(map, extent='device') + 
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