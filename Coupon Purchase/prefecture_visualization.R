#Coupon Purchase Prediction Prefecture Visualization

#Loads libraries
library(ggmap)
library(ggplot2)


#Initializes data
locations = read.csv("input/prefecture_locations.csv")
names(locations) = c('pref','pref_ofice', 'lat','long')
map = get_map(location=c(139,35), zoom=5)

#Plots the map
ggmap(map, extent='device') + geom_point(data=locations, aes(x=long, y=lat, color=pref, alpha=0.8), size=4) +
    labs(list(title="Locations of Prefectures in Japan", color="Prefectures"))  + 
    scale_size_area()