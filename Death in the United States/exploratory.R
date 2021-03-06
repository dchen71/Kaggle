## Kaggle - Death in the United States
## Exploratory

# Packages
library(readr)
library(dplyr)

# Read input data
deathRecords = read_csv("input/DeathRecords.csv")
mannerOfDeath = read_csv("input/MannerOfDeath.csv")
education2003 = read_csv("input/Education2003Revision.csv")

filteredRecords = deathRecords[,c('Id', "MannerOfDeath", "Education2003Revision")]

# Merge together manner of death and educational info
filteredRecords = merge(filteredRecords, mannerOfDeath, by.x="MannerOfDeath", by.y="Code")
filteredRecords$MannerOfDeath = filteredRecords$Description
filteredRecords$Description = NULL
filteredRecords = merge(filteredRecords, education2003, by.x="Education2003Revision", by.y="Code")
filteredRecords$Education2003Revision = filteredRecords$Description
filteredRecords$Description = NULL

#Group by education and manner of death
grouped_data = aggregate(filteredRecords, 
                         by=list(filteredRecords$Education2003Revision, filteredRecords$MannerOfDeath), 
                         FUN=length)

grouped_data$Id = NULL
grouped_data$MannerOfDeath = NULL
names(grouped_data) = c("Education", "MannerOfDeath", "Count")
grouped_data$Education = as.factor(grouped_data$Education)
grouped_data$MannerOfDeath = as.factor(grouped_data$MannerOfDeath)

#Want to normalize to group to find percentage of count/total # in group
total8th = sum(grouped_data$Count[grouped_data$Education == "8th grade or less"])
total9th = sum(grouped_data$Count[grouped_data$Education == "9 - 12th grade, no diploma"])
totalAssoc = sum(grouped_data$Count[grouped_data$Education == "Associate degree"])
totalBac = sum(grouped_data$Count[grouped_data$Education == "Bachelor's degree"])
totalDoc = sum(grouped_data$Count[grouped_data$Education == "Doctorate or professional degree"])
totalHigh = sum(grouped_data$Count[grouped_data$Education == "high school graduate or GED completed"])
totalMast = sum(grouped_data$Count[grouped_data$Education == "Master's degree"])
totalColl = sum(grouped_data$Count[grouped_data$Education == "some college credit, but no degree"])
totalUnknown = sum(grouped_data$Count[grouped_data$Education == "Unknown"])

#Normalize data by dividing each value based on education group total numbers
grouped_data$PercentGroup = 0

for(i in 1:nrow(grouped_data)){
  if(grouped_data$Education[i] == "8th grade or less"){
    grouped_data$PercentGroup[i] = grouped_data$Count[i] / total8th
  } else if(grouped_data$Education[i] == "9 - 12th grade, no diploma"){
    grouped_data$PercentGroup[i] = grouped_data$Count[i] / total9th
  } else if(grouped_data$Education[i] == "Associate degree"){
    grouped_data$PercentGroup[i] = grouped_data$Count[i] / totalAssoc
  } else if(grouped_data$Education[i] == "Bachelor's degree"){
    grouped_data$PercentGroup[i] = grouped_data$Count[i] / totalBac
  } else if(grouped_data$Education[i] == "Doctorate or professional degree"){
    grouped_data$PercentGroup[i] = grouped_data$Count[i] / totalDoc
  } else if(grouped_data$Education[i] == "high school graduate or GED completed"){
    grouped_data$PercentGroup[i] = grouped_data$Count[i] / totalHigh
  } else if(grouped_data$Education[i] == "Master's degree"){
    grouped_data$PercentGroup[i] = grouped_data$Count[i] / totalMast
  } else if(grouped_data$Education[i] == "some college credit, but no degree"){
    grouped_data$PercentGroup[i] = grouped_data$Count[i] / totalColl
  } else if(grouped_data$Education[i] == "Unknown"){
    grouped_data$PercentGroup[i] = grouped_data$Count[i] / totalUnknown
  }
}
grouped_data$PercentGroup = grouped_data$PercentGroup * 100
