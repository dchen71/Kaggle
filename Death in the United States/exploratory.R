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
