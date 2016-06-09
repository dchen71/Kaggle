## Kaggle - NIPS 2015 Papers

# Packages
library(readr)
library(dplyr)

# Read input data
authors = read_csv("input/Authors.csv")
paperAuthors = read_csv("input/PaperAuthors.csv")
papers = read_csv("input/Papers.csv")

#Look at type of events at NIPS
table(papers$EventType)

#Check last names
authorNames = strsplit(authors$Name, " ")

#Function to find last name for each row
findLastName = function(fullname){
  return(fullname[length(fullname)])
}
lastNames = lapply(authorNames, findLastName)
lastNames = as.character(lastNames)

#Count number of each last name
lastNamesCount = as.data.frame(table(lastNames))
lastNamesCount = lastNamesCount[order(lastNamesCount$Freq),]