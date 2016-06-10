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

#Function to find last name for each row
findLastName = function(fullname){
  return(fullname[length(fullname)])
}

##
## Check last names for authors
##

#Check last names
authorNames = strsplit(authors$Name, " ")

lastNames = lapply(authorNames, findLastName)
lastNames = as.character(lastNames)

#Count number of each last name
lastNamesCount = as.data.frame(table(lastNames))
lastNamesCount = lastNamesCount[order(lastNamesCount$Freq),]

##
## Check last name counts for authors linked to papers
##

#Find who wrote papers
publishers = merge(authors, paperAuthors, by.x = "Id", by.y="AuthorId")

#Check last names
authorNames = strsplit(publishers$Name, " ")
lastNamesPub = lapply(authorNames, findLastName)
lastNamesPub = as.character(lastNamesPub)

#Count number of each last name
lastNamesPubCount = as.data.frame(table(lastNamesPub))
lastNamesPubCount = lastNamesPubCount[order(lastNamesPubCount$Freq),]
