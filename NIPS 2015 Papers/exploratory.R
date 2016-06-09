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


