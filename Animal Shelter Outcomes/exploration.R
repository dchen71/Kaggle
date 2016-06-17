## Kaggle - Shelter Animal Outcomes
## Data exploration

# Read input data
train = read.csv("input/train.csv")
test = read.csv("input/test.csv")

head(train)
head(test)

#Train missing OutcomeType/OutcomeSubtype
#Want to predict OutcomeType

#Need to merge the test and train dataset in order to share factors
#Convert AgeuponOutcome into float
#Consider using DateTime(maybe just date)
#Consider mix column
#Consider using the / for breeds