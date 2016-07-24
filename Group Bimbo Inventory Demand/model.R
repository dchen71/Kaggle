## Kaggle - Group Bimbo Inventory Demand
## model

# Packages
library(readr)
library(dplyr)

# Read input data
test = read_csv("input/test.csv")
train = read_csv("input/train.csv")
town_state = read_csv("input/town_state.csv")

#Merge training/test data with table to extract state
train = left_join(train, town_state, by="Agencia_ID")
test = left_join(test, town_state, by="Agencia_ID")
train$Town = NULL
test$Town = NULL
test$Agencia_ID = NULL
train$Agencia_ID = NULL

#Convert everything to factor
train$Dev_proxima = NULL
train$Dev_uni_proxima = NULL


convert_factor = function(df,variable){
  return(as.factor(df[variable][[1]]))
}

test$Semana = convert_factor(test,"Semana")
test$Canal_ID = convert_factor(test, "Canal_ID")
test$Ruta_SAK = convert_factor(test, "Ruta_SAK")
test$Cliente_ID = convert_factor(test, "Cliente_ID")
test$State = convert_factor(test, "State")
train$Semana = convert_factor(train,"Semana")
train$Canal_ID = convert_factor(train, "Canal_ID")
train$Ruta_SAK = convert_factor(train, "Ruta_SAK")
train$Cliente_ID = convert_factor(train, "Cliente_ID")
train$State = convert_factor(train, "State")

#Create submission file

submission = data.frame(id=test$id, Demanda_uni_equil = 0)

write.csv(submission, "submission.csv", row.names = FALSE)