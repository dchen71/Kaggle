## Kaggle - Group Bimbo Inventory Demand
## model

# Packages
library(readr)
library(dplyr)

# Read input data
test = read_csv("input/test.csv")
train = read_csv("input/train.csv")
clients = read_csv("input/cliente_tabla.csv")
products = read_csv("input/producto_tabla.csv")
town_state = read_csv("input/town_state.csv")

#Merge training/test data with table to extract state
train = left_join(train, town_state, by="Agencia_ID")
test = left_join(test, town_state, by="Agencia_ID")
train$Town = NULL
test$Town = NULL

#Convert everything to symbol

#Create submission file

submission = data.frame(id=test$id, Demanda_uni_equil = 0)

write.csv(submission, "submission.csv", row.names = FALSE)